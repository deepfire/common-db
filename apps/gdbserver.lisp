;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10 -*-
;;;
;;;  (c) copyright 2010 by
;;;           Elvees (http://www.elvees.ru)
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.


(defpackage :common-db-gdbserver
  (:nicknames :comgdbsrv)
  (:use :common-lisp :alexandria :iterate :pergamum :custom-harness :setc :bitmop :device-model :isa :isa-mips :assem :assem-mips :blitz.debug.gdb-remote :binary-types
        :portability :options :spaces :bus :address-map :loadable :generic :interface :platform :target :core :sysdev
        :mips :dsp :flash
        :common-db)
  (:shadowing-import-from :isa #:disassemble)
  (:shadowing-import-from :bitmop #:space #:*space*)
  (:shadowing-import-from :common-db #:catch #:step #:get #:set #:trace)
  (:export
   #:test
   ))

(in-package :common-db-gdbserver)

(set-namespace :core)

(defvar *gdbserver-help-en*
  "  GDB server options:
    --port <port-number>        Accept remote GDB connections on this port.")

;;;;
;;;; Classes
;;;;
(defclass common-db-target (target-context gdbremote::target)
  ())

(defclass common-db-gdbserver (common-db-target gdb-server)
  ())

;;;;
;;;; GDB-SERVER methods
;;;;
(defmethod gdb-describe-target ((o common-db-gdbserver))
  (gdb:describe-target (ctx-target o)))

(defvar *gdb-register-instance-vector*)

(defmethod gdb-target-registers-as-vector ((o common-db-gdbserver))
  (lret ((regvec (make-array (* 4 (length *gdb-register-instance-vector*)))))
    (iter (for ri in-vector *gdb-register-instance-vector*)
          (for offt from 0 by 4)
          (setf (u8-vector-word32le regvec offt) (reginstance-value ri)))))

(defmethod gdb-set-target-registers-from-vector ((o common-db-gdbserver) vector)
  (lret ((regvec (make-array (* 4 (length *gdb-register-instance-vector*)))))
    (iter (for ri in-vector *gdb-register-instance-vector*)
          (for offt from 0 by 4)
          (setf (reginstance-value ri) (u8-vector-word32le regvec offt)))))

(defmethod gdb-read-memory ((o common-db-gdbserver) addr size &aux
                            (c (ctx-core o)))
  (handler-case (lret ((iovec (make-array size :element-type '(unsigned-byte 8)))) 
                  (read-block c addr iovec))
    (error (c)
      (format *trace-output* "Error in GDB-READ-MEMORY: ~A~%" c)
      "E00")))

(defmethod gdb-write-memory ((o common-db-gdbserver) addr data &aux
                             (c (ctx-core o)))
  #+ ignore (format *trace-output* "Writing ~A@~X~%" data addr)
  (handler-case (progn
                  (write-block c addr data)
                  "OK")
    (error (c)
      (format *trace-output* "Error in GDB-WRITE-MEMORY: ~A~%" c)
      "E00")))

;;;;
;;;; Execution control
;;;;
(defmethod running? ((o common-db-gdbserver))
  (lret ((result (core-running-p (ctx-core o))))
    #+nil (format *trace-output* "RUNNING? ~S~%" result)))

(defmethod gdb-why-stop ((o common-db-gdbserver))
  (format *trace-output* "GDB-WHY-STOP~%")
  (let ((code (typecase (core-stop-reason (ctx-core o))
                (trap gdbremote:+reason-trap+)
                (t gdbremote:+reason-interrupt+))))
    (format nil "T~2,'0X" code)))

(defmethod gdb-interrupt ((o common-db-gdbserver) &aux
                          (core (ctx-core o)))
  (format *trace-output* "Interrupt!~%")
  (when (core-running-p core)
    (setf (state core) :stop))
  (setf (state core) :debug)
  (values))

(defmethod gdb-continue-at ((o common-db-gdbserver) addr &aux
                            (core (ctx-core o)))
  (format *trace-output* "CONTINUE-AT ~X~%" addr)
  (run-core-asynchronous core addr)
  (resume server)
  (loop
     ;; Poll for condition of remote target every now and then. Not
     ;; very pretty...
     (unless (zerop *poll-interval*)
       (sleep *poll-interval*))
     ;; Check to see if GDB wants a break or if the target has hit a
     ;; breakpoint.
     (let ((int? (check-interrupt server))
           (db? (inside-debugger? c)))
       (when (or int? db?)
         (stop server)
         ;; If we are interrupted or inside the debugger return to
         ;; GDB.
         (when db?
           ;; A debug condition (via debug extensions or INT3)
           ;; happened. If it was a INT3, decrement EIP.
           (let ((eip (tss-eip (tss-of c))))
             (when (find (1- eip) (soft-breakpoints-of server) :key #'address-of)
               (decf (tss-eip (tss-of c))))))
         (when (or int? db?)
           (return-from gdb-continue-at
             (return-code
              (if db?
                  +reason-trap+
                  +reason-interrupt+)
              (tss-eip (tss-of c)))))
         ;; Otherwise continue the target.
         (resume c)
         )))  
  "S00")

(defmethod gdb-single-step-at ((o common-db-gdbserver) addr &aux
                               (core (ctx-core o)))
  (set-core-insn-execution-limit core 1)
  (gdb-continue-at o addr))

;;;;
;;;; Breakpoints
;;;;
(defmethod gdb-insert-breakpoint ((o common-db-gdbserver) type address length &aux
                                  (core (ctx-core o)))
  (declare (ignore length))
  (cond
    ((eq type :software)
     (add-sw-breakpoint core address) ; We ignore LENGTH for software breakpoints.
     "OK")
    ((not (= 4 length)) "EInvalidLength")
    (t
     (if-let ((b (add-hw-breakpoint core address)))
       (prog1 "OK"
         (format *trace-output* "~&New breakpoint in slot ~A, type ~A, address ~8,'0X.~%" (trap-id b) type address))
       "ENoDebugRegistersLeft"))))

(defmethod gdb-remove-breakpoint ((o common-db-gdbserver) type address length &aux
                                  (core (ctx-core o)))
  (declare (ignore type length))
  (when-let ((b (core-trap core address)))
    (disable-trap b))
  "OK")

;;;;
;;;; Attach/detach
;;;;
(defmethod gdb-detach ((o common-db-gdbserver) &aux
                       (core (ctx-core o)))
  (dolist (core (cons core (core-slaves core)))
    (do-core-traps (b core)
      (disable-breakpoint b)))
  ;; Call next method to really terminate the connection.
  (call-next-method))

(defmethod gdb-kill ((o common-db-gdbserver) &aux
                     (core (ctx-core o)))
  (format *trace-output* "Killing is not supported. Hit the reset button manually.~%")
  (dolist (core (cons core (core-slaves core)))
    (do-core-traps (b core)
      (disable-breakpoint b)))
  ;; Call next method to really terminate the connection.
  (call-next-method))

;;;;
;;;; Stubs
;;;;
(defmethod gdb-set-thread ((o common-db-gdbserver) domain thread)
  (format *trace-output* "GDB-SET-THREAD ~S ~S~%" domain thread)
  (if (or (= thread -1)
          (= thread 0))
      "OK"
      "E00"))

;;;;
;;;; Test
;;;;
(defun test (&optional (target-context *current*) (port 9000))
  (change-class target-context 'common-db-gdbserver)
  ;; XXX: wtf?
  (setf (slot-value target-context 'gdbremote::no-ack-mode) nil)
  (accept-gdb-connection target-context port))
