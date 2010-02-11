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
   #:test))

(in-package :common-db-gdbserver)

(set-namespace :core)

(defvar *gdbserver-help-en*
  "  GDB server options:
    --port <port-number>        Accept remote GDB connections on this port.")

(defvar *poll-interval* (/ 20)
  "How often to query target for its state.")

;;;;
;;;; Classes
;;;;
(defclass common-db-target (target-context gdbremote::target)
  ())

(defclass common-db-gdbserver (common-db-target gdb-extended-server)
  ())

(defvar *gdb-register-instance-vector*)
(defvar *gdb-id-to-register-instance-map*)

(define-root-container *gdb-id-to-register-instance-map* gdb-reginstance :type register-instance :if-exists :continue)

;;;;
;;;; GDB-SERVER methods
;;;;
(defmethod gdb-describe-target ((o common-db-gdbserver))
  (gdb:describe-target (ctx-target o)
                       (lambda (ri register-nr)
                         (setf (gdb-reginstance register-nr) ri))))

(defmethod gdb-describe-target-memory-map ((o common-db-gdbserver))
  (gdb:describe-memory-map (ctx-core o)))

(defmethod gdb-describe-target-spu ((o common-db-gdbserver))
  (gdb:describe-spu (ctx-target o)))

(defmethod gdb-target-registers-as-vector ((o common-db-gdbserver))
  (lret ((regvec (make-array (* 4 (length *gdb-register-instance-vector*)) :element-type '(unsigned-byte 8))))
    (iter (for ri in-vector *gdb-register-instance-vector*)
          (for offt from 0 by 4)
          (setf (u8-vector-word32le regvec offt) (reginstance-value ri)))))

(defmethod gdb-set-target-registers-from-vector ((o common-db-gdbserver) vector)
  (lret ((regvec (make-array (* 4 (length *gdb-register-instance-vector*)) :element-type '(unsigned-byte 8))))
    (iter (for ri in-vector *gdb-register-instance-vector*)
          (for offt from 0 by 4)
          (setf (reginstance-value ri) (u8-vector-word32le regvec offt)))))

(defmethod gdb-read-target-register ((o common-db-gdbserver) register-nr)
  (gdb-read-target-register (ctx-core o) register-nr))

(defmethod gdb-write-target-register ((o common-db-gdbserver) register-nr value)
  (gdb-write-target-register (ctx-core o) register-nr value))

(defmethod gdb-read-target-register ((o little-endian-core) register-nr)
  (swap-word32 (reginstance-value (gdb-reginstance register-nr))))

(defmethod gdb-read-target-register ((o big-endian-core) register-nr)
  (reginstance-value (gdb-reginstance register-nr)))

(defmethod gdb-write-target-register ((o little-endian-core) register-nr value)
  (setf (reginstance-value (gdb-reginstance register-nr)) (swap-word32 value)))

(defmethod gdb-write-target-register ((o big-endian-core) register-nr value)
  (setf (reginstance-value (gdb-reginstance register-nr)) value))

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
(defun encode-gdb-stop-reason (trapped &key core-id watchpoint-address)
  (format nil "T~2,'0X~:[~;core:~:*~D~]~:[~;watch:~:*~X~]"
          (if trapped +reason-trap+ +reason-interrupt+)
          core-id
          watchpoint-address))

(defmethod running? ((o common-db-gdbserver))
  (core-running-p (ctx-core o)))

(defmethod gdb-why-stop ((o common-db-gdbserver))
  (encode-gdb-stop-reason (typep (core-stop-reason (ctx-core o)) 'trap)))

(defmethod gdb-interrupt ((o common-db-gdbserver) &aux
                          (core (ctx-core o)))
  (format *trace-output* "Interrupt!~%")
  (when (core-running-p core)
    (setf (state core) :stop))
  (setf (state core) :debug)
  (values))

(defmethod gdb-continue-at ((o common-db-gdbserver) addr &aux
                            (core (ctx-core o)))
  (run-core-asynchronous core addr)
  (loop
     ;; Poll for condition of remote target every now and then. Not
     ;; very pretty...
     (unless (zerop *poll-interval*)
       (sleep *poll-interval*))
     ;; Check to see if GDB wants a break or if the target has hit a
     ;; breakpoint.
     (let ((int? (check-interrupt o))
           (db? (not (core-running-p core))))
       (when (or int? db?)
         (setf (state core) :debug)
         ;; If we are interrupted or inside the debugger return to
         ;; GDB.
         (return-from gdb-continue-at
           (encode-gdb-stop-reason db?)))))  
  "S00")

(defmethod gdb-single-step-at ((o common-db-gdbserver) addr &aux
                               (core (ctx-core o)))
  (set-core-insn-execution-limit core 1)
  (let ((*poll-interval* 0))
    (gdb-continue-at o addr)))

(defmethod gdb-extended-query ((o common-db-gdbserver) (c (eql :cont)) arguments)
  "vCont;cs")

(defmethod gdb-extended-command ((o common-db-gdbserver) (c (eql :cont)) arguments)
  "Use the first action and completely ignore all thread IDs."
  (if (plusp (length arguments))
      (case (char arguments 0)
        (#\c (gdb-continue-at o nil))
        (#\s (gdb-single-step-at o nil))
        (t ""))
      ""))

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
  (when-let ((b (trap core address)))
    (disable-trap b))
  "OK")

;;;;
;;;; Attach/detach
;;;;
(defmethod gdb-detach ((o common-db-gdbserver) &aux
                       (core (ctx-core o)))
  (dolist (core (cons core (master-device-slaves core)))
    (do-core-traps (b core)
      (disable-breakpoint b)))
  ;; Call next method to really terminate the connection.
  (call-next-method))

(defmethod gdb-kill ((o common-db-gdbserver) &aux
                     (core (ctx-core o)))
  (format *trace-output* "Killing is not supported. Hit the reset button manually.~%")
  (dolist (core (cons core (master-device-slaves core)))
    (do-core-traps (b core)
      (disable-breakpoint b)))
  ;; Call next method to really terminate the connection.
  (call-next-method))

;;;;
;;;; Stubs
;;;;
(defmethod gdb-set-thread ((o common-db-gdbserver) domain thread)
  (if (or (= thread -1)
          (= thread 0))
      "OK"
      "E00"))

(defmethod gdb-monitor ((o common-db-gdbserver) command rest-arg)
  (declare (ignore args))
  "Command not supported.")

(defmethod gdb-monitor ((o common-db-gdbserver) (command (eql :eval)) rest-arg)
  (princ-to-string (eval (let ((*package* (find-package :common-db)))
                           (read-from-string rest-arg)))))

;;;;
;;;; Test
;;;;
(defun test (&optional (target-context *current*) (address "127.0.0.1") (port 9000) &aux
             (core (ctx-core target-context)))
  (change-class target-context 'common-db-gdbserver)
  (let ((ri-names (gdb:core-register-order core)))
    (setf *gdb-register-instance-vector*
          (make-array (length ri-names)
                      :initial-contents (mapcar (curry #'device-register-instance core) ri-names))
          *gdb-id-to-register-instance-map*
          (make-hash-table)
          ;; XXX: wtf?
          (slot-value target-context 'gdbremote::no-ack-mode) nil))
  (accept-gdb-connection target-context port address))
