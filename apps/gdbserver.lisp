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
   #:*poll-interval*
   #:*trace-state-actions*
   #:gdbserver
   #:gdbserver-toplevel))

(in-package :common-db-gdbserver)

(set-namespace :core)

(defvar *poll-interval* (/ 20)
  "How often to query target for its state.")

(defvar *trace-state-actions* nil
  "Whether to log state and breakpoint -related actions.")

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
;;;; This is definitive: explain our capabilities
(defmethod gdb-handle-query ((o common-db-gdbserver) (q (eql :supported)) args)
  "QStartNoAckMode+;PacketSize=4000;qXfer:features:read+;qXfer:memory-map:read+;qXfer:spu:read+")

;;;;
;;;; Explain out our guts to GDB
(defmethod gdb-describe-target ((o common-db-gdbserver))
  (gdb:describe-target (ctx-target o)
                       (lambda (ri register-nr)
                         (setf (gdb-reginstance register-nr) ri))))

(defmethod gdb-describe-target-memory-map ((o common-db-gdbserver))
  (gdb:describe-memory-map (ctx-core o)))

(defmethod gdb-describe-target-spu ((o common-db-gdbserver))
  (gdb:describe-spu (ctx-target o)))

;;;;
;;;; Register set I/O
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

;;;;
;;;; Memory I/O
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
  (when *trace-state-actions*
    (format *trace-output* "~&Entering debug mode..~%"))
  (when (core-running-p core)
    (setf (state core) :stop))
  (setf (state core) :debug)
  (values))

(defmethod gdb-continue-at ((o common-db-gdbserver) addr &aux
                            (core (ctx-core o)))
  (run-core-asynchronous core (unless (or (not addr)
                                          (= addr (moment-fetch (saved-core-moment core))))
                                addr))
  (when *trace-state-actions*
    (format t "~&Continuing from ~8,'0X~%" addr))
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
  (when *trace-state-actions*
    (format t "~&Single-stepping from ~8,'0X~%" addr))
  (set-core-insn-execution-limit core 1)
  (let ((*poll-interval* 0))
    (gdb-continue-at o addr)))

(defmethod gdb-extended-command ((o common-db-gdbserver) (c (eql :cont)) arguments)
  "Use the first action and completely ignore all thread IDs."
  (if (plusp (length arguments))
      (case (char arguments 0)
        (#\c (gdb-continue-at o nil))
        (#\s (gdb-single-step-at o nil))
        (t ""))
      ""))

(defmethod gdb-extended-command ((o common-db-gdbserver) (c (eql :kill)) arguments)
  (gdb-kill o))

;;;;
;;;; Breakpoints
(defmethod gdb-insert-breakpoint ((o common-db-gdbserver) type address length &aux
                                  (core (ctx-core o)))
  (cond
    ((eq type :software)
     (when *trace-state-actions*
       (format t "~&Adding a software breakpoint at ~8,'0X~%" address))
     (add-sw-breakpoint core address) ; We ignore LENGTH for software breakpoints.
     "OK")
    ((not (= 4 length)) "EInvalidLength")
    (t
     (when *trace-state-actions*
       (format t "~&Attempting to add a hardware breakpoint at ~8,'0X~%" address))
     (if-let ((b (add-hw-breakpoint core address)))
       (prog1 "OK"
         (when *trace-state-actions*
           (format *trace-output* "~&New hardware breakpoint in slot ~A, type ~A, address ~8,'0X.~%" (trap-id b) type address)))
       "ENoDebugRegistersLeft"))))

(defmethod gdb-remove-breakpoint ((o common-db-gdbserver) type address length &aux
                                  (core (ctx-core o)))
  (declare (ignore type length))
  (when *trace-state-actions*
    (format *trace-output* "~&Attempting to remove a breakpoint at ~8,'0X~%" address))
  (when-let ((b (trap core address)))
    (disable-trap b))
  "OK")

;;;;
;;;; Termination
(defmethod gdb-detach ((o common-db-gdbserver) &aux
                       (core (ctx-core o)))
  (dolist (core (cons core (master-device-slaves core)))
    (do-core-traps (b core)
      (disable-breakpoint b)))
  ;; Call next method to really terminate the connection.
  (call-next-method))

(defmethod gdb-kill ((o common-db-gdbserver) &aux
                     (core (ctx-core o)))
  (reset :core core)
  "OK")

;;;;
;;;; Security hole
(defmethod gdb-monitor ((o common-db-gdbserver) (command (eql :eval)) rest-arg)
  (princ-to-string (handler-case (eval (let ((*package* (find-package :common-db)))
                                         (read-from-string rest-arg)))
                     (serious-condition (c)
                       (format nil "~A" c)))))

;;;;
;;;; Stubs
(defmethod gdb-set-thread ((o common-db-gdbserver) domain thread)
  (if (or (= thread -1)
          (= thread 0))
      "OK"
      "E00"))

(defmethod gdb-handle-query ((o common-db-gdbserver) (q (eql :attached)) args)
  "0")

;;;;
;;;; Server
(defvar *gdbserver-help-en*
  "  GDB-server options:
    --address <dotted-quad>     Address of the interface to accept connections.
                                  Defaults to 127.0.0.1.
    --port <integer>            Number of the TCP port to accept connections on.
                                  Defaults to 9000.
    --single-shot               Exit after the first connection terminates.")

(defun gdbserver-toplevel ()
  (setf common-db::*additional-help-en* *gdbserver-help-en*
        common-db::*additional-help-ru* common-db::*gdbserver-help-ru*)
  (comdb::comdb-toplevel-wrapper #'gdbserver
                                 '((:address :string) :port)
                                 '(:single-shot)
                                 :no-memory-detection t))

(defun gdbserver (&key (target-context *current*) (address "127.0.0.1") (port 9000) single-shot (trace-state-actions t) trace-exchange &aux
                  (core (if target-context
                            (ctx-core target-context)
                            (error "~@<No active target context: cannot start GDB server.~:@>"))))
  (change-class target-context 'common-db-gdbserver)
  (let ((ri-names (gdb:core-register-order core))
        (*trace-state-actions* trace-state-actions))
    (setf *gdb-register-instance-vector*
          (make-array (length ri-names)
                      :initial-contents (mapcar (curry #'device-register-instance core) ri-names))
          *gdb-id-to-register-instance-map*
          (make-hash-table))
    (iter (format t "; Accepting connections on ~A:~D~:[~;, tracing exchanges, up to ~:*~X bytes~]~%" address port trace-exchange)
          (setf (slot-value target-context 'gdbremote::no-ack-mode) nil)
          (accept-gdb-connection target-context port address trace-exchange)
          (until single-shot))))
