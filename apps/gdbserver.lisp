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
        :portability :options :spaces :bus :address-map :loadable :generic :interface :platform :tgt :core :sysdev
        :mips :dsp :flash
        :common-db)
  (:shadowing-import-from :isa #:disassemble)
  (:shadowing-import-from :bitmop #:space #:*space*)
  (:shadowing-import-from :common-db #:catch #:step #:get #:set #:trace)
  (:export
   #:*poll-interval*
   #:*trace-comdb-calls*
   #:*trace-even-noisy-comdb-calls*
   #:*trace-comdb-memory-io*
   #:gdbserver
   #:gdbserver-toplevel))

(in-package :common-db-gdbserver)

(set-namespace :core)

(defvar *poll-interval* (/ 20)
  "How often to query target for its state.")

(defvar *trace-comdb-calls* nil
  "Whether to trace COMMON-DB calls, except noisy ones.")

(defvar *trace-even-noisy-comdb-calls* nil
  "Whether to trace noisy COMMON-DB calls as well.")

(defvar *trace-comdb-memory-io* nil
  "Whether to trace target memory I/O COMMON-DB calls.")

(defun log-comdb (call control &rest args)
  (apply #'format *trace-output* (concatenate 'string "~&COMDB ~A: " control "~%")
         call args))

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
  (declare (ignore args))
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
  (when *trace-comdb-calls*
    (log-comdb 'reginstance-value "~A" (map 'list #'name *gdb-register-instance-vector*)))
  (lret ((regvec (make-array (* 4 (length *gdb-register-instance-vector*)) :element-type '(unsigned-byte 8))))
    (iter (for ri in-vector *gdb-register-instance-vector*)
          (for offt from 0 by 4)
          (setf (u8-vector-word32le regvec offt) (reginstance-value ri)))))

(defmethod gdb-set-target-registers-from-vector ((o common-db-gdbserver) vector)
  (declare (ignore vector))
  (when *trace-comdb-calls*
    (log-comdb '(setf reginstance-value) "~A" (map 'list #'name *gdb-register-instance-vector*)))
  (lret ((regvec (make-array (* 4 (length *gdb-register-instance-vector*)) :element-type '(unsigned-byte 8))))
    (iter (for ri in-vector *gdb-register-instance-vector*)
          (for offt from 0 by 4)
          (setf (reginstance-value ri) (u8-vector-word32le regvec offt)))))

;;; reroute these calls to re-dispatch on core endianness
(defmethod gdb-read-target-register ((o common-db-gdbserver) register-nr)
  (gdb-read-target-register (ctx-core o) register-nr))
(defmethod gdb-write-target-register ((o common-db-gdbserver) register-nr value)
  (gdb-write-target-register (ctx-core o) register-nr value))

(defmethod gdb-read-target-register ((o little-endian-core) register-nr &aux
                                     (ri (gdb-reginstance register-nr)))
  (when *trace-comdb-calls*
    (log-comdb 'reginstance-value "~A" (name ri)))
  (swap-word32 (reginstance-value ri)))

(defmethod gdb-read-target-register ((o big-endian-core) register-nr &aux
                                     (ri (gdb-reginstance register-nr)))
  (when *trace-comdb-calls*
    (log-comdb 'reginstance-value "~A" (name ri)))
  (reginstance-value ri))

(defmethod gdb-write-target-register ((o little-endian-core) register-nr value &aux
                                      (ri (gdb-reginstance register-nr))
                                      (value (swap-word32 value)))
  (when *trace-comdb-calls*
    (log-comdb '(setf reginstance-value) "~A ~X" (name ri) value))
  (setf (reginstance-value ri) value))

(defmethod gdb-write-target-register ((o big-endian-core) register-nr value &aux
                                      (ri (gdb-reginstance register-nr)))
  (when *trace-comdb-calls*
    (log-comdb '(setf reginstance-value) "~A ~X" (name ri) value))
  (setf (reginstance-value ri) value))

;;;;
;;;; Memory I/O
(defmethod gdb-read-memory ((o common-db-gdbserver) addr size &aux
                            (c (ctx-core o)))
  (handler-case (lret ((iovec (make-array size :element-type '(unsigned-byte 8)))) 
                  (when *trace-comdb-calls*
                    (log-comdb 'read-block "~X, 0x~X bytes" addr size))
                  (read-block c addr iovec)
                  (when *trace-comdb-memory-io*
                    (print-u8-sequence *trace-output* iovec :address addr)))
    (error (c)
      (format *trace-output* "Error in GDB-READ-MEMORY: ~A~%" c)
      "E00")))

(defmethod gdb-write-memory ((o common-db-gdbserver) addr data &aux
                             (c (ctx-core o)))
  (handler-case (progn
                  (when *trace-comdb-calls*
                    (log-comdb 'write-block "~X, 0x~X bytes~:[~;, MD5: ~:*~{~2,'0X~}~]"
                               addr (length data)
                               #+disable-ironclad nil
                               #-disable-ironclad (coerce (ironclad:digest-sequence :md5 data) 'list))
                    (when *trace-comdb-memory-io*
                      (print-u8-sequence *trace-output* data :address addr)))
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
  (when *trace-comdb-calls*
    (log-comdb 'core-stop-reason "~A" (core-stop-reason (ctx-core o))))
  (encode-gdb-stop-reason (typep (core-stop-reason (ctx-core o)) 'trap)))

(defmethod gdb-interrupt ((o common-db-gdbserver) &aux
                          (core (ctx-core o)))
  (when (core-running-p core)
    (when *trace-comdb-calls*
      (log-comdb '(setf state) ":STOP"))
    (setf (state core) :stop))
  (when *trace-comdb-calls*
    (log-comdb '(setf state) ":DEBUG"))
  (setf (state core) :debug)
  (values))

(defmethod gdb-continue-at ((o common-db-gdbserver) addr &aux
                            (core (ctx-core o)))
  (when *trace-comdb-calls*
    (log-comdb 'run-core-asynchronous "~:[continuing~;jumping to address ~:*~X~]" addr))
  (run-core-asynchronous core (unless (or (not addr)
                                          (= addr (moment-fetch (saved-core-moment core))))
                                addr))
  (loop
     ;; Poll for condition of remote target every now and then. Not
     ;; very pretty...
     (unless (zerop *poll-interval*)
       (sleep *poll-interval*))
     ;; Check to see if GDB wants a break or if the target has hit a
     ;; breakpoint.
     (let ((int? (check-interrupt o))
           (db? (not (core-running-p core))))
       (when (and *trace-comdb-calls*
                  *trace-even-noisy-comdb-calls*)
         (log-comdb 'core-running-p "~:[running~;stopped~]" db?))
       (when (or int? db?)
         (when *trace-comdb-calls*
           (log-comdb '(setf state) ":DEBUG"))
         (setf (state core) :debug)
         ;; If we are interrupted or inside the debugger return to
         ;; GDB.
         (return-from gdb-continue-at
           (encode-gdb-stop-reason db?)))))  
  "S00")

(defmethod gdb-single-step-at ((o common-db-gdbserver) addr &aux
                               (core (ctx-core o)))
  (when *trace-comdb-calls*
    (log-comdb 'set-core-insn-execution-limit "1"))
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
  (declare (ignore arguments))
  (gdb-kill o))

;;;;
;;;; Breakpoints
(defmethod gdb-insert-breakpoint ((o common-db-gdbserver) type address length &aux
                                  (core (ctx-core o)))
  (cond
    ((eq type :software)
     (when *trace-comdb-calls*
       (log-comdb 'add-sw-breakpoint "~X" address))
     (add-sw-breakpoint core address) ; We ignore LENGTH for software breakpoints.
     "OK")
    ((not (= 4 length)) "EInvalidLength")
    (t
     (when *trace-comdb-calls*
       (log-comdb 'add-hw-breakpoint "~X => allocate.." address))
     (if-let ((b (add-hw-breakpoint core address)))
       (prog1 "OK"
         (when *trace-comdb-calls*
           (log-comdb 'add-hw-breakpoint "~X => success" address)))
       "ENoDebugRegistersLeft"))))

(defmethod gdb-remove-breakpoint ((o common-db-gdbserver) type address length &aux
                                  (core (ctx-core o)))
  (declare (ignore type length))
  (if-let ((b (trap core address :if-does-not-exist :continue)))
          (progn
            (when *trace-comdb-calls*
              (log-comdb 'disable-trap "~X" address))
            (disable-trap b)
            "OK")
          "ENoSuchBreakpoint"))

;;;;
;;;; Termination
(defmethod gdb-detach ((o common-db-gdbserver) &aux
                       (core (ctx-core o)))
  (dolist (core (cons core (master-device-slaves core)))
    (do-core-traps (nil b core)
      (when *trace-comdb-calls*
        (log-comdb 'disable-trap "~X" (trap-address b)))
      (disable-trap b)))
  ;; Call next method to really terminate the connection.
  (call-next-method))

(defmethod gdb-kill ((o common-db-gdbserver) &aux
                     (core (ctx-core o)))
  (when *trace-comdb-calls*
    (log-comdb 'reset ""))
  (reset :core core)
  "OK")

;;;;
;;;; Security hole
(defmethod gdb-monitor ((o common-db-gdbserver) (command (eql :eval)) rest-arg)
  (when *trace-comdb-calls*
    (log-comdb 'eval "~S" rest-arg))
  (values-list
   (mapcar #'princ-to-string
           (multiple-value-list
            (handler-case (eval (let ((*package* (find-package :common-db)))
                                  (read-from-string rest-arg)))
              (serious-condition (c)
                (format nil "~A" c)))))))

;;;;
;;;; Stubs
(defmethod gdb-set-thread ((o common-db-gdbserver) domain thread)
  (declare (ignore domain))
  (if (or (= thread -1)
          (= thread 0))
      "OK"
      "E00"))

(defmethod gdb-handle-query ((o common-db-gdbserver) (q (eql :attached)) args)
  (declare (ignore args))
  "0")

;;;;
;;;; Server
(defvar *gdbserver-help-en*
  "  GDB-server options:
    --address <dotted-quad>     Address of the interface to accept connections.
                                  Defaults to 127.0.0.1.
    --port <integer>            Number of the TCP port to accept connections on.
                                  Defaults to 9000.
    --trace-comdb-calls         Trace all common-db API calls, except noisy.
    --trace-even-noisy-comdb-calls
                                Trace noisy common-db API calls as well.
    --trace-comdb-memory-io     Trace all memory IO.
    --trace-exchange            Trace gdbserver protocol exchange.
    --single-shot               Exit after the first connection terminates.")

(defun gdbserver-toplevel ()
  (setf common-db::*additional-help-en* *gdbserver-help-en*
        common-db::*additional-help-ru* common-db::*gdbserver-help-ru*)
  (comdb::comdb-toplevel-wrapper #'gdbserver
                                 '((:address :string) :port :trace-exchange)
                                 '(:start-swank :single-shot :trace-comdb-calls :trace-even-noisy-comdb-calls :trace-comdb-memory-io)
                                 :no-memory-detection t))

(defun gdbserver (&key verbose (target-context *current*) (address "127.0.0.1") (port 9000) single-shot
                  start-swank trace-comdb-calls trace-even-noisy-comdb-calls trace-comdb-memory-io trace-exchange &aux
                  (core (if target-context
                            (ctx-core target-context)
                            (error "~@<No active target context: cannot start GDB server.~:@>"))))
  (change-class target-context 'common-db-gdbserver)
  (let ((ri-names (gdb:core-register-order core))
        (*trace-comdb-calls* trace-comdb-calls)
        (*trace-even-noisy-comdb-calls* trace-even-noisy-comdb-calls)
        (*trace-comdb-memory-io* trace-comdb-memory-io))
    (setf *gdb-register-instance-vector*
          (make-array (length ri-names)
                      :initial-contents (mapcar (curry #'device-register-instance core) ri-names))
          *gdb-id-to-register-instance-map*
          (make-hash-table))
    (when start-swank
      #-with-swank
      (format t "; Swank server start requested, but swank wasn't compiled in, ignoring.~%")
      #+with-swank
      (swank:create-server :dont-close t))
    (iter (format t "; Accepting connections on ~A:~D~:[~;, tracing exchanges, up to ~:*~X bytes~]~%" address port trace-exchange)
          (setf (slot-value target-context 'gdbremote::no-ack-mode) nil)
          (reset :core core)
          (accept-gdb-connection target-context port address trace-exchange)
          (until single-shot))))
