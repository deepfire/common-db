;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CORE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2009, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2007-2009 by
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

(in-package :core)


(defvar *log-core-pipeline-crit* nil)

;;;;
;;;; Moments of core
;;;;
(defclass moment ()
  ((fetch :accessor moment-fetch :initarg :fetch)
   (opcode :accessor moment-opcode :initarg :opcode)))

(define-print-object-method ((o moment) fetch opcode)
    "PC ~8,'0X, opcode ~8,'0X" fetch opcode)

(defun make-moment (type fetch opcode &rest moment-args &key &allow-other-keys)
  (apply #'make-instance type :fetch fetch :opcode opcode moment-args))

(defgeneric core-moment (core))
(defgeneric set-core-moment (core moment))
(defgeneric make-neutral-moment (core address))
(defgeneric derive-moment (moment address)
  (:method ((m moment) (address integer))
    (make-moment 'moment address (moment-opcode m))))

(defsetf core-moment set-core-moment)

;;;;
;;;; ...and trails thereof.
;;;;
(define-protocol-class trail () ())

(defgeneric core-trail (core))
(defgeneric set-core-trail (core trail))
(defgeneric make-neutral-trail (core))

(defsetf core-trail set-core-trail)

(defgeneric pc (core))
(defgeneric (setf pc) (value core))

(defun %pc (core selector)
  (declare (ignore selector))
  (pc core))

(defun (setf %pc) (value core selector)
  (declare (ignore selector))
  (setf (pc core) value))

;;;;
;;;; Core
;;;;
(define-namespace :core
  (:layouts
   ((:control   "Control")
    (:pc        0))))

(define-protocol-device-class core :core (generic:memory-device)
  (;; generic properties
   (isa :accessor core-isa :initarg :isa)
   (insn-execution-limit :accessor core-insn-execution-limit :initarg :insn-instruction-limit)
   ;; constituents
   (traps :accessor core-traps :type hash-table :initarg :traps :documentation "All address traps, by address.")
   (hw-breakpoints :accessor core-hw-breakpoints :type hash-table :initarg :hw-breakpoints :documentation "Hardware breakpoints, by id.")
   (default-sw-breakpoint-type :accessor core-default-sw-breakpoint-type :initarg :default-sw-breakpoint-type)
   (default-hw-breakpoint-type :accessor core-default-hw-breakpoint-type :initarg :default-hw-breakpoint-type)
   ;; state
   (instruction-counter :accessor core-instruction-counter :type (integer 0) :initarg :instruction-counter)
   (stop-reason :accessor core-stop-reason :initarg :stop-reason))
  (:documentation
   "This definition takes an obligation to implement the memory device 
protocol, but doesn't provides one. Which shifts the responsibility
to the concrete classes.")
  (:default-initargs
   :insn-instruction-limit nil
   :traps (make-hash-table)
   :hw-breakpoints (make-hash-table)
   :instruction-counter 0
   :stop-reason nil)
  (:layouts
   (:control %pc (setf %pc))))

(define-protocol-device-class general-purpose-core :core (core)
  ((moment :accessor saved-core-moment :initarg :moment)
   (moment-changed-p :accessor core-moment-changed-p :initarg :moment-changed-p)
   (trail :accessor saved-core-trail :initarg :trail)
   (trail-important-p :accessor core-trail-important-p :initarg :trail-important-p)
   (machine :accessor core-machine :initarg :machine)
   (slaves :accessor core-slaves :type list :initarg :slaves)
   (executable :accessor core-executable :initarg :executable))
  (:documentation "Cores which define GPR/(SETF GPR)")
  (:default-initargs
   :enumeration-class 'general-purpose-core
   :moment nil
   :moment-changed-p nil
   :trail-important-p nil
   :slaves nil
   :executable nil))

(define-protocol-device-class little-endian-core :core (core) ())
(define-protocol-device-class big-endian-core :core (core) ())

(define-protocol-device-class mmu-core :core (core)
    ((tlb-entries-nr :reader core-tlb-entries-nr :initarg :tlb-entries-nr)))

(defmethod set-core-moment :after ((o general-purpose-core) moment)
  (setf (core-moment-changed-p o) t
        (saved-core-trail o) (make-neutral-trail o)
        (core-trail-important-p o) nil))

(defmethod (setf saved-core-moment) :after (moment (o general-purpose-core))
  (setf (core-moment-changed-p o) t))

(defmethod (setf saved-core-trail) :after (trail (o general-purpose-core))
  (setf (core-trail-important-p o) t))

(define-subcontainer trap :container-slot traps :type address-trap :iterator do-core-traps :remover remove-trap :if-exists :continue)
(define-subcontainer hwbreak :container-slot hw-breakpoints :type hardware-breakpoint :iterator do-core-hwbreaks :if-exists :error)

(defgeneric coerce-to-trap (trap-specifier))

(define-print-object-method ((o core) id backend)
    "~A backend: ~A" id (with-output-to-string (string) (print-device-object backend string)))

(define-print-object-method ((o general-purpose-core) id backend moment trail)
    "~A backend: ~A moment: ~A trail: ~A"
  id (with-output-to-string (string) (print-device-object backend string)) moment trail)

(define-condition core-condition ()
  ((core :accessor condition-core :initarg :core)))
(define-condition core-error (error core-condition) ())
(define-reported-condition core-halt-failure (core-error) () 
  (:report (core)
           "~@<failed to halt core ~S~:@>" core))
(define-reported-condition invalid-core-frequence-multiplier (core-error)
  ((frequency-multiplier :accessor condition-frequency-multiplier :initarg :frequency-multiplier))
  (:report (core frequency-multiplier)
           "~@<Frequency multiplier ~S is invalid for core ~S.~:@>" frequency-multiplier core))
(define-condition core-execution-error (core-error) 
  ((segment :reader condition-segment :initarg :segment)
   (address :reader condition-address :initarg :address)
   (error-address :reader condition-error-address :initarg :error-address)))
(define-reported-condition unexpected-stop-reason (core-execution-error) 
  ((expected :reader condition-expected :initarg :expected)
   (actual :reader condition-actual :initarg :actual))
  (:report (core segment address error-address actual)
           "~@<While executing ~S on ~S at #x~8,'0X: unexpected stop reason ~S. EPC: ~8,'0X.~:@>"
           segment core address actual error-address))

(defun core-report (core format-control &rest format-arguments)
  (apply #'format *log-stream* (concatenate 'string "~&CORE~A: " format-control "~%") (enumerated-id core) format-arguments))

;;;;
;;;; GPR
;;;;
(defgeneric gpr (core gpr))
(defgeneric set-gpr (core i val))
(defsetf gpr set-gpr)
(defgeneric gpr-by-name (core gpr))
(defgeneric set-gpr-by-name (core gpr value))
(defsetf gpr-by-name set-gpr-by-name)
;;;;
;;;; Pipeline
;;;;
(defgeneric push-core-pipeline-stages (core nstages))
(defgeneric print-pipeline (core stream))
(defgeneric print-pipeline-terse (core stream))
;;;;
;;;; State
;;;;
(defgeneric core-running-p (core))
(defgeneric (setf core-running-p) (run-p core))
(defgeneric step-core-asynchronous (core))
(defgeneric step-core-debug (core))
(defgeneric reset-core (core))
(defgeneric free-to-stop (core &rest args)  (:method ((o general-purpose-core) &rest args) (declare (ignore args)) t))
(defgeneric stop-to-debug (core &rest args) (:method ((o general-purpose-core) &rest args) (declare (ignore args)) t))
(defgeneric debug-to-stop (core &rest args) (:method ((o general-purpose-core) &rest args) (declare (ignore args)) t))
(defgeneric stop-to-free (core &rest args)  (:method ((o general-purpose-core) &rest args) (declare (ignore args)) t))
(defgeneric deduce-stop-reason (core))
(defgeneric poll-core-interruptible (core &optional watch-fn watch-period iteration-period iteration-limit)
  (:documentation
   "Poll CORE until it is not running with ITERATION-PERIOD 
granularity, or the user requests termination, while executing WATCH-FN
every ITERATION-PERIOD * WATCH-PERIOD nanoseconds. The return value is the stop reason.
ITERATION-LIMIT Optionally soft-limits the time spent sleeping."))
(defgeneric analyse-core (core)
  (:documentation "Collect necessary forensics just after the core stopped."))
(defgeneric wait-core (core &optional watch-fn watch-period iteration-period iteration-limit)
  (:documentation "Poll core until it stops, then ANALYSE it."))
;;;;
;;;; Traps
;;;;
(defgeneric set-core-insn-execution-limit (core ninsns))
(defgeneric enable-trap (controlled-trap))
(defgeneric disable-trap (controlled-trap))
(defgeneric forget-volatile-trap (volatile-address-trap))
(defgeneric set-trap-enabled (controlled-trap enabledp))
(defgeneric add-sw-breakpoint (core address))
(defgeneric disable-breakpoint (breakpoint))
(defgeneric setup-hw-breakpoint (breakpoint address skip-count &key &allow-other-keys))
(defgeneric add-hw-breakpoint (core address &optional skip-count)
  (:method ((core core) address &optional (skipcount 0))
    (when-let ((free-breakpoint (allocate-hardware-breakpoint core)))
      (setup-hw-breakpoint free-breakpoint address skipcount))))
(defgeneric add-cell-watchpoint (core address &optional skip-count))

(defsetf trap-enabled-p set-trap-enabled)
;;;;
;;;; Slaves
;;;;
(defgeneric freeze-core-slaves (core))
(defgeneric thaw-core-slaves (core))
;;;;
;;;; Miscellaneous
;;;;
(defgeneric flush-core-instruction-cache (core))
(defgeneric flush-core-data-cache (core))
(defgeneric core-call-stack (core))
(defgeneric capture-instruction-counters (core))
(defgeneric restore-instruction-counters (core))
(defgeneric reset-instruction-counters (core))
(defgeneric watch-core (core cenv stack-top))
(defgeneric prepare-trans-args (core stack-top args))
(defgeneric trans-funcall (core cenv address-space function-name args &key &allow-other-keys))

(defvar *trace-trans-calls* nil
  "Whether the trans calls should be traced for correspondence between code in memory
and code actually hitting the target core pipeline.  This is useful for tracking
icache-related anomalies.")

(define-execute-with-bound-variable *trace-trans-calls*
  (:binding traced-trans-calls t :define-with-maybe-macro t))

(defgeneric handle-execution-error (core condition-type condition-args)
  (:method ((o core) condition-type condition-args)
    (error (apply #'make-condition condition-type condition-args))))


;;;;
;;;; Disassembly
;;;;
(defun print-variable-length-opcode (stream opcode-and-width colon at)
  (declare (ignore colon at))
  (destructuring-bind (opcode . width) opcode-and-width
    (ecase width
      (4 (format stream "~8,'0X~8,1@T" opcode))
      (8 (format stream "~16,'0X" opcode)))))

(defun default-disassembly-line-printer (stream addr opcode width insn params)
  (format stream "~8,'0X:    ~/core::print-variable-length-opcode/    ~A~1,14T~{~S,~2,6T~}" addr (cons opcode width) insn params))

(defun disassemble-and-print (stream isa startaddr seq &optional line-pre-annotate-fn line-post-annotate-fn
                              (line-fn #'default-disassembly-line-printer))
  (iter (for (opcode width insn . params) in (disassemble isa seq))
        (for addr initially startaddr then (+ addr width))
        (funcall (or line-pre-annotate-fn #'values) stream addr)
        (funcall line-fn stream addr opcode width insn params)
        (funcall (or line-post-annotate-fn #'values) stream addr insn params)
        (terpri stream)))

(defun core-disassemble (core addr len &key (stream *standard-output*) line-pre-annotate-fn line-post-annotate-fn
                         (line-fn #'default-disassembly-line-printer))
  (let* ((ioaddr (logandc1 #x3 addr))
         (iolen (logandc1 #b11 (logior len (if (zerop (logand #b11 len)) 0 #b100))))
         (ioarr (make-array iolen :element-type '(unsigned-byte 8))))
    (read-block core ioaddr ioarr)
    (disassemble-and-print stream (core-isa core) addr ioarr line-pre-annotate-fn line-post-annotate-fn line-fn)))

;;;;
;;;; Debugger
;;;;
(defun invoke-with-core-debugger (core fn &key segment address)
  (labels ((reader (desc)
             (format *debug-io* "Enter a positive integer for ~A: " desc)
             (finish-output *debug-io*)
             (let ((input (read *debug-io*)))
               (if (and (integerp input) (plusp input))
                   input
                   (progn
                     (format *debug-io* "~S is not a positive integer, please try again.~%" input)
                     (finish-output *debug-io*)
                     (reader desc)))))
           (nvalue-reader (&rest descs)
             (mapcar #'reader descs))
           (reset-core ()
             (reset-platform core)
             (setf (state core) :debug)
             t))
    (loop (restart-case (funcall fn)
            (cycle ()
              :report "Recheck core status.")
            (exit-and-continue ()
              :report "Exit the debugger and continue execution."
              (return))
            (print-pipeline ()
              :report "Print core pipeline."
              (print-pipeline core *debug-io*))
            (reset-and-abort ()
              :report "Reset the core, enable debug mode and abort."
              (reset-core)
              (invoke-restart (find-restart 'abort)))
            (reset-interface ()
              :report "Reset the interface."
              (interface:interface-reset (backend (backend core))))
            (reset-target ()
              :report "Reset the target and enable debug mode."
              (reset-core))
            (dump-memory (base size)
              :report "Dump memory."
              :interactive (lambda () (nvalue-reader "dump base" "dump size"))
              (core-disassemble core base size :stream *debug-io*))
            (disassemble-memory (base size)
              :report "Disassemble memory."
              :interactive (lambda () (nvalue-reader "disassemble base" "disassemble size"))
              (core-disassemble core base size :stream *debug-io*))
            (disassemble ()
              :report "Disassemble the executed segment."
              :test (lambda (c) (declare (ignore c)) segment)
              (disassemble-and-print *debug-io*
                                     (core-isa core)
                                     (or address (when (typep segment 'pinned-segment) (pinned-segment-base segment)) 0)
                                     (segment-active-vector segment)))))))

(defmacro with-core-debugger ((core &key segment address) &body form)
  `(invoke-with-core-debugger ,core (lambda () ,@form)
                              ,@(when segment `(:segment ,segment))
                              ,@(when address `(:address ,address))))

;;;;
;;;; Core API implementations
;;;;
(defmethod gpr-by-name ((o core) (gpr-name symbol))
  (gpr o (optype-evaluate (isa-gpr-optype (core-isa o)) gpr-name)))

(defmethod set-gpr-by-name ((o core) (gpr-name symbol) value)
  (set-gpr o (optype-evaluate (isa-gpr-optype (core-isa o)) gpr-name) value))

(defmethod reset-core :around ((o general-purpose-core))
  (setf (saved-core-moment o) (make-neutral-moment o nil)
        (saved-core-trail o) (make-neutral-trail o)
        (core-trail-important-p o) nil)
  (mapc #'reset-core (core-slaves o))
  (call-next-method))

(defmethod reset-core :around ((o core))
  (do-core-traps (b o)
    (disable-breakpoint b))
  (call-next-method))

(defmethod reset-instruction-counters :after ((o core))
  (setf (core-instruction-counter o) 0))

(defmethod add-sw-breakpoint ((o core) address)
  (lret ((bp (or (let ((b (trap o address)))
                   (when (typep b 'software-breakpoint)
                     b))
                 (make-instance (core-default-sw-breakpoint-type o) :core o :address address))))
    (setf (trap-enabled-p bp) t)))

(defgeneric reset-platform (core &key &allow-other-keys)
  (:method ((o core) &key &allow-other-keys))
  (:method :around ((o core) &rest platform-args &key stop-cores-p &allow-other-keys)
    (let ((target (backend o)))
      (iface:interface-reset-target (backend target) stop-cores-p)
      (mapc #'reset-core (target-devices-by-type target 'general-purpose-core))
      (call-next-method)
      (apply #'configure-target-platform target (target-platform target)
             (remove-from-plist platform-args :stop-cores-p)))))

(defun step-core-synchronous (core)
  "Perform steps necessary to make CORE do one step, and wait until it stops.
The return value is T, except when execution is interrupted by SIGINT, 
in which case it is NIL."
  (step-core-asynchronous core)
  (busywait-interruptible-executing (not (core-running-p core)) nil))

(defmethod poll-core-interruptible ((core core) &optional (watch-fn #'values) (watch-period 1) (iteration-period 10000000) run-iteration-limit)
  (busywait-interruptible-executing (not (core-running-p core)) (funcall watch-fn *standard-output*)
                                    :watch-period watch-period :iteration-period iteration-period :run-time run-iteration-limit))


(defmethod analyse-core :before ((core general-purpose-core))
  (setf (saved-core-moment core) (core-moment core)
        (saved-core-trail core) (core-trail core)))

(defmethod wait-core ((core core) &optional (watch-fn #'values) (watch-period 1) (iteration-period 10000000) iteration-limit)
  (let ((execution-status (poll-core-interruptible core watch-fn watch-period iteration-period iteration-limit)))
    (analyse-core core)
    (ecase execution-status
      (:timeout :timeout)
      ((t)      (setf (core-stop-reason core) (deduce-stop-reason core)))
      ((nil)    (setf (core-stop-reason core) (make-instance 'user-interruption :core core))))))

(defun prime-core-executable (core loadable &optional check)
  "Prepare LOADABLE to be executed on a properly configured general-purpose
CORE. When CHECK is non-NIL, an integrity check during upload of LOADABLE
is performed."
  (loadable:upload-loadable (backend core) loadable :section-before-fn #'loadable:report-section :check check)
  (when *log-loadable-processing*
    (format *log-stream* "Setting continuation address of ~S to ~8,'0X.~%" core (loadable:loadable-entry-point loadable)))
  (setf (core-executable core) loadable)
  (let ((trail (make-neutral-trail core))
        (moment (make-neutral-moment core (loadable:loadable-entry-point loadable))))
    (setf (saved-core-trail core) trail
          (core-trail core) trail
          (core-trail-important-p core) nil ; just done it manually
          (saved-core-moment core) moment
          (core-moment core) moment)))

(defun run-core-asynchronous (core &optional address (moment-changed (not (null address))))
  (prog1 (state core)
    (setf (state core
                 :address (or address (moment-fetch (saved-core-moment core)))
                 :moment-changed moment-changed)
          :free)))

(defun run-core-synchronous (core &key address (moment-changed (not (null address))) exit-state watch-fn watch-period (iteration-period 10000000)
                             segment iteration-limit (if-limit-reached :error) &aux
                             (watch-fn (or watch-fn #'values))
                             (watch-period (or watch-period 1)))
  "Put CORE into running state, possibly changing its continuation address
to ADDRESS, and specifying its MOMENT-CHANGED property, after which poll
for either CORE stopping or the user requesting termination. The EXIT-STATE
keyword customizes the exit state, which defaults to the state at the time
of the call. The CORE's status is polled every ITERATION-PERIOD nanoseconds,
optionally up to ITERATION-LIMIT times and an optional WATCH-FN is ran
every WATCH-PERIOD such polls."
  (let ((old-state (run-core-asynchronous core address moment-changed))
        execution-status)
    (funcall watch-fn *log-stream*)
    (unwind-protect (setf execution-status (wait-core core watch-fn watch-period iteration-period iteration-limit))
      (cond ((eq execution-status :timeout)
             (ecase if-limit-reached
               (:break)
               (:warn
                (warn "~@<While executing ~:[~;~:*~S ~]on ~S at #x~8,'0X:~_~
                          ran out of execution time budget of ~D nanoseconds.~_~
                          Core is ~:[not running anymore~;still running~].~:@>"
                      segment core address (* iteration-period iteration-limit)
                      (core-running-p core)))
               (:error
                (with-core-debugger (core :address address :segment segment)
                  (error "~@<While executing ~:[~;~:*~S ~]on ~S at #x~8,'0X:~_~
                          ran out of execution time budget of ~D nanoseconds.~_~
                          Core is ~:[not running anymore~;still running~].~:@>"
                         segment core address (* iteration-period iteration-limit)
                         (core-running-p core))))))
            (t
             (setf (state core) (or exit-state old-state)))))))

;;;;
;;;; State
;;;;
(defparameter *log-state-changes* nil)
(defparameter *core-transitions* `((:free  :stop  ,#'free-to-stop)
                                   (:stop  :debug ,#'stop-to-debug)
                                   (:debug :stop  ,#'debug-to-stop)
                                   (:stop  :free  ,#'stop-to-free)))

(defmethod initialize-instance :after ((o general-purpose-core) &key &allow-other-keys)
  (setf (core-machine o) (state:make-machine o (if (core-running-p o) :free :stop)
                                             '(:free :stop :debug)
                                             :change-fn (lambda (from to &rest rest)
                                                          (declare (ignore rest))
                                                          (when *log-state-changes*
                                                            (core-report o "changed state from ~S to ~S" from to)))))
  (unless (core-running-p o)
    (setf (saved-core-moment o) (core-moment o)
          (saved-core-trail o) (core-trail o)))
  (mapcar (curry #'apply #'state:set-transition-action (core-machine o)) *core-transitions*))

(defmethod stop-to-free ((core general-purpose-core) &key address moment-changed (insn-execution-limit nil iel-specified) &allow-other-keys)
  (when *log-core-pipeline-crit*
    (let ((moment (core-moment core)))
      (core-report core "STOP->FREE: op/pc/mchg/mchgdcl -> ././.: ~X ~X ~S ~S => ~X ~X ~S ~S"
                   (moment-opcode moment) (moment-fetch moment) (core-moment-changed-p core) moment-changed
                   (moment-opcode (saved-core-moment core)) (or address (moment-fetch (saved-core-moment core))) (or (and address t) moment-changed)
                   (decode-insn (core-isa core) (moment-opcode (saved-core-moment core))))))
  (when iel-specified
    (setf (core-insn-execution-limit core) insn-execution-limit))
  (setc (core-moment core) (if address
                               (derive-moment (saved-core-moment core) address)
                               (saved-core-moment core))
        (core-moment-changed-p core) (or (and address t) moment-changed)
        (core-running-p core) t))

(defun deeper-state (state)
  (ecase state
    (:free :stop)
    (:stop :debug)
    (:debug (error "there is no ~A debug state for ~S" "deeper" state))))

(defun shallower-state (state)
  (ecase state
    (:debug :stop)
    (:stop (error "there is no ~A debug state for ~S" "shallower" state))))

(defun state (core)
  (state:state (core-machine core)))

(defun (setf state) (new-state core &rest transition-args)
  (setf (state:state (core-machine core) transition-args) new-state))

(defun invoke-with-maybe-state (core entry-state exit-state maybe args fn)
  (unwind-protect (progn
                    (when maybe
                      (apply #'(setf state) entry-state core args))
                    (funcall fn))
    (setf (state core) exit-state)))

(defmacro with-state ((core &rest transition-args &key (entry-state :free) (exit-state :debug) &allow-other-keys) &body body)
  `(invoke-with-maybe-state ,core ,entry-state ,exit-state t (list ,@(remove-from-plist transition-args :entry-state :exit-state))
                            (lambda () ,@body)))

(defmacro with-maybe-temporary-state ((core state maybe &rest transition-args &key &allow-other-keys) &body body)
  (with-gensyms (state-save)
    (once-only (core)
      `(let ((,state-save (state ,core)))
         (invoke-with-maybe-state ,core ,state ,state-save ,maybe (list ,@(remove-from-plist transition-args :entry-state :exit-state))
                                  (lambda () ,@body))))))

(defmacro with-temporary-state ((core state &rest transition-args &key &allow-other-keys) &body body)
  `(with-maybe-temporary-state (,core ,state t ,@transition-args)
     ,@body))

(defun invoke-with-retry-with-state-restart (core state fn)
  (let ((to-state (state core)))
    (with-retry-restarts ((retry-with-state ()
                            :report (lambda (stream)
                                      (format stream "Retry in ~S mode." state))
                            (setf to-state state)))
      (with-temporary-state (core to-state)
        (funcall fn)))))

(defmacro with-retry-with-state-restart ((core state) &body body)
  `(invoke-with-retry-with-state-restart ,core ,state (lambda () ,@body)))

;;;;
;;;; Core stop reasoning
;;;;
(define-protocol-class core-stop-reason () ())

(define-protocol-class trap (core-stop-reason)
  ((core :accessor trap-core :initarg :core)))

(define-protocol-class controlled-trap (trap)
  ((enabled :reader trap-enabled-p :initarg :enabled))
  (:default-initargs
   :enabled nil))

(define-protocol-class address-trap (controlled-trap)
  ((address :accessor trap-address :initarg :address))
  (:default-initargs
   :address nil))

(define-protocol-class skippable-trap (controlled-trap)
  ((skipcount :accessor trap-skipcount :type (unsigned-byte 32) :initarg :skipcount))
  (:default-initargs
   :skipcount 0))

(define-protocol-class enumerated-trap (controlled-trap)
  ((id :accessor trap-id :initarg :id)))

(define-protocol-class volatile-address-trap (address-trap) ())

(defclass user-interruption (core-stop-reason) ())

(defclass intercore-trap (trap) 
  ((trapping-core :accessor trap-causing-core :initarg :trapping-core)))

(defclass instruction-count-trap (controlled-trap) 
  ((instruction-limit :accessor trap-instruction-limit)))

(defclass vector-trap (address-trap) ())
(defclass memory-access-trap (address-trap) ())

(defclass breakpoint (address-trap)
  ()
  (:default-initargs
   :address #xfaced911
   :skipcount 1))

(defclass hardware-breakpoint (breakpoint)
  ((owned :accessor breakpoint-owned-p :initarg :owned))
  (:default-initargs
   :owned nil))
(defclass software-breakpoint (volatile-address-trap breakpoint)
  ((saved-insn :accessor software-breakpoint-saved-insn :initform 0 :initarg :saved-insn :type (unsigned-byte 32))))

(defmacro do-core-controlled-traps ((o core) &body body)
  `(do-core-traps (,o ,core)
     (when (typep ,o 'controlled-trap) ,@body)))
(defmacro do-core-vector-traps ((o core) &body body)
  `(do-core-traps (,o ,core)
     (when (typep ,o 'vector-trap) ,@body)))
(defmacro do-core-hardware-breakpoints ((o core) &body body)
  `(do-core-traps (,o ,core)
     (when (typep ,o 'hardware-breakpoint) ,@body)))
(defmacro do-core-software-breakpoints ((o core) &body body)
  `(do-core-traps (,o ,core)
     (when (typep ,o 'software-breakpoint) ,@body)))

(define-print-object-method ((o trap)) "")
(define-print-object-method ((o intercore-trap) trapping-core)
    " trapping core: ~S" trapping-core)
(define-print-object-method ((o controlled-trap) enabled)
    " ~:[dis~;en~]abled" enabled)
(define-print-object-method ((o address-trap) enabled address)
    " ~:[dis~;en~]abled, address: 0x~X" enabled address)
(define-print-object-method ((b hardware-breakpoint) address trigcount id)
    "~8,'0X trigcount: ~D id: ~D" address trigcount id)
(define-print-object-method ((b software-breakpoint) address trigcount saved-insn)
    "~8,'0X insn: 0x~8,'0X" address saved-insn)

(defmethod initialize-instance :after ((o core) &key &allow-other-keys)
  (do-core-hwbreaks (b o)
    (setf (trap-core b) o))
  (do-core-vector-traps (v o)
    (setf (trap-core v) o
          (trap o (trap-address v)) v)))

(defmethod set-trap-enabled ((o controlled-trap) enabledp)
  (if enabledp
      (enable-trap o)
      (disable-trap o))
  enabledp)
(defmethod enable-trap   :after ((o controlled-trap)) (setf (slot-value o 'enabled) t))
(defmethod disable-trap  :after ((o controlled-trap)) (setf (slot-value o 'enabled) nil))
(defmethod initialize-instance :after ((o address-trap) &key core address &allow-other-keys)
  (when (and core address)
    (setf (trap core address) o)))

(defmethod coerce-to-trap ((o address-trap))
  o)

(defmethod forget-volatile-trap ((o volatile-address-trap))
  (when (trap-enabled-p o)
    (disable-trap o))
  (remove-trap (trap-core o) (trap-address o)))

(defmethod disable-breakpoint ((o volatile-address-trap))
  (forget-volatile-trap o))
(defmethod disable-breakpoint ((o controlled-trap))
  (disable-trap o))

(defmethod setup-hw-breakpoint :before ((o hardware-breakpoint) address skipcount &key &allow-other-keys)
  (setf (trap-address o) address))

(defmethod setup-hw-breakpoint :after ((o hardware-breakpoint) address skipcount &key &allow-other-keys)
  (setf (trap-enabled-p o) (not (null address))))

(defun allocate-hardware-breakpoint (core)
  (do-core-hwbreaks (b core)
    (unless (breakpoint-owned-p b)
      (setf (breakpoint-owned-p b) t)
      (return b))))

(defun release-hardware-breakpoint (b)
  (setf (breakpoint-owned-p b) nil))

(defun allocate-hardware-breakpoints (core n)
  (lret ((breakpoints (iter (repeat n) (collect (allocate-hardware-breakpoint core)))))
    (unless (every #'identity breakpoints)
      (dolist (allocated breakpoints)
        (when allocated
          (release-hardware-breakpoint allocated)))
      (error "~@<Not enough free hardware breakpoints on ~S.~:@>" core))))

(defun invoke-with-maybe-free-hardware-breakpoints (maybe core fn addresses skipcount
                                                    &rest breakpoint-setup-args &key &allow-other-keys)
  (if maybe
      (let ((breakpoints (allocate-hardware-breakpoints core (length addresses))))
        (unwind-protect (progn
                          (iter (for b in breakpoints)
                                (for a in addresses)
                                (when a
                                  (apply #'setup-hw-breakpoint b a skipcount breakpoint-setup-args)))
                          (apply fn breakpoints))
          (dolist (b breakpoints)
            (release-hardware-breakpoint b)
            (disable-trap b))))
      (funcall fn nil)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun parse-defaulting-binding-list (binding-list &optional default)
    (iter (for binding in binding-list)
          (collect (if (consp binding) (first binding) binding) into vars)
          (collect (if (consp binding) (second binding) default) into values)
          (finally (return (values vars values))))))

(defmacro with-maybe-free-hardware-breakpoints ((maybe core &optional (skipcount 0) &rest breakpoint-args &key &allow-other-keys)
                                                (&rest breakpoint-bindings)
                                                &body body)
  "Execute BODY with BREAKPOINT bound to a free hardware breakpoint.
BREAKPOINT is released when the form is exited, by any means."
  (multiple-value-bind (vars addresses) (parse-defaulting-binding-list breakpoint-bindings)
    `(invoke-with-maybe-free-hardware-breakpoints ,maybe ,core (lambda ,vars
                                                                 (declare (ignorable ,@vars))
                                                                 ,@body)
                                                  (list ,@addresses) ,skipcount ,@breakpoint-args)))

(defmacro with-free-hardware-breakpoints ((core &optional (skipcount 0) &rest breakpoint-args &key &allow-other-keys)
                                          (&rest breakpoint-bindings)
                                          &body body)
  "Execute BODY with BREAKPOINT bound to a free hardware breakpoint.
BREAKPOINT is released when the form is exited, by any means."
  `(with-maybe-free-hardware-breakpoints (t ,core ,skipcount ,@breakpoint-args) ,breakpoint-bindings
     ,@body))

(defun invoke-with-traps (traps fn &aux
                          (traps (mapcar #'coerce-to-trap traps)))
  "Call FN with TRAPS enabled within the dynamic extent of the call."
  (dolist (trap traps)
    (enable-trap trap))
  (unwind-protect (funcall fn)
    (dolist (trap traps)
      (disable-trap trap))))

;;; XXX: this corelessness seriously ain't good
(defmacro with-traps ((&rest traps) &body body)
  "Execute BODY with TRAPS enabled."
  `(invoke-with-traps (list ,@traps) (lambda () ,@body)))

;;;;
;;;; Assembly & snippet execution
;;;;
(defun trace-segment (core segment &key continuep)
  (upload-segment core segment)
  (with-temporary-state (core :stop)
    ;; re-implementing the stop-to-free protocol, what's to say...
    (setf (core-moment core) (if continuep
                                 (derive-moment (saved-core-moment core) (pinned-segment-base segment))
                                 (make-neutral-moment core (pinned-segment-base segment))))
    ;; shall we use OTC/TRACE-MODE here, instead (wouldn't that be a lie)?
    (iter (repeat count)
          (while (prog1 (step-core-synchronous core)
                   (dolist (slave (core-slaves core))
                     (when (core-running-p slave)
                       (step-core-debug slave))))))
    (free-to-stop core)))

(defun execute-segment (core segment &key swbreaks (iteration-period 100000) iteration-limit watch-fn watch-period &aux
                        (address (pinned-segment-base segment)))
  (upload-segment core segment)
  (let ((swbreaks (mapcar (curry #'add-sw-breakpoint core) swbreaks)))
    (unwind-protect
         (with-free-hardware-breakpoints (core) ((b (+ address (length (segment-active-vector segment)))))
           (flet ((stop-reason-expected-p ()
                    (or (eq (core-stop-reason core) b)
                        (member (core-stop-reason core) swbreaks)
                        (typep (core-stop-reason core) 'user-interruption))))
             (setf (saved-core-moment core) (make-neutral-moment core address))
             (run-core-synchronous core :segment segment :iteration-period iteration-period :iteration-limit iteration-limit
                                   :watch-fn watch-fn :watch-period watch-period)
             (unless (stop-reason-expected-p)
               (handle-execution-error core 'unexpected-stop-reason
                                       (list :core core :segment segment :address address :actual (core-stop-reason core))))))
      (mapc #'forget-volatile-trap swbreaks))))

(defun invoke-with-execution-of-emitted-segment (emitter-fn core address &key trace report (iteration-period 100000) iteration-limit watch-fn watch-period)
  "Execute the code emitted by EMITTER-FN at ADDRESS on CORE,
with optional nanosecond-granular ITERATION-LIMIT and WATCH-FN."
  (let ((segment (with-segment-emission ((core-isa core) (make-instance 'pinned-segment :base address))
                   (funcall emitter-fn))))
    (when report
      (format *log-stream* "~@<WITH-EXECUTED-SEGMENT: emitted #x~X bytes @ #x~X.~:@>~%"
              (length (segment-active-vector *segment*)) address))
    (if trace
        (trace-segment core segment)
        (execute-segment core segment :iteration-period iteration-period :iteration-limit iteration-limit :watch-fn watch-fn :watch-period watch-period))))

(defmacro with-executed-segment ((core address &key trace report (iteration-period 100000) iteration-limit watch-fn watch-period) &body body)
  "Execute the code produced by evaluating BODY at ADDRESS on CORE,
with optional nanosecond-granular ITERATION-LIMIT and WATCH-FN."
  `(invoke-with-execution-of-emitted-segment (lambda () ,@body) ,core ,address :trace ,trace :report ,report
                                             :iteration-period ,iteration-period :iteration-limit ,iteration-limit :watch-fn ,watch-fn :watch-period ,watch-period))

(defmethod trans-funcall :around ((o core) (cenv compilation-environment) (as address-space) function-name args &key trace &allow-other-keys)
  (declare (ignore trace))
  (with-compilation-environment cenv
    (call-next-method)))

(defun trans-funcall* (core cenv as function-name &rest args)
  (trans-funcall core cenv as function-name args :trace *trace-trans-calls*))

(defun invoke-with-trans-funcallability (emission-fn fn address-space &key adjust-data-segment (dataseg-alignment #x10) (entry-length #x80))
  (with-slots (extent code data stack) address-space
    (let* ((cenv (funcall emission-fn))
           (code-length (length (segment-active-vector (first (cenv-segments cenv))))))
      (setf (size code) (+ code-length entry-length))
      (when adjust-data-segment
        (setf (base data) (align-up dataseg-alignment (+ (base extent) (size code) dataseg-alignment))
              (size data) (- (size extent) (- (base extent) (base data)) (size stack))))
      (funcall fn cenv address-space))))

(defmacro with-trans-funcallability ((cenv as as-form &key adjust-data-segment (dataseg-alignment #x10) (entry-length #x80)) emission-form &body body)
  `(invoke-with-trans-funcallability (lambda () ,emission-form) (lambda (,cenv ,as) ,@body) ,as-form
                                     :adjust-data-segment ,adjust-data-segment
                                     :dataseg-alignment ,dataseg-alignment
                                     :entry-length ,entry-length))

;;;;
;;;; TLB
;;;;
(defgeneric tlb-entry (core i))
(defgeneric set-tlb-entry (core i val))
(defsetf tlb-entry set-tlb-entry)
(defgeneric decode-tlb-entry (entry))
(defgeneric probe-tlb (core asid vpn))
(defgeneric get-tlb (core))
(defgeneric set-tlb (core new-tlb))
(defsetf get-tlb set-tlb)
(defgeneric tlb-address-map (core tlb page-size))

(defmethod get-tlb ((o mmu-core))
  (iter (for i below (core-tlb-entries-nr o))
        (collect (tlb-entry o i))))

(defmethod set-tlb ((o mmu-core) new-tlb)
  (iter (for i below (core-tlb-entries-nr o))
        (for entry in new-tlb)
        (set-tlb-entry o i entry)))

;;;;
;;;; Core frequency
;;;;
(defgeneric core-frequency-multiplier (core))
(defgeneric core-frequency-multiplier-valid-p (core frequency-multiplier)
  (:method ((o core) mult)
    (and (integerp mult) (> mult 1))))
(defgeneric default-core-frequency-multiplier (core)
  (:method ((o core))
    2))

(defmethod configure-platform-system :around (platform system &key core-multiplier prereset-core-multiplier)
  (let ((gp-core (target-device (backend system) '(general-purpose-core 0))))
    (call-next-method
     platform system
     :core-multiplier
     (lret ((multiplier (or core-multiplier (max (default-core-frequency-multiplier gp-core)
                                                 (or prereset-core-multiplier 0)))))
       (unless (core-frequency-multiplier-valid-p gp-core multiplier)
         (error 'invalid-core-frequence-multiplier :core gp-core :platform platform :frequency-multiplier multiplier))))))

;;;;
;;;; Bioable
;;;;
(defmethod read-block ((o core) base vector &optional start end)
  (read-block (backend o) base vector start end))

(defmethod write-block ((o core) base vector &optional start end)
  (write-block (backend o) base vector start end))