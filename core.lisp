;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CORE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2010, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2007-2010 by
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
;;;; Core protocol
;;;;
(define-namespace :core
  (:layouts
   ((:control   "Control")
    (:pc        0))))

(defun %pc (core selector) (declare (ignore selector)) (pc core))
(defun (setf %pc) (value core selector) (declare (ignore selector)) (setf (pc core) value))

(define-protocol-device-class core :core (generic:memory-device)
  (;; generic properties
   (isa :accessor core-isa :initarg :isa)
   (nopcode :reader core-nopcode)
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
protocol, but doesn't provide one.  Which shifts responsibility
to the concrete classes.")
  (:default-initargs
   :insn-instruction-limit nil
   :traps (make-hash-table)
   :hw-breakpoints (make-hash-table)
   :instruction-counter 0
   :stop-reason nil)
  (:layouts
   (:control %pc (setf %pc))))

;;; pipeline
(defgeneric core-moment (core))
(defgeneric set-core-moment (core moment))
(defsetf core-moment set-core-moment)
(defgeneric make-neutral-moment (core address))
(defgeneric derive-moment (moment address))

(defgeneric core-trail (core))
(defgeneric set-core-trail (core trail))
(defsetf core-trail set-core-trail)
(defgeneric make-neutral-trail (core))

(defgeneric pc (core)
  (:documentation
   "The backend must provide access to an illusion of a program counter,
which doesn't really exists as a single entity, on pipelined CPUs."))
(defgeneric (setf pc) (value core)
  (:documentation
   "The backend must provide access to an illusion of a program counter,
which doesn't really exists as a single entity, on pipelined CPUs."))

(defgeneric save-core-pipeline (core &key trail-important)
  (:documentation
   "The provided default method copies CORE's pipeline into backing store,
using CORE-MOMENT and CORE-TRAIL.  TRAIL-IMPORTANT provides means to specify
the importance of the trail; it defaults to T."))
(defgeneric finish-core-pipeline (core)
  (:documentation
   "The backend must provide a method which is expected to make sure that
all effects of instructions on CORE's pipeline have taken place."))
(defgeneric patch-core-pipeline-reginstances (core)
  (:documentation
   "Make sure that pipeline register modifications, as channeled through
their respective register instances, go to the proper backing store, so
they take effect upon transition to :FREE state.
The default primary method does nothing.")
  (:method ((o core))))
(defgeneric core-pipeline-addresses (core &optional cached)
  (:documentation
   "Obtain a list of addresses of instructions currently on CORE's pipeline,
in most-recent-first order.  If CACHED is non-NIL, return the cached values,
instead of going to the device.  Note, that this function isn't obliged
to return an atomic picture of pipeline when CORE is running."))
(defgeneric print-pipeline (core stream))
(defgeneric print-pipeline-terse (core stream))

;;; assets
(defgeneric gpr (core gpr))
(defgeneric set-gpr (core i val))
(defsetf gpr set-gpr)
(defgeneric gpr-by-name (core gpr))
(defgeneric set-gpr-by-name (core gpr value))
(defsetf gpr-by-name set-gpr-by-name)
(defgeneric tlb-entry (core i))
(defgeneric set-tlb-entry (core i val))
(defsetf tlb-entry set-tlb-entry)
(defgeneric decode-tlb-entry (entry))
(defgeneric probe-tlb (core asid vpn))
(defgeneric get-tlb (core))
(defgeneric set-tlb (core new-tlb))
(defsetf get-tlb set-tlb)
(defgeneric tlb-address-map (core tlb page-size))

;;; state
(defgeneric core-running-p (core))
(defgeneric (setf core-running-p) (run-p core))
(defgeneric step-core-asynchronous (core))
(defgeneric step-core-debug (core))
(defgeneric reset-platform (core &key &allow-other-keys)
  (:documentation
   "The default primary method does nothing.  The :AROUND method acts as following:
   - calls INTERFACE-RESET-TARGET on the target,
   - then calls RESET-CORE on all of the GENERAL-PURPOSE-CORES associated with the target,
   - yields to the next method,
   - calls CONFIGURE-TARGET-PLATFORM."))
(defgeneric reset-core (core)
  (:documentation
   "The backend is required to provide a primary method."))
(defgeneric analyse-core (core)
  (:documentation
   "The backend is required to provide a primary method, which would collect
necessary forensics information for DEDUCE-STOP-REASON to succeed."))
(defgeneric deduce-stop-reason (core)
  (:documentation
   "The backend is required to provide a primary method, which is expected to return
an object of type CORE-STOP-REASON."))
(defgeneric poll-core-interruptible (core &optional watch-fn watch-period iteration-period iteration-limit)
  (:documentation
   "The provided default primary method polls CORE until either CORE-RUNNING-P returns NIL, 
with ITERATION-PERIOD granularity, or user requests termination.  WATCH-FN is executed
every ITERATION-PERIOD * WATCH-PERIOD nanoseconds.
ITERATION-LIMIT optionally soft-limits the time spent sleeping."))
(defgeneric wait-core (core &optional watch-fn watch-period iteration-period iteration-limit)
  (:documentation
   "The provided default primary method calls POLL-CORE-INTERRUPTIBLE, and does ANALYSE-CORE,
unless there was a timeout."))
(defgeneric core-enter-debug (core)
  (:documentation
   "Manipulate the state machinery of CORE so that it can be resumed later.
Different backends have different requirements on what exact state the core
needs to be put in to guarantee non-intervention, and these requirements
might vary depending on situation."))

(defgeneric free-to-stop (core &key &allow-other-keys)
  (:method ((o core) &key &allow-other-keys) t)
  (:method :after ((o core) &key &allow-other-keys)
    (analyse-core o)))
(defgeneric stop-to-debug (core &key &allow-other-keys)
  (:method ((o core) &key &allow-other-keys) t))
(defgeneric debug-to-stop (core &key &allow-other-keys)
  (:method ((o core) &key &allow-other-keys) t))
(defgeneric stop-to-free (core &key &allow-other-keys)
  (:method ((o core) &key &allow-other-keys) t))

;;; slaves
(defgeneric core-slaves (core))
(defgeneric freeze-core-slaves (core) (:method ((o core))))
(defgeneric thaw-core-slaves (core) (:method ((o core))))

;;; core frequency
(defgeneric core-frequency-multiplier (core))
(defgeneric core-frequency-multiplier-valid-p (core frequency-multiplier) (:method ((o core) mult) (and (integerp mult) (> mult 1))))
(defgeneric default-core-frequency-multiplier (core)                      (:method ((o core)) 2))

;;; caches
(defgeneric flush-core-instruction-cache (core))
(defgeneric flush-core-data-cache (core))

;;; instruction counters
(defgeneric capture-instruction-counters (core))
(defgeneric restore-instruction-counters (core))
(defgeneric reset-instruction-counters (core))

;;; trans calls
(defgeneric prepare-trans-args (core stack-top args))
(defgeneric trans-funcall (core cenv address-space function-name args &key &allow-other-keys))
(defgeneric watch-core (core cenv stack-top))

;;; misc
(defgeneric core-call-stack (core))

;;; traps
(defgeneric set-core-insn-execution-limit (core ninsns)
  (:documentation
   "Arrange things, so that CORE stops after executing NINSNS instructions."))
(defgeneric enable-trap (controlled-trap)
  (:documentation
   "Make CONTROLLED-TRAP effective."))
(defgeneric disable-trap (controlled-trap)
  (:documentation
   "Make CONTROLLED-TRAP ineffective, without dealing away with it."))
(defgeneric recognise-sw-breakpoint (core address)
  (:documentation
   "Register a software breakpoint, which appeared for reasons external
to COMMON-DB, e.g. user manually inserting a trap instruction in source code."))
(defgeneric add-sw-breakpoint (core address)
  (:documentation
   "Register a new software breakpoint at ADDRESS."))
(defgeneric setup-hw-trap (breakpoint address skip-count &key &allow-other-keys)
  (:documentation
   "Reconfigure the hardware BREAKPOINT, so that it triggers after SKIP-COUNT
references to ADDRESS, with the nature of reference dependent on the specific
type of BREAKPOINT.  The type of BREAKPOINT also governs which additional
keywords might be specified."))
(defgeneric add-hw-breakpoint (core address &optional skip-count)
  (:documentation
   "Allocate a hardware from a free pool, and configure it, by the means of
SETUP-HW-TRAP.  The breakpoint returned is meant to be released, once
it is no longer used, by calling DISABLE-TRAP."))
(defgeneric add-cell-watchpoint (core address &optional skip-count)
  (:documentation
   "Allocate a hardware from a free pool, and configure it, by the means of
SETUP-HW-TRAP.  The breakpoint returned is meant to be released, once
it is no longer used, by calling DISABLE-TRAP."))
(defgeneric coerce-to-trap (trap-specifier)
  (:documentation
   "Given a TRAP-SPECIFIER, that is, either a trap number, or a trap,
return the corresponding trap."))

;;;;
;;;; Moments of core, and trails thereof.
;;;;
(defclass moment ()
  ((fetch :accessor moment-fetch :type (integer 0) :initarg :fetch)
   (opcode :accessor moment-opcode :type (integer 0) :initarg :opcode)))

(define-print-object-method ((o moment) fetch opcode)
    "PC ~8,'0X, opcode ~8,'0X" fetch opcode)

(defun make-moment (type fetch opcode &rest moment-args &key &allow-other-keys)
  (declare (type (integer 0) fetch opcode))
  (apply #'make-instance type :fetch fetch :opcode opcode moment-args))

(defmethod derive-moment ((m moment) (address integer))
  (make-moment 'moment address (moment-opcode m)))

(define-protocol-class trail () ())

;;;;
;;;; Subclasses
;;;;
(define-protocol-device-class general-purpose-core :core (master-device core)
  ((moment :accessor saved-core-moment :initarg :moment)
   (moment-changed-p :accessor core-moment-changed-p :initarg :moment-changed-p)
   (trail :accessor saved-core-trail :initarg :trail)
   (trail-important-p :accessor core-trail-important-p :initarg :trail-important-p)
   (machine :accessor core-machine :initarg :machine)
   (executable :accessor core-executable :initarg :executable))
  (:documentation
   "Cores which define GPR/(SETF GPR)")
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

;;;;
;;;; Printing
;;;;
(define-print-object-method ((o core) id backend)
    "~A backend: ~A" id (with-output-to-string (string) (print-device-object backend string)))

(define-print-object-method ((o general-purpose-core) id backend moment trail)
    "~A backend: ~A ~_moment: ~A ~_trail: ~A"
  id (with-output-to-string (string) (print-device-object backend string)) moment trail)

;;;;
;;;; Conditions
;;;;
(define-condition core-condition ()
  ((core :accessor condition-core :initarg :core)))
(define-condition core-error (error core-condition) ())
(define-reported-condition core-halt-failure (core-error) () 
  (:report (core)
           "~@<Failed to halt core ~S~:@>" core))
(define-reported-condition no-core-breakpoint (core-error)
  ((address :reader condition-address :initarg :address))
  (:report (core address)
           "~@<Unknown trap at address ~X for core ~A~:@>" address core))
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
;;;; State management
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
;;; *** STATE ***

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

(define-protocol-class hardware-trap (address-trap)
  ((owned :accessor trap-owned-p :initarg :owned))
  (:default-initargs
   :owned nil))

(define-protocol-class volatile-address-trap (address-trap) ())

;;;;
;;;;    The Grand Scheme of Things.
;;;;
;;; +---------------+---------------------+
;;; | Legend:       | cGRE Protocol class |
;;; +---------------+---------------------+
;;;
;;;  +-----------------------+  +-------------------+
;;;  | cGRE core-stop-reason +->| user-interruption |
;;;  +--------+--------------+  +-------------------+
;;;           |
;;;           V
;;;     +-----------+      +----------------+
;;;     | cGRE trap +----->| intercore-trap |
;;;     +-----+-----+      +----------------+
;;;           |
;;;           |                     +------------------------+
;;;           |        /----------->| instruction-count-trap |
;;;           |        |            +------------------------+
;;;           V        |
;;;  +-----------------+----+       +---------------------+
;;;  | cGRE controlled-trap +------>| cGRE skippable-trap |
;;;  +--------+--------+----+       +---------------------+
;;;           |        |
;;;           |        |            +----------------------+
;;;           |        \----------->| cGRE enumerated-trap |
;;;           |                     +----------------------+
;;;           |
;;;           |                     +-------------+
;;;           |      /------------->| vector-trap |
;;;           |      |              +-------------+
;;;           V      |
;;; +----------------+--+           +--------------------+
;;; | cGRE address-trap +---------->| memory-access-trap |
;;; +---------+------+--+           +--------------------+
;;;           |      |
;;;           |      |              +------------+
;;;           |      \------------->| breakpoint |
;;;           |                     +------+-----+
;;;           V                            |
;;; +----------------------------+    /----+------------\
;;; | cGRE volatile-address-trap |    |                 |
;;; +---------+------------------+    |                 V
;;;           |                       |      +----------+----------+
;;;           \------------------+----/      | hardware-breakpoint |
;;;                              |           +---------------------+
;;;                              V
;;;                   +---------------------+
;;;                   | software-breakpoint |
;;;                   +---------------------+
;;;

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
   :address #x0
   :skipcount 1))

(defclass software-breakpoint (volatile-address-trap breakpoint)
  ((saved-insn :accessor software-breakpoint-saved-insn :initform 0 :initarg :saved-insn :type (unsigned-byte 32))))

;;;;
;;;; Core trap containers
;;;;
(define-subcontainer traps :container-slot traps :type list :iterator do-core-traps :remover remove-trap :if-exists :continue :if-does-not-exist :continue
                     :iterator-bind-key t)
(define-subcontainer hwbreak :container-slot hw-breakpoints :type hardware-trap :iterator do-core-hardware-breakpoints :if-exists :error)

(defmacro do-core-controlled-traps ((o core) &body body)
  (with-gensyms (traplist)
    `(do-core-traps (nil ,traplist ,core)
       (dolist (,o ,traplist)
         (when (typep ,o 'controlled-trap) ,@body)))))
(defmacro do-core-vector-traps ((o core) &body body)
  (with-gensyms (traplist)
    `(do-core-traps (nil ,traplist ,core)
       (dolist (,o ,traplist)
         (when (typep ,o 'vector-trap) ,@body)))))
(defmacro do-core-software-breakpoints ((o core) &body body)
  (with-gensyms (traplist)
    `(do-core-traps (nil ,traplist ,core)
       (dolist (,o ,traplist)
         (when (typep ,o 'software-breakpoint) ,@body)))))

(define-print-object-method ((o trap)) "")
(define-print-object-method ((o intercore-trap) trapping-core)
    " trapping core: ~S" trapping-core)
(define-print-object-method ((o controlled-trap) enabled)
    " ~:[dis~;en~]abled" enabled)
(define-print-object-method ((o address-trap) enabled address)
    " ~:[dis~;en~]abled, address: 0x~X" enabled address)
(define-print-object-method ((b hardware-trap) address trigcount id)
    "~8,'0X trigcount: ~D id: ~D" address trigcount id)
(define-print-object-method ((b software-breakpoint) address trigcount saved-insn)
    "~8,'0X insn: 0x~8,'0X" address saved-insn)

(defmethod enable-trap  :after ((o controlled-trap))       (setf (slot-value o 'enabled) t))
(defmethod disable-trap :after ((o controlled-trap))       (setf (slot-value o 'enabled) nil))
(defmethod disable-trap :after ((o hardware-trap))         (setf (slot-value o 'owned) nil))
(defmethod disable-trap :after ((o volatile-address-trap)) (remove-trap (trap-core o) (trap-address o)))

(defmethod initialize-instance :after ((o address-trap) &key core address &allow-other-keys)
  (when (and core address)
    (push o (traps core address))))

(defmethod coerce-to-trap ((o address-trap))
  o)

(defmethod recognise-sw-breakpoint ((o core) address)
  (lret ((trap (make-instance (core-default-sw-breakpoint-type o) :core o :address address :saved-insn (core-nopcode o))))
    (push trap (traps o address))
    (setf (slot-value trap 'enabled-p) t)))

(defmethod add-sw-breakpoint ((o core) address)
  (lret ((bp (or (find-if (of-type 'software-breakpoint)
                          (traps o address))
                 (make-instance (core-default-sw-breakpoint-type o) :core o :address address))))
    (enable-trap bp)))

(defmethod setup-hw-trap :before ((o hardware-trap) address skipcount &key &allow-other-keys)
  (declare (ignore skipcount))
  (if address
      (push o (traps (trap-core o) address))
      (removef (traps (trap-core o) address) o))
  (setf (trap-address o) address))
(defmethod setup-hw-trap ((b hardware-trap) address skipcount &key &allow-other-keys)
  (declare (ignore address skipcount))
  b)
(defmethod setup-hw-trap :after ((o hardware-trap) address skipcount &key &allow-other-keys)
  (declare (ignore skipcount))
  (if address
      (enable-trap o)
      (disable-trap o)))

(defun allocate-hardware-breakpoint (core &optional (if-no-free-breakpoints :error))
  (or (do-core-hardware-breakpoints (b core)
        (unless (trap-owned-p b)
          (setf (trap-owned-p b) t)
          (return b)))
      (ecase if-no-free-breakpoints
        (:error (error "~@<No free breakpoints.  Used:~{ ~8,'0X~}~:@>"
                       (do-core-hardware-breakpoints (b core)
                         (collect (trap-address b)))))
        (:continue))))

(defmethod add-hw-breakpoint ((core core) address &optional (skipcount 0))
  (setup-hw-trap (allocate-hardware-breakpoint core) address skipcount))

(defun allocate-hardware-breakpoints (core n)
  (iter (repeat n)
        (collect (allocate-hardware-breakpoint core))))

(defun invoke-with-maybe-free-hardware-breakpoints (maybe core fn addresses skipcount
                                                    &rest breakpoint-setup-args &key &allow-other-keys)
  (if maybe
      (let ((breakpoints (allocate-hardware-breakpoints core (length addresses))))
        (unwind-protect (progn
                          (iter (for b in breakpoints)
                                (for a in addresses)
                                (when a
                                  (apply #'setup-hw-trap b a skipcount breakpoint-setup-args)))
                          (apply fn breakpoints))
          (dolist (b breakpoints)
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
;;;; Core protocol implementations
;;;;
(defmethod initialize-instance :after ((o core) &key isa &allow-other-keys)
  "Tie parent links."
  (when isa
    ;; Don't be aggressive: not all cores currently specify an ISA.
    ;; In future, we might actually start to enforce that.
    (setf (slot-value o 'nopcode) (isa-nopcode isa)))
  (do-core-hardware-breakpoints (b o)
    (setf (trap-core b) o))
  (do-core-vector-traps (v o)
    (setf (trap-core v) o)))

(defmethod gpr-by-name ((o core) (gpr-name symbol))
  (gpr o (optype-evaluate (isa-gpr-optype (core-isa o)) gpr-name)))

(defmethod set-gpr-by-name ((o core) (gpr-name symbol) value)
  (set-gpr o (optype-evaluate (isa-gpr-optype (core-isa o)) gpr-name) value))

(defmethod core-slaves ((o general-purpose-core))
  (remove-if-not (of-type 'core) (master-device-slaves o)))

;;;     RESET
(defmethod reset-platform ((o core) &key &allow-other-keys))
(defmethod reset-platform :around ((o core) &rest platform-args &key stop-cores-p &allow-other-keys)
  (let ((target (backend o)))
    (iface:interface-reset-target (backend target) stop-cores-p)
    (mapc #'reset-core (target-devices-by-type target 'general-purpose-core))
    (call-next-method)
    (apply #'configure-target-platform target (target-platform target)
           (remove-from-plist platform-args :stop-cores-p))))

(defmethod reset-core :around ((o general-purpose-core))
  (save-core-pipeline o :trail-important nil)
  (mapc #'reset-core (core-slaves o))
  (call-next-method))

(defmethod reset-core :around ((o core))
  (do-core-traps (nil bs o)
    (mapc #'disable-trap bs))
  (setf (core-stop-reason o) nil)
  (call-next-method))
;;; *** RESET ***

;;;     PIPELINE
(defmethod set-core-moment :after ((o general-purpose-core) moment)
  (declare (ignore moment))
  (setf (core-moment-changed-p o) t
        (saved-core-trail o) (make-neutral-trail o)
        (core-trail-important-p o) nil))

(defmethod (setf saved-core-moment) :after (moment (o general-purpose-core))
  (declare (ignore moment))
  (setf (core-moment-changed-p o) t))

(defmethod (setf saved-core-trail) :after (trail (o general-purpose-core))
  (declare (ignore trail))
  (setf (core-trail-important-p o) t))

(defmethod save-core-pipeline ((o general-purpose-core) &key (trail-important t))
  (setf (saved-core-moment o) (core-moment o)
        (saved-core-trail o) (core-trail o)
        (core-trail-important-p o) trail-important))

(defmethod (setf pc) :before (value (o core))
  "The moment we change PC knowingly, CORE's stop reason ceases to matter."
  (declare (ignore value))
  (setf (core-stop-reason o) nil))
;;; *** PIPELINE ***

;;;     STATE
(defmethod poll-core-interruptible ((core core) &optional (watch-fn #'values) (watch-period 1) (iteration-period 10000000) run-iteration-limit)
  (busywait-interruptible-executing (not (core-running-p core)) (funcall watch-fn core *standard-output*)
                                    :watch-period watch-period :iteration-period iteration-period :run-time run-iteration-limit))

(defmethod analyse-core :before ((o general-purpose-core))
  (save-core-pipeline o))

(defmethod analyse-core :after ((o general-purpose-core))
  (setf (core-stop-reason o) (or (deduce-stop-reason o)
                                 (make-instance 'user-interruption :core o))))

(defmethod wait-core ((o core) &optional (watch-fn #'values) (watch-period 1) (iteration-period 10000000) iteration-limit)
  (case (poll-core-interruptible o watch-fn watch-period iteration-period iteration-limit)
    (:timeout :timeout)
    ((t nil) (setf (state o) :stop))))
;;; *** STATE ***

(defmethod reset-instruction-counters :after ((o core))
  (setf (core-instruction-counter o) 0))

;;;;
;;;; Core protocol -based toolkit
;;;;
(defun step-core-synchronous (core)
  "Perform steps necessary to make CORE do one step, and wait until it stops.
The return value is T, except when execution is interrupted by SIGINT, 
in which case it is NIL."
  (step-core-asynchronous core)
  (busywait-interruptible-executing (not (core-running-p core)) nil))

(defun prime-core-executable (core loadable &key check report checksum dump)
  "Prepare LOADABLE to be executed on a properly configured general-purpose
CORE. When CHECK is non-NIL, an integrity check during upload of LOADABLE
is performed."
  (loadable:upload-loadable (backend core) loadable
                            :check check
                            :section-before-fn (cond (dump #'loadable:dump-section)
                                                     (checksum #'loadable:report-checksummed-section)
                                                     (report #'loadable:report-section)
                                                     (t #'values)))
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
    (funcall watch-fn core *log-stream*)
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
                (error "~@<While executing ~:[~;~:*~S ~]on ~S at #x~8,'0X:~_~
                          ran out of execution time budget of ~D nanoseconds.~_~
                          Core is ~:[not running anymore~;still running~].~:@>"
                       segment core address (* iteration-period iteration-limit)
                       (core-running-p core)))))
            (t
             (setf (state core) (or exit-state old-state)))))))

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
;;;; Trans calls
;;;;
(defvar *trace-trans-calls* nil
  "Whether the trans calls should be traced for correspondence between code in memory
and code actually hitting the target core pipeline.  This is useful for tracking
icache-related anomalies.")
(defvar *disasm-trans-calls* nil
  "Whether to print disassembly of whole trans call zone before doing the call.")

(define-execute-with-bound-variable *trace-trans-calls*
  (:binding traced-trans-calls t :define-with-maybe-macro t))

(defgeneric handle-execution-error (core condition-type condition-args)
  (:method ((o core) condition-type condition-args)
    (error (apply #'make-condition condition-type condition-args))))

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
    #+nil
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
      (mapc #'disable-trap swbreaks))))

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

(defmethod trans-funcall :around ((o core) (cenv compilation-environment) (as address-space) function-name args
                                  &key (disasm *disasm-trans-calls*) (trace *trace-trans-calls*) &allow-other-keys)
  (declare (ignore function-name args trace disasm))
  (with-compilation-environment cenv
    (call-next-method)))

(defun trans-funcall* (core cenv as function-name &rest args)
  (trans-funcall core cenv as function-name args
                 :trace *trace-trans-calls*
                 :disasm *disasm-trans-calls*))

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

(defmethod get-tlb ((o mmu-core))
  (iter (for i below (core-tlb-entries-nr o))
        (collect (tlb-entry o i))))

(defmethod set-tlb ((o mmu-core) new-tlb)
  (iter (for i below (core-tlb-entries-nr o))
        (for entry in new-tlb)
        (set-tlb-entry o i entry)))

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
