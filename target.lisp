;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TARGET; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :tgt)


(define-namespace :target
  (:documentation "TAP target"))

;;;
;;; Conditions
;;;
(define-condition target-condition ()
  ((target :accessor condition-target :initarg :target)))

(define-condition target-error (error target-condition) ())
(define-simple-error target-error :object-initarg :target)

(define-reported-condition unknown-target-device (target-condition)
  ((device-spec :accessor condition-device-spec :initarg :device-spec))
  (:report (target device-spec)
           "~@<~S has no device ~S.~:@>" target device-spec))

(define-reported-condition target-platform-detection-error (target-error) ()
  (:report (target) "~@<Unable to detect platform of ~S.~:@>" target))

;;;
;;; Target
;;;
(define-protocol-device-class target :target (bioable-memory-device)
  ((backend :type interface)
   (platform :accessor target-platform :initarg :platform)
   (enumpool :accessor target-enumpool :initarg :enumpool)
   (mapped-artifact-map :accessor target-mapped-artifact-map :initarg :mapped-artifact-map))
  (:default-initargs
   :enumpool (make-reginstance-enumeration-pool)
   :memory-configuration nil))

(define-protocol-device-class 32bit-bus-target :target (32bit-memory-device target) ())
(define-protocol-device-class 64bit-bus-target :target (64bit-memory-device target) ())
(define-protocol-device-class little-endian-target :target (little-endian-memory-device target) ())
(define-protocol-device-class big-endian-target :target (big-endian-memory-device target) ())

(defmethod print-object ((o target) stream)
  (format stream "~@<#<~;~A:~A platform: ~S devices: ~S backend: ~S~;>~:@>"
          (type-of o) (slot-value* o 'id) (slot-value* o 'platform) (mapcar #'type-of (target-devices o)) (slot-value* o 'backend)))

;;;
;;; Identification
;;;
(defgeneric target-platform-discrimination-tree (target))

(defun find-target-class-for-interface (interface &optional (if-does-not-exist :error))
  (or (discriminate (interface-target-discrimination-tree interface) interface)
      (ecase if-does-not-exist
        (:error
         (interface-close interface)
         (target-error nil "~@<Unable to find a suitable target type for target version ~S at ~S.~:@>"
                       (iface-idcode interface) interface))
        (:continue))))

(defgeneric detect-target-platform (target &optional if-does-not-exist)
  (:method :around ((o target) &optional (if-does-not-exist :error))
   (or (call-next-method)
       (ecase if-does-not-exist
         (:error (error 'target-platform-detection-error :target o))
         (:continue)))))

;;;;
;;;; Target device enumeration, and its fruit
;;;;
(defgeneric add-target-device (target device)
  (:method ((targ target) (d device))
    (enumerate-device (target-enumpool targ) d)
    (let ((ris (create-device-register-instances d)))
      (when (typep d 'mapped-device)
        (dolist (ri ris)
          (sym:add (mapped-device-register-address d (name (reginstance-register ri))) ri (target-mapped-artifact-map targ)))))))

(defgeneric remove-target-device (target device)
  (:method ((targ target) (d device))
    (let ((enumpool (target-enumpool targ)))
      (purge-device-register-instances d)
      (enumpool-remove enumpool (enumclass enumpool (device-enumeration-class d)) d))))

(defun target-reginstance (target name)
  "Find a NAMEd register instance within the unified namespace
of registers belonging to devices registered within TARGET."
  (register-instance (target-enumpool target) name))

(defun target-reginstance-by-id (target id)
  "Find a register instance by its TARGET-local ID."
  (register-instance-by-id (target-enumpool target) id))

(defun target-reg-addr (target name)
  "Return address of the memory-mapped register from unified namespace of registers belonging
to devices registered within TARGET."
  (let ((ri (target-reginstance target name)))
    (unless (mapped-device-p (reginstance-device ri))
      (error "Register ~A does not belong to a memory-mapped device." name))
    (mapped-reginstance-address ri)))

(defun target-reg (target name)
  "Read a register from unified namespace of registers belonging
to devices registered within TARGET."
  (reginstance-value (target-reginstance target name)))

(defun set-target-reg (target name value)
  "Write a register within unified namespace of registers belonging
to devices registered within TARGET."
  (set-reginstance-value (target-reginstance target name) value))

(defsetf target-reg set-target-reg)

(defun target-compile-raw-register-value (target name bitfield-names bitfield-values &aux
                                          (ri (register-instance (target-enumpool target) name))
                                          (device (reginstance-device ri)))
  (values (mapped-device-register-address device (name (reginstance-register ri)))
          (fbits bitfield-names bitfield-values)))

(defun target-decompile-raw-register-value (target address raw-value &aux
                                            (address (fixmap-address target address))
                                            (artifact (target-artifact-by-address target address)))
  (if (typep artifact 'register-instance)
      (let* ((reg (reginstance-register artifact))
             (bitfield-values (decode-using-format (reg-format reg) raw-value)))
        (values (name reg)
                (nreverse (mapcar #'car bitfield-values))
                (nreverse (mapcar #'cdr bitfield-values))))
      (error "~@<Address ~X contains ~S, which is not a device register.~:@>" address artifact)))

;;;;
;;;; Target device creation
;;;;
(defun make-target-device (target type &rest initargs)
  "Create an instance of device TYPE, by providing it INITARGS,
and attaching it to the TARGET."
  (lret ((device (apply #'make-instance type :backend target :platform (target-platform target) initargs)))
    (add-target-device target device)))

(defun create-target-device-from-spec (target type &rest device-initargs &key slave (slaves (when slave (list slave)))
                                       &allow-other-keys)
  "Create a device of TYPE in TARGET, with regard to DEVICE-INITARGS and
potential SLAVES."
  (lret ((device (apply #'make-target-device target type (remove-from-plist device-initargs :slave :slaves))))
    (when-let ((slaves (iter (for (slave-type . slave-args) in slaves)
                             (collect (apply #'create-target-device-from-spec target slave-type :master device slave-args)))))
      (when (typep device 'master-device)
        (setf (master-device-slaves device) slaves)))))

(defgeneric configure-target-platform (target platform &key inhibit-memory-configuration inhibit-memory-detection memory-config
                                              force-memory-detection memory-detection-threshold memory-configuration-failure-error-p
                                              prereset-core-multiplier core-multiplier &allow-other-keys)
  (:documentation
   "Keyword options:
   command-line-available:   INHIBIT-MEMORY-CONFIGURATION, MEMORY-CONFIGURATION-FAILURE-ERROR-P
   command-line-unavailable: FORCE-MEMORY-DETECTION")
  (:method ((tg target) (p platform) &key inhibit-memory-configuration inhibit-memory-detection memory-config
            force-memory-detection (memory-detection-threshold 1024) (memory-configuration-failure-error-p t)
             prereset-core-multiplier core-multiplier)
    (configure-platform-system p (target-device tg '(system 0))
                               :prereset-core-multiplier prereset-core-multiplier
                               :core-multiplier core-multiplier)
    (if inhibit-memory-configuration
        (progn
          (syncformat *log-stream* "WARNING: memory configuration disabled!~%")
          (unless (find #x0 (target-devices-by-type tg 'ram) :key (compose #'car #'memory-region-extent))
            (make-target-device tg 'ram
                                :extent (extent 0 67108864) ; yay hardcoded values!
                                :master (target-device tg '(general-purpose-core 0)))))
        (configure-platform-memory p
                                   (cond (inhibit-memory-detection :inhibit)
                                         (force-memory-detection   :force)
                                         (t                        :allow))
                                   memory-config memory-detection-threshold memory-configuration-failure-error-p))))

;;;
;;; The :AFTER INITIALIZE-INSTANCE method for PLATFORM only can be described in terms of system.lisp
;;;
(defmethod initialize-instance :after ((o target) &rest platform-initargs &key backend keep-target-intact forced-platform skip-platform-init &allow-other-keys)
  (check-type backend interface)
  (add-target-device o backend)
  (push o (iface-targets backend))
  (let ((platform-type (or (prog1 forced-platform
                             (unless keep-target-intact
                               (interface-reset (backend o))
                               (interface-reset-target (backend o) t)))
                           (detect-target-platform o))))
    (when *log-platform-processing*
      (syncformat *log-stream* "~@<NOTE: ~@;initializing ~S with ~S.~:@>~%" o platform-type))
    (setf (target-platform o) (make-instance platform-type :target o))
    (if skip-platform-init
        (syncformat *log-stream* "WARNING: platform initialisation disabled!~%")
        (apply #'configure-target-platform o (target-platform o)
               (remove-from-plist platform-initargs :backend :keep-target-intact :skip-platform-init :forced-platform)))))

;;;;
;;;; Queries
;;;;
(defun target-devices (target)
  (remove-duplicates
   (iter (for (nil enumclass) in-hashtable (devmodel::enumpool-root (target-enumpool target)))
         (appending (hash-table-values (devmodel::enumclass-root enumclass))))
   :test #'eq))

(defun target-devices-by-type (target type)
  "Find TARGET devices by TYPE."
  (declare (type target target) (type symbol type))
  (remove-duplicates (map-enumpool-type (target-enumpool target) type #'identity)
                     :test #'eq))

(defun target-device (target name &optional (if-does-not-exist :error))
  "Find a TARGET's device by NAME, which is a list of type name and device id."
  (declare (type target target) (type (cons symbol (cons (integer 0) null)) name))
  (or (when-let ((devices (target-devices-by-type target (car name))))
        (find (cadr name) devices :key #'enumerated-id))
      (ecase if-does-not-exist
        (:continue)
        (:error (error 'unknown-target-device :target target :device-spec name)))))

(defun target-artifact-by-address (target address)
  (multiple-value-bind (artifact real-address) (sym:name (target-mapped-artifact-map target) address)
    (when (= address real-address)
      artifact)))

(defmacro with-target-devices (target bindings &body body)
  (once-only (target)
    `(let ,(loop :for (var devname) :in bindings :collect
              `(,var (target-device ,target ,devname)))
       ,@body)))

;;;;
;;;; Address space
;;;;
(defgeneric fixmap-address (target address))

;;;;
;;;; Execution
;;;;
(defgeneric exec-raw (target insn &optional address))

;;;;
;;;; Memory device & bioable
;;;;
(defmethod memory-device-32bit-ref ((o 32bit-bus-target) address)
  (interface-bus-word (backend o) (fixmap-address o address)))
(defmethod memory-device-32bit-set ((o 32bit-bus-target) address val)
  (setf (interface-bus-word (backend o) (fixmap-address o address)) val))

(defmethod read-aligned-block ((o 32bit-bus-target) address vector offset length)
  (interface-bus-io (backend o) vector address length :read offset))

(defmethod write-aligned-block ((o 32bit-bus-target) address vector offset length)
  (interface-bus-io (backend o) vector address length :write offset))

;;; XXX: inefficient
(defmethod read-block ((o 32bit-bus-target) base vector &optional start end)
  (declare (type (integer 0) base))
  (if (or start end)
      (let* ((iolen (- end start))
             (iovec (make-array iolen :element-type '(unsigned-byte 8))))
        (bioable-memory-io o (fixmap-address o base) iovec iolen nil)
        (setf (subseq vector start end) iovec))
      (bioable-memory-io o (fixmap-address o base) vector (length vector) nil)))

;;; XXX: inefficient
(defmethod write-block ((o 32bit-bus-target) base vector &optional start end)
  (declare (type (integer 0) base))
  (if (or start end)
      (bioable-memory-io o (fixmap-address o base) (subseq vector start end) (- end start) t)
      (bioable-memory-io o (fixmap-address o base) vector (length vector) t)))

#+sbcl
(define-device-class busmem :target (sequence device)
  ((max-index :accessor busmem-max-index :type (unsigned-byte 32) :initarg :max-index)))

#+sbcl
(defmethod sequence:length ((o busmem))
  (1+ (busmem-max-index o)))

#+sbcl
(defmethod sequence:elt ((o busmem) index)
  (memory-ref (backend o) (ash index 2)))

#+sbcl
(defmethod (setf sequence:elt) (new-value (o busmem) index)
  (setf (memory-ref (backend o) (ash index 2)) new-value))

#+sbcl
(defmethod sequence:make-sequence-like ((o busmem) length &rest keys)
  (declare (ignorable o))
  (apply #'make-array length keys))
