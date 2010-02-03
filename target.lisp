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

(in-package :target)


(define-namespace :target
  (:documentation "TAP target"))

(defvar *forced-platform* nil)
(defvar *keep-target-intact* nil)
(defvar *force-memory-detection* nil)
(defvar *inhibit-memory-detection* nil)

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
(define-protocol-device-class target :target (memory-device)
  ((backend :type interface)
   (platform :accessor target-platform :initarg :platform)
   (enumpool :accessor target-enumpool :initarg :enumpool)
   (mapped-artifact-map :accessor target-mapped-artifact-map :initarg :mapped-artifact-map))
  (:default-initargs
   :enumpool (make-reginstance-enumeration-pool)
   :memory-configuration nil))

(define-protocol-device-class 32bit-bus-target :target (32bit-memory-device target) ())
(define-protocol-device-class 64bit-bus-target :target (32bit-memory-device target) ())

(defmethod print-object ((o target) stream)
  (format stream "~@<#<~;~A:~A platform: ~S devices: ~S backend: ~S~;>~:@>"
          (type-of o) (slot-value* o 'id) (slot-value* o 'platform) (mapcar #'type-of (target-devices o)) (slot-value* o 'backend)))

;;;
;;; Identification
;;;
(defvar *target-discrimination* nil)

(defun find-target-class-for-interface (interface &optional (if-does-not-exist :error))
  (or (discriminate *target-discrimination* interface)
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

(defun target-reg (target name)
  "Read a register from unified namespace of registers belonging
to devices registered within TARGET."
  (reginstance-value (register-instance (target-enumpool target) name)))

(defun set-target-reg (target name value)
  "Write a register within unified namespace of registers belonging
to devices registered within TARGET."
  (set-reginstance-value (register-instance (target-enumpool target) name) value))

(defsetf target-reg set-target-reg)

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
  (let ((device (apply #'make-target-device target type (remove-from-plist device-initargs :slave :slaves))))
    (iter (for (slave-type . slave-args) in slaves)
          (apply #'create-target-device-from-spec target slave-type :master device slave-args))))

(defgeneric configure-target-platform (target platform &key &allow-other-keys)
  (:method ((tg target) (p platform) &key
            prereset-core-multiplier core-multiplier
            (inhibit-memory-detection *inhibit-memory-detection*)
            (force-memory-detection *force-memory-detection*)
            (error-on-failed-detection t))
    (configure-platform-system p (target-device tg '(system 0))
                               :prereset-core-multiplier prereset-core-multiplier
                               :core-multiplier core-multiplier)
    (if inhibit-memory-detection
        (unless (find #x0 (target-devices-by-type tg 'ram) :key (compose #'car #'memory-region-extent))
          (make-target-device tg 'ram
                              :extent (extent 0 67108864) ; yay hardcoded values!
                              :master (target-device tg '(general-purpose-core 0))))
        (configure-platform-memory p force-memory-detection (if error-on-failed-detection
                                                                :error
                                                                :warn)))))

;;;
;;; The :AFTER INITIALIZE-INSTANCE method for PLATFORM only can be described in terms of system.lisp
;;;
(defmethod initialize-instance :after ((o target) &rest platform-initargs &key backend &allow-other-keys)
  (check-type backend interface)
  (add-target-device o backend)
  (push o (iface-targets backend))
  (let ((platform-type (or (prog1 *forced-platform*
                             (unless *keep-target-intact*
                               (interface-reset (backend o))
                               (interface-reset-target (backend o) t)))
                           (detect-target-platform o))))
    (when *log-platform-processing*
      (format *log-stream* "~@<NOTE: ~@;initializing ~S with ~S.~:@>~%" o platform-type))
    (setf (target-platform o) (make-instance platform-type :target o))
    (apply #'configure-target-platform o (target-platform o) (remove-from-plist platform-initargs :backend))))

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
;;;; Memory device
;;;;
(defmethod memory-device-32bit-ref ((o 32bit-bus-target) address)
  (interface-bus-word (backend o) (fixmap-address o address)))
(defmethod memory-device-32bit-set ((o 32bit-bus-target) address val)
  (setf (interface-bus-word (backend o) (fixmap-address o address)) val))

(defun merge-u8-extremity-32 (interface vector base length headp writep)
  (declare (type (vector (unsigned-byte 8)) vector))
  (let ((extremity (if headp base (+ base length))))
    (with-alignment (granule-base left right mask) 4 extremity
      (unless (= granule-base extremity)
        (let* ((exvec (make-array 4 :element-type '(unsigned-byte 8))))
          (setc (u8-vector-word32le exvec 0) (interface-bus-word interface granule-base))
          (operate-on-extremity length headp left right
                                (if writep
                                    (lambda (g i) (setf (aref exvec g) (aref vector i)))
                                    (lambda (g i) (setf (aref vector i) (aref exvec g)))))
          (when writep
            (setc (interface-bus-word interface granule-base) (u8-vector-word32le exvec 0))))))))

(defun bus-extent (vector physaddr size dir tgt &aux (interface (backend tgt)))
  (merge-u8-extremity-32 interface vector physaddr size t (eq dir :write))
  (with-alignment (nil nil head) 4 physaddr
    (with-alignment (nil tail) 4 (+ physaddr size)
      (interface-bus-io interface vector (+ physaddr head) (- size head tail) dir head)))
  (merge-u8-extremity-32 interface vector physaddr size nil (eq dir :write)))

(defmethod read-block ((o 32bit-bus-target) base vector &optional start end)
  (bus-extent vector (fixmap-address o base) (- end start) :read o))

(defmethod write-block ((o 32bit-bus-target) base vector &optional start end)
  (bus-extent vector (fixmap-address o base) (- end start) :write o))

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
