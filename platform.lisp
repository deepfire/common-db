;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PLATFORM; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2008, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2007-2008 by
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

(in-package :platform)


(defvar *log-platform-processing* nil)
(defvar *log-system-configuration* nil)

;;;;
;;;; Conditions
;;;;
(define-condition platform-condition ()
  ((platform :accessor condition-platform :initarg :platform)))
(define-condition platform-error (error platform-condition) ())
(define-simple-error platform-error)

(define-reported-condition platform-essentials-missing (platform-error)
  ((missing-device-types :initarg :missing-device-types))
  (:report (platform missing-device-types)
           "~@<Platform ~S does not define essential devices:~{ ~A~}~%~:@>" platform missing-device-types))

;;;;
;;;; Classes
;;;;
(defclass platform ()
  ((target :accessor platform-target :initarg :target)
   (predefined-devices :accessor platform-predefined-devices :initarg :predefined-devices)
   (memory-map :accessor platform-memory-map :initarg :memory-map)
   (memory-configuration :accessor platform-memory-configuration :initform nil)))

(define-protocol-device-class platform-device nil ()
  ((platform :accessor device-platform :initarg :platform)))
(define-protocol-device-class platform-memory-region nil (platform-device memory-region) ())
(define-protocol-device-class mapped-platform-memory-region nil (platform-memory-region) ())

(defmethod initialize-instance :after ((o mapped-platform-memory-region) &key &allow-other-keys)
  (let* ((extent (memory-region-extent o)))
    (oct-1d:insert (car extent) o (platform-memory-map (device-platform o)))))

(defvar *platforms* (make-hash-table :test 'equal)
  "For prototype-ish registration, as CLASS-PROTOTYPE won't cut it for us.")

(define-root-container *platforms* platform :iterator do-platforms)

(defmethod initialize-instance :before ((o platform) &key &allow-other-keys)
  (unless (platform (class-name (class-of o)) :if-does-not-exist :continue)
    (setf (platform (class-name (class-of o))) o)))

;;;
;;; The PLATFORM's :AFTER method on INITIALIZE-INSTANCE is used by target.lisp,
;;; and is described in terms of system.lisp
;;;

;;;;
;;;; Address regions
;;;;
(defun platform-address-region (platform address)
  (oct-1d:tree-left address (platform-memory-map platform)))

;;;;
;;;; Memory
;;;;
(defgeneric detect-platform-memory-size (platform base &key minimum maximum when-infinite))
(defgeneric configure-platform-system (platform system &key &allow-other-keys))
(defgeneric configure-platform-memory (platform &optional force-detection if-detection-fails)
  (:documentation 
   "Configure PLATFORM's memory, performing detection when there is no stored 
configuration or when FORCE-DETECTION is non-NIL, otherwise using the configuration
stored in PLATFORM.

When no working configuration is found, and IF-DETECTION-FAILS is :ERROR (the default),
an error of type PLATFORM-NO-USABLE-MEMORY-DETECTED-ERROR is signalled.  When it is :WARN,
a warning of type PLATFORM-NO-USABLE-MEMORY-DETECTED is signalled instead."))

;;;;
;;;; Definitions
;;;;
(defun device-type-valid-p (type)
  (and (find-class type nil)
       (device-class type)))

(defun merge-device-specs (primary complement)
  (cond
    ((null complement) primary)         ; nothing to add? go on
    ((null primary) complement)         ; good good stuffs
    (t
     (destructuring-bind (type &rest primary-initargs &key slave (slaves (when slave (list slave))) &allow-other-keys) (first primary)
       (cons (if-let ((match (assoc type complement)))
               (let ((primary-slaves slaves))
                 (destructuring-bind (&rest complement-initargs &key slave (slaves (when slave (list slave))) &allow-other-keys) (rest match)
                   `(,type ,@(remove-from-plist primary-initargs :slave :slaves)
                           ,@(remove-from-plist complement-initargs :slave :slaves)
                           ,@(when (or primary-slaves slaves)
                                   (list :slaves (merge-device-specs primary-slaves slaves))))))
               (first primary))
             (merge-device-specs (rest primary) (remove type complement :count 1 :key #'car)))))))

(defun device-specs-merge-with-plats (primary plat-names)
  "Complement PRIMARY device specs with those provided by platforms
specified by PLAT-NAMES."
  (iter (for name in plat-names)
        (for platform = (platform name))
        (for devices initially primary then
             (merge-device-specs devices (platform-predefined-devices platform)))
        (finally (return devices))))

(defmacro define-platform (name superclasses &body platform-options)
  (let* ((predefined-devices (cdr (assoc :predefined-devices platform-options)))
         (instantiate-p (cdr (assoc :instantiate-p platform-options)))
         (superclasses (or superclasses '(platform)))
         (relevant-supers (remove-if-not-subtype-of 'platform (remove 'platform superclasses)))
         (user-specified-default-initargs (cdr (assoc :default-initargs platform-options))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (when-let ((unknowns (iter (for sym in ',(remove-duplicates
                                                 (mapcar #'car predefined-devices)))
                                  (unless (eq :external (nth-value 1 (find-symbol (symbol-name sym) (symbol-package sym))))
                                    (collect sym)))))
         (platform-error "~@<In definition of platform ~S, following device type symbols are not exported: ~S~:@>" ',name unknowns))
       (when-let ((unknowns (remove-if #'device-type-valid-p
                                       ',(remove-duplicates
                                          (mapcar #'car predefined-devices)))))
         (platform-error "~@<In definition of platform ~S, following device types are unknown: ~S~:@>" ',name unknowns))
       (defclass ,name ,superclasses
         ((predefined-devices :allocation :class))
         ,@(remove-if (rcurry #'member '(:predefined-devices :instantiate-p :default-initargs)) platform-options :key #'car)
         (:default-initargs
          :predefined-devices
          (device-specs-merge-with-plats
           (list ,@(mapcar (lambda (dev) (list* 'list `',(first dev) (rest dev)))
                           predefined-devices))
           ',relevant-supers)
          ,@user-specified-default-initargs))
       ,@(when instantiate-p
          `((make-instance ',name))))))

(defun all-platform-classes ()
  (flatten (rest (maptree #'identity (complement #'class-direct-subclasses) #'class-direct-subclasses
                          (find-class 'platform)
                          :collect-non-atoms t))))