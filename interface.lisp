;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: INTERFACE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :interface)


(define-namespace :empty
  (:documentation "Empty space"))

(define-namespace :interface
  (:documentation "OnCD interface"))

(defvar *verbose-interface-init* nil)
(defparameter *log-tap-register-access* nil)
(defvar *virtual-interface-enabled* nil)

(define-condition interface-error (error)
  ((interface :reader condition-interface :initarg :interface)))

(define-simple-error interface-error :object-initarg :interface)

(defmacro with-interface-error-trap-and-return (error-return-form &body body)
  "Execute BODY in context where conditions of type INTERFACE-ERROR are trapped,
and returned as secondary value. The primary value in such cases is specified by
ERROR-RETURN-FORM."
  (with-gensyms (cond)
    `(handler-case (progn ,@body)
       (interface-error (,cond)
         (values ,error-return-form ,cond)))))

(define-condition interface-status-timeout (interface-error)
  ((operation :reader condition-operation :initarg :operation)
   (time :reader condition-time :initarg :time))
  (:report (lambda (condition stream)
             (format stream "~@<Operation ~S on ~S timed out after ~S sec~:@>"
                     (condition-operation condition)
                     (condition-interface condition)
                     (condition-time condition)))))

(define-condition interface-state-transition-timeout (interface-error)
  ((command :reader condition-command :initarg :command)
   (destination-state :reader condition-destination-state :initarg :destination-state))
  (:report (lambda (condition stream)
             (format stream "~@<~S timed out transitioning into state ~S after command ~S~:@>"
                     (condition-interface condition)
                     (condition-destination-state condition)
                     (condition-command condition)))))

(define-condition interface-unknown-target (interface-error)
  ((known-target-alist :reader condition-known-target-alist :initarg :known-target-alist)
   (idcode :reader condition-idcode :initarg :idcode))
  (:report (lambda (condition stream)
             (let ((*print-base* #x10))
               (format stream "~@<Query on ~S~:_  encountered IDCODE ~8,'0X~:_  matching none of the known ones: ~S~:@>"
                       (condition-interface condition)
                       (condition-idcode condition)
                       (condition-known-target-alist condition))))))

(define-condition interface-debug-quiescence-timeout (interface-error) ()
  (:report (lambda (condition stream)
             (format stream "~@<Failed to quiesce ~S into debug state~:@>" (condition-interface condition)))))

(define-reported-condition interface-memory-timeout (interface-error)
  ()
  (:report (interface)
           "~@<Memory access timeout on interface ~S.~:@>" interface))

(define-condition persistent-interface-error (interface-error)
  ((error :reader interface-error-error :initarg :error))
  (:report (lambda (condition stream)
             (format stream "~@<Error persisting after reset on ~S: ~A~:@>" (condition-interface condition) (interface-error-error condition)))))

(defgeneric interface-reset (interface))
(defgeneric interface-target-discrimination-tree (interface))
(defgeneric interface-attach-target (interface))
(defgeneric interface-stop-target (interface))
(defgeneric stop-target-using-interface (target interface))
(defgeneric interface-reset-target (interface stop-cores-p))
(defgeneric reset-target-using-interface (target interface))
(defgeneric interface-bus-word (interface address)) 
(defgeneric (setf interface-bus-word) (value interface address)) 
(defgeneric interface-bus-io (interface buffer address size direction &optional offset))
(defgeneric interface-close (interface))

(define-protocol-device-class interface-bus :empty (bus) ())

(define-protocol-device-class interface :interface (extended-register-device bus-device)
  ((version :accessor iface-version :initarg :version)
   (idcode :accessor iface-idcode :type keyword)
   (name :reader iface-name)
   (targets :accessor iface-targets :initarg :targets)
   (fastio :accessor iface-fastio :initarg :fastio))
  (:default-initargs
   :targets nil
   :fastio t
   :enumeration-class 'interface)
  (:documentation
   "Device representing the means of access to the target's OnChip Debug machinery."))

(defvar *initializing-interface*)

(defmethod initialize-instance :after ((o interface) &key address &allow-other-keys)
  (setf (slot-value o 'name) (format nil "~A-~X" (type-of o) address)
        *initializing-interface* o))

(defmethod print-object ((o interface) stream)
  (labels ((slot (id) (if (slot-boundp o id) (slot-value o id) :unbound-slot)))
    (format stream "~@<#<~;~A version: ~S IDCODE: ~S~;>~:@>"
            (slot 'name) (slot 'version) (slot 'idcode))))

(defmethod interface-close ((o interface)) t)

(defmethod bus-populate-address :around ((o interface-bus) address)
  (declare (ignore address))
  (unwind-protect (handler-bind ((error (lambda (c)
                                          (declare (ignore c))
                                          (interface-close *initializing-interface*))))
                    (call-next-method))
    (makunbound '*initializing-interface*)))

(defun scan-interface-busses (&key force-rescan virtual (physical t) server-address server-port server-type)
  (declare (ignore #+disable-virtcore
                   virtual
                   #+disable-networking #+disable-networking #+disable-networking
                   server-address server-port server-type)
           #-disable-networking
           (type (member nil :tap :rtl) server-type))
  #-disable-virtcore
  (let ((*virtual-interface-enabled* virtual)
        (*virtual-target-enabled* virtual))
    (bus-scan (or (root-bus 'virtif :if-does-not-exist :continue) (make-instance 'virtif-bus :name 'virtif)) force-rescan))
  #-disable-networking
  (when server-address
    (let ((bus-type (ecase server-type
                      (:tap 'tapclient-bus)
                      (:rtl 'client-bus))))
      (bus-scan (or (when-let ((bus (root-bus bus-type :if-does-not-exist :continue)))
                      (reinitialize-instance bus :address server-address :port server-port :type server-type)
                      bus)
                    (make-instance bus-type :name bus-type :address server-address :port server-port :type server-type))
                force-rescan)))
  (when physical
    #-disable-parport
    (unless (arg :disable-parport-interfaces)
      (bus-scan (or (root-bus 'parport :if-does-not-exist :continue) (make-instance 'parport-bus :name 'parport)) force-rescan))
    (unless (arg :disable-usb-interfaces)
      (bus-scan (or (root-bus 'ezusb :if-does-not-exist :continue) (make-instance 'ezusb-bus :name 'ezusb)) force-rescan))))

(defun interfaces ()
  "Return the list of all active interfaces."
  (append #-disable-virtcore
          (bus-devices (root-bus 'virtif))
          #-disable-networking
          (bus-devices (root-bus 'tapclient-bus))
          #-disable-networking
          (bus-devices (root-bus 'client-bus))
          #-disable-parport
          (bus-devices (root-bus 'parport))
          (bus-devices (root-bus 'ezusb))))

