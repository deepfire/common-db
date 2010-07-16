;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BUS; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :bus)


(defvar *log-bus-events* nil)

(defvar *root-busses* (make-hash-table))

(define-device-class bus nil ()
  ((name :accessor bus-name :type symbol :initarg :name)
   (devices :accessor %bus-devices :type hash-table :initarg :devices))
  (:default-initargs 
   :devices (make-hash-table)))

(define-device-class root-bus nil (bus) ())

(define-device-class enumerating-bus nil (bus) ())
(define-device-class probe-discovery-bus nil (bus)
  ((probe-address-set :accessor bus-probe-address-set :type list :initarg :probe-address-set)
   (probe-time-bus-ids :accessor bus-probe-time-bus-ids :type hash-table)))

(define-device-class bus-device nil ()
  ((bus :accessor device-bus :type bus :initarg :bus)
   (bus-id :accessor device-bus-id :initarg :bus-id :documentation "Something vaguely representing the device type, like a PCI ID.")
   (address :accessor device-bus-address :type (integer 0) :initarg :address :documentation "Unique distinguishing address of device on a bus."))
  (:default-initargs
   :bus-id nil))

(define-device-class child-bus nil (bus bus-device)
  ())

;; Desires:
;;    - ability to detect changes of bus configuration
;;        whether or not a certain address still contains the same device as before, to distinguish newly-appeared devices
;;           this implies some sort of false-negative-less device identity mechanism
;;              we do potentially disruptive initialisation on negatives, that is when we have a "nay, different device"?
;;           when we do such checks?
;;              when the user means "please rescan devices", which, in most cases is when he just attached a new device
;;           however, user controls actions, so harmful inits won't happen without his direct will
;;              how direct is "direct"?
;;    - represent both smart busses, able to enumerate attached devices, and probe-style busses

(define-root-container *root-busses* root-bus :if-exists :error)
(define-subcontainer bus-device :container-slot devices :if-exists :error :remover t :mapper t :iterator t)

(define-print-object-method ((o bus) devices)
    "~@<devices: ~S~:@>" devices)

(define-condition bus-error (error)
  ((bus :accessor condition-bus :initarg :bus)))

(define-simple-error bus-error :object-initarg :bus)

(define-reported-condition bus-addition-error (bus-error)
  ((device :accessor condition-device :initarg :device)
   (condition :accessor condition-condition :initarg :condition))
  (:report (device bus condition) "~@<Failed to add ~A to ~A. Encountered condition: ~A~:@>" device bus condition))

(defgeneric bus-probe-address (bus address)
  (:documentation 
   "Determine whether BUS has a device attached at ADDRESS.
The secondary value, when non-NIL, represents a bus ID."))

(defgeneric bus-id-at-address (bus address)
  (:documentation
   "Retrieve the bus ID of a device residing on BUS at ADDRESS.")
  (:method ((bus bus) device)
    (declare (ignore device))
    t)
  (:method ((o probe-discovery-bus) address)
    (gethash address (bus-probe-time-bus-ids o))))

(defgeneric bus-id-equalp (bus id1 id2)
  (:documentation
   "Determine whether ID1 and ID2 are sufficiently same, 
so as not to require a device reinitialisation.")
  (:method ((bus bus) id1 id2)
    (declare (ignore id1 id2))
    t))

(defgeneric bus-occupied-addresses (bus)
  (:documentation
   "Query a smart BUS for addresses occupied by attached devices,
or do probing for the same information in dumb bus case.")
  (:method ((o probe-discovery-bus))
    (iter (for addr in (bus-probe-address-set o))
          (multiple-value-bind (present-p maybe-id) (bus-probe-address o addr)
            (setf (gethash addr (bus-probe-time-bus-ids o)) maybe-id)
            (when present-p
              (collect addr))))))

(defgeneric bus-notice-survivor (bus device)
  (:documentation
   "A callback for bus-specific reaction to bus-scan-time encounters
of apparently surviving devices.")
  (:method ((o bus) (d bus-device)) t))

(defgeneric bus-add (bus device)
  (:method ((bus bus) device)
    (declare (ignore device))
    t)
  (:method :after ((bus bus) device)
    (setf (bus-device bus (device-bus-address device)) device)))

(defgeneric bus-populate-address (bus address)
  (:documentation
  "A callback for bus-specific reaction to the event of a new device
appearing on it.
The primary must return the new device, if any.")
  (:method :around ((o bus) address)
    (declare (ignore address))
    (when-let ((device (call-next-method)))
      (handler-case (bus-add o device)
        (error (c)
          (syncformat *error-output* "~@<ERROR: ~@;encountered error while adding ~S to bus:~%~3T~A~:@>~%" o c))))))

(defgeneric bus-remove (bus device)
  (:method ((bus bus) device)
    (declare (ignore device))
    t)
  (:method :before ((bus bus) device)
    (remove-bus-device bus (device-bus-address device))))

(defmethod initialize-instance :after ((o bus-device) &key &allow-other-keys)
  (setf (device-bus-id o) (bus-id-at-address (device-bus o) (device-bus-address o))))

(defgeneric bus-scan (bus &optional force-rescan)
  (:documentation
   "Scan BUS for devices, calling BUS-ADD on new devices, BUS-REMOVE
on devices that disappeared and ")
  (:method ((o bus) &optional force-rescan)
    (let* ((old-occupied (unless force-rescan
                           (mapcar #'device-bus-address (bus-devices o))))
           (new-occupied (bus-occupied-addresses o)) ; This fills in probe-time bus IDs, for dumb busses.
           (missing (set-difference old-occupied new-occupied :test #'=))
           (new (set-difference new-occupied old-occupied :test #'=))
           (survivors (intersection new-occupied old-occupied :test #'=)))
      (when *log-bus-events*
        (syncformat *log-stream* "~&While processing bus ~S:~%~Tnewly vacant addresses:~{ ~S~}~%~Tnewly occupied addresses:~{ ~S~}~%~Tstill busy addresses:~{ ~S~}~%"
                    o missing new survivors))
      (dolist (addr survivors)
        (let ((survivor-at-addr (bus-device o addr)))
          (cond ((bus-id-equalp o (bus-id-at-address o addr) (device-bus-id survivor-at-addr))
                 (bus-notice-survivor o survivor-at-addr))
                ;; The device looks sufficiently different to warrant a fresh look at itself.
                (t
                 (push addr missing)
                 (push addr new)))))
      (dolist (addr missing)
        (bus-remove o (bus-device o addr)))
      (dolist (addr new)
        (bus-populate-address o addr)))))

(defun bus-devices (bus)
  "Return all BUS devices."
  (maphash-values #'identity (%bus-devices bus)))

(defmethod initialize-instance :after ((bus root-bus) &key name &allow-other-keys)
  (setf (root-bus name) bus))

(defmethod initialize-instance :after ((bus probe-discovery-bus) &key bus-probe-address-set &allow-other-keys)
  (setf (bus-probe-time-bus-ids bus) (make-hash-table :size (length bus-probe-address-set))))
