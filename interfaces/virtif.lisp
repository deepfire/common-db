;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: INTERFACE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2010, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2010 by
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

(in-package :interface-virtual)

(set-namespace :interface)


(defvar *virtual-interface-stop-during-reset* nil)

;;;;
;;;; Interface bus
;;;;
(define-device-class virtif-bus :empty (root-bus enumerating-bus interface-bus)
  ())

(defmethod bus-occupied-addresses ((o virtif-bus))
  (when *verbose-interface-init*
    (syncformat t "NOTE: looking up attached virtif devices~%"))
  (when *virtual-interface-enabled*
    (list 0)))

(defmethod bus-populate-address ((o virtif-bus) address)
  (lret ((iface (make-instance 'virtif-interface :bus o :address address :version 0)))
    (setf (iface-idcode iface) (decode-bitfield :oncd-version (interface-reset iface)))))

;;;;
;;;; Interface
;;;;
(define-device-class virtif-interface :interface (interface)
  ((state :accessor virtif-state :type (member :undefined :unattached :attached) :initarg :state))
  (:default-initargs
   :state :undefined))

(defmethod interface-reset ((o virtif-interface))
  (setf (virtif-state o) :unattached)
  #x42) ; return idcode

(defmethod interface-attach-target ((o virtif-interface))
  (setf (virtif-state o) :attached))

(defmethod interface-stop-target ((o virtif-interface))
  (stop-target-using-interface (first (iface-targets o)) o))

(defmethod interface-reset-target ((o virtif-interface) stop-cores)
  (let ((*virtual-interface-stop-during-reset* stop-cores))
    (reset-target-using-interface (first (iface-targets o)) o)))

;;;
;;; interface-bus-{word,io} are not necessarily implemented at this level,
;;; as upper layers use the memory device and bioable APIs.
;;;
