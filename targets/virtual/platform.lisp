;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: VIRTCORE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :virtcore)


(define-platform virtual-platform ()
  (:instantiate-p t)
  (:predefined-devices)
  (:default-initargs
   :memory-map (oct-1d:make-tree :length #x20000000)))

(define-protocol-device-class virtual-mapped-device nil (mapped-device platform-device)
  ()
  (:default-initargs
   :mutator-fn #'backend
   :get-fn #'memory-ref
   :set-fn #'(setf memory-ref)))

(define-device-class virtport :platform (virtual-mapped-device)
    ()
  (:layouts (:vport mapped-ref (setf mapped-ref))))

(define-device-class virtport-dma-channel :platform (virtual-mapped-device slave-device)
    ()
  (:layouts (:vdma mapped-ref (setf mapped-ref))))

(define-device-class memory-dma-engine :platform (virtual-mapped-device slave-device)
  ()
  (:layouts (:vdma mapped-ref (setf mapped-ref))))

(define-device-class virtimer :platform (virtual-mapped-device)
  ()
  (:layouts (:vtimer mapped-ref (setf mapped-ref))))

(define-protocol-device-class system :platform (virtual-mapped-device) ()
  (:default-initargs :enumeration-class 'system))

(define-device-class virtsys :platform (system) ())
