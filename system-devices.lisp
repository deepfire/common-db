;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MULTICORE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2009, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2009 by
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

(in-package :sysdev)


(define-namespace :platform
  (:documentation "Platform devices"))

(define-device-class cache :platform (platform-memory-region slave-memory-region)
    ()
  (:default-initargs :enumeration-class 'cache-memory))

(define-device-class memory :platform (mapped-platform-memory-region)
    ()
  (:default-initargs :enumeration-class 'memory))

(define-device-class internal-memory :platform (memory slave-memory-region)
    ())

(define-device-class external-memory :platform (memory slave-memory-region)
    ())

(define-device-class ram :platform (external-memory) ()
  (:default-initargs :enumeration-class 'ram))

(define-device-class dcache :platform (cache) ())
(define-device-class icache :platform (cache) ())
(define-device-class scache :platform (cache) ())
