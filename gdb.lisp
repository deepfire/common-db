;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GDB; Base: 10; indent-tabs-mode: nil -*-
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

(defpackage #:gdb
  (:use :common-lisp :alexandria :pergamum :iterate :isa :isa-mips :assem :assem-mips :setc :bitmop :device-model
        :options :portability :spaces :generic :platform :tgt :core :who)
  (:shadow #:step)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   #:describe-target
   #:describe-memory-map
   #:describe-spu
   #:core-register-id
   #:core-register-order))

(in-package :gdb)

(defgeneric core-register-id (core name)
  (:documentation
   "Map register names to GDB ids, which it unfortunately mandates."))

(defgeneric describe-target (target reginstance-cb)
  (:method ((o target) reginstance-cb &aux gdb-id-to-register-instance-map)
    (values (with-html-output-to-string (s)
              (:target :version "1.0"
                       (terpri s)
                       (multiple-value-bind (core-desc ri-map) (describe-core (target-device o '(general-purpose-core 0)) reginstance-cb)
                         (setf gdb-id-to-register-instance-map ri-map)
                         (str core-desc))))
            gdb-id-to-register-instance-map)))

(defgeneric describe-core (core reginstance-cb)
  (:method-combination most-specific-last)
  (:method ((o core) reginstance-cb)
    (declare (ignore reginstance-cb))
    (concatenate 'string
                 (with-html-output-to-string (s)
                   (:architecture (str (string-downcase (string (isa-name (core-isa o))))))
                   (terpri s))
                 (call-next-method))))

(defgeneric describe-register (device name bitsize regnum type group)
  (:method ((o device) name bitsize regnum type group)
    (format nil "<reg name='~(~A~)' bitsize='~D'~:[~; regnum=~:*'~D'~]~:[~; ~:*type='~(~A~)'~] group='~(~A~)'/>~%"
            name bitsize regnum type group)))

(defgeneric describe-core-memory-region (core type start length)
  (:method ((o core) type start length)
    (format nil "<memory type='~(~A~)' start='0x~X' length='0x~X'/>"
            type start length)))

(defgeneric describe-memory-map (core)
  (:method-combination most-specific-last)
  (:method ((o core))
    (with-html-output-to-string (s)
      (:memory-map :version "1.0"
                   (terpri s)
                   (str (call-next-method))))))

(defgeneric describe-spu (target)
  (:method ((o target))
    ""))

(defgeneric core-register-order (core))
