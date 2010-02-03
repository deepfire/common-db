;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GDB; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :gdb)


(defgeneric describe-target (target)
  (:method ((o target))
    (with-html-output-to-string (s)
      (:target :version "1.0"
               (terpri s)
               (str (describe-core (target-device o '(general-purpose-core 0))))))))

(defgeneric describe-core (core)
  (:method-combination most-specific-last)
  (:method ((o core))
    (concatenate 'string
                 (with-html-output-to-string (s)
                   (:architecture (str (string-downcase (string (isa-name (core-isa o))))))
                   (terpri s))
                 (call-next-method))))

(defgeneric describe-register (device name bitsize regnum group)
  (:method ((o device) name bitsize regnum group)
    (format nil "<reg name='~(~A~)' bitsize='~A'~:[~; regnum='~D'~] group='~(~A~)'/>~%"
            name bitsize #+nil regnum nil group)))

(defgeneric core-register-order (core))
