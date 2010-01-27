;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TEST-PERFORMANCE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :test-hardware)

(set-namespace :interface :core)

(defcomdbtest :perf memory-block-io-speed () (core &aux (target (backend core)))
  (format nil "~D kb/sec"
          (with-measured-performance (size #x40000 :type 'integer :scale 512)
            (test-target-memory target #x0 size :if-fails :print-checksum))))

(defcomdbtest :perf memory-word-read-speed () (core &aux (target (backend core)))
  (format nil "~D word reads/sec"
          (with-measured-performance (size #x2000 :type 'integer)
            (iter (repeat size) (memory-ref target #x0)))))

(defcomdbtest :perf gpr-read-speed () (core)
  (setc (state core) :debug)
  (format nil "~D $ra reads/sec"
          (with-measured-performance (size #x1000 :type 'integer)
            (iter (repeat size) (devreg core :ra)))))

(defcomdbtest :perf cop0-read-speed () (core)
  (setc (state core) :debug)
  (format nil "~D cop0.cause reads/sec"
          (with-measured-performance (size #x800 :type 'integer)
            (iter (repeat size) (devreg core :cause)))))

(defcomdbtest :perf debug-state-change-speed () (core)
  (setc (state core) :debug)
  (format nil "~D debug->stop->debug transitions/sec"
          (with-measured-performance (size #x100 :type 'integer)
            (iter (repeat size) (with-temporary-state (core :stop))))))

(defcomdbtest :perf heavy-step-speed () (core)
  (setc (state core) :debug)
  (format nil "~D debug steps/sec"
          (with-measured-performance (size #x100 :type 'integer)
            (iter (repeat size) (step 1 :display nil)))))

(defcomdbtest :perf light-step-speed () (core)
  (setc (state core) :stop)
  (format nil "~D light steps/sec"
          (with-measured-performance (size #x100 :type 'integer)
            (iter (repeat size) (step 1 :display nil)))))
