;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :common-db)


(defun in-hex ()
  #+help-ru
  "Установить базу читаемых чисел в 16."
  #-help-ru
  "Set *READ-BASE* to 16."
  (setf *read-base* #x10))

(defun out-hex ()
  #+help-ru
  "Установить базу печатаемых чисел в 16."
  #-help-ru
  "Set *PRINT-BASE* to 16."
  (setf *print-base* #x10))

(defun in-dec ()
  #+help-ru
  "Установить базу читаемых чисел в 10."
  #-help-ru
  "Set *READ-BASE* to 10."
  (setf *read-base* #xa))

(defun out-dec ()
  #+help-ru
  "Установить базу печатаемых чисел в 10."
  #-help-ru
  "Set *PRINT-BASE* to 10."
  (setf *print-base* #xa))