;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :common-db)

(defun dsp (n)
  #+help-ru
  "Вернуть объект представляющий N-ное DSP-ядро."
  (target-device *target* (list 'dsp n)))

(defun coerce-to-dsp (x)
  (etypecase x
    (integer (dsp x))
    (dsp x)))

(defun dspbreak (dsp-core-or-id n address)
  #+help-ru
  "Установить N-ную точку останова DSP-ядра заданного через спецификатор DSP-ядра
DSP-CORE-OR-ID в ADDRESS, если последний не-NIL.  В противном случае, отключить её.

Примеры:
  : (dspbreak (dsp 0) 3 #x1010)
  : (dspbreak 0 3 #x1010)"
  #-help-ru
  "Manage N'th breakpoint of a DSP core: NIL for ADDRESS clears the breakpoint."
  (check-address-alignment 4 address)
  (let* ((dsp-core (coerce-to-dsp dsp-core-or-id))
         (breakpoint (hwbreak dsp-core n)))
    (setf (values (trap-enabled-p breakpoint) (trap-address breakpoint))
          (when address
            (values t address)))
    (values)))

(defun dspreset (dsp-core-or-id)
  #+help-ru
  "Произвести аппаратный сброс DSP-ядра заданного через спецификатор DSP-ядра DSP-CORE-OR-ID.

Примеры:
  : (dspreset (dsp 0))
  : (dspreset 0)"
  #-help-ru
  "Reset DSP-CORE."
  (let ((dsp-core (coerce-to-dsp dsp-core-or-id)))
    (reset-core dsp-core))
  (values))

(defun dspstop (dsp-core-or-id)
  #+help-ru
  "Ввести DSP-ядро заданное через спецификатор DSP-ядра DSP-CORE-OR-ID в режим останова.

Примеры:
  : (dspstop (dsp 0))"
  #-help-ru
  "Stop a running DSP."
  (let ((dsp-core (coerce-to-dsp dsp-core-or-id)))
    (setc (core-running-p dsp-core) nil)
    (poll-core-interruptible dsp-core))
  (values))
