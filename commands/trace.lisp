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

(defun count-trace (addr-or-sym &rest run-args)
  #+help-ru
  "Установить аппаратную точку останова в адрес заданный через 
спецификатор адреса ADDR-OR-SYM, и запускать целевое устройство, 
выводя на экран счётчик остановов.

Остаточные параметры при каждом пуске передаются команде RUN."
  (with-free-hardware-breakpoints (*core*) ((b (coerce-to-address addr-or-sym)))
    (with-temporary-state (*core* :stop)
      (iter (for i from 1)
            (apply #'run run-args)
            (syncformat *log-stream* "~&---( at ~,'0X: stop ~D~%" addr-or-sym i))))
  (values))

(defun block-trace (size &optional count &rest run-args &key (state :stop) &allow-other-keys &aux
                    (core *core*))
  #+help-ru
  "Произвести трассировку блоками, по SIZE инструкций в блоке.
Если задан параметр COUNT не равный NIL, выйти после указанного им количества блоков.

Остаточные параметры при каждом пуске передаются команде RUN."
  (with-temporary-state (core state)
    (let ((*display* nil)
          (*explain* nil))
      (iter (for i from 0)
            (when (and count (= i count))
              (return))
            (settrace size)
            (apply #'run (remove-from-plist run-args :state))
            (syncformat *log-stream* "---( ~D passed, ~D " size i)
            (addr (moment-fetch (saved-core-moment core)))))
    (values)))
