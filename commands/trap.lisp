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


(defun settrace (count)
  #+help-ru
  "Установить ограничитель количества исполняемых инструкций в COUNT."
  (set-core-insn-execution-limit *core* count)
  (values))

(defun hw-break (break-or-id address &optional (skipcount 0) &key bound)
  #+help-ru
  "Установить аппаратную точку останова заданную через спецификатор
аппаратной точки останова BREAK-OR-ID на адрес заданный через спецификатор
адреса ADDRESS. Опциональный параметр SKIPCOUNT задаёт количество пропусков
срабатывания."
  (declare (type (or (integer 0 1) hardware-trap) break-or-id))
  (let ((breakpoint (xform-if (of-type 'integer) (curry #'hwbreak *core*) break-or-id)))
    (setup-hw-trap breakpoint (if address (coerce-to-address address)) skipcount
                   :read t :write nil :bound bound :memory nil)) ; XXX: abstract out these flags
  address)

(defun watch (break-or-id address &optional (skipcount 0) &key (read t) write bound)
  #+help-ru
  "Установить адресную ловушку точку заданную через спецификатор
аппаратной точки останова BREAK-OR-ID на адрес заданный через спецификатор
адреса ADDRESS. Опциональный параметр SKIPCOUNT задаёт количество пропусков
срабатывания."
  (declare (type (or (integer 0 1) hardware-trap) break-or-id))
  (let ((breakpoint (xform-if (of-type 'integer) (curry #'hwbreak *core*) break-or-id)))
    (setup-hw-trap breakpoint (if address (coerce-to-address address)) skipcount
                   :read read :write write :bound bound :memory t))
  address)

(defun catch (&rest addresses)
  #+help-ru
  "Проинтерпретировать остаточный список параметров как набор адресов
векторных ловушек, и активировать их, сбросив неуказанные."
  (let* ((core *core*)
         (catches (mapcar (curry #'trap core) addresses)))
    (iter (for catch in catches)
          (unless catch (collect catch into missing))
          (unless (typep catch 'vector-trap) (collect catch into uncatches))
          (finally
           (when (or missing uncatches)
             (error "Invalid catches:~{ ~8,'0X~}.~%Valid catches are:~{ ~8,'0X~}~%"
                    (append missing uncatches)
                    (do-core-vector-traps (v core)
                      (collect (trap-address v)))))))
    (do-core-vector-traps (v core)
      (if (member v catches)
          (enable-trap v)
          (disable-trap v))))
  (values))

(defun sw-break (address-or-symbol)
  #+help-ru
  "Установить программную точку останова на адрес заданный через
спецификатор адреса ADDRESS-OR-SYMBOL."
  (lret ((address (coerce-to-address address-or-symbol)))
    (add-sw-breakpoint *core* address)))

(defun clear-break (address)
  #+help-ru
  "Удалить точку останова, будь то программную или аппаратную, установленную
на ADDRESS."
  (if-let ((b (trap *core* address :if-does-not-exist :continue)))
          (disable-trap b)
          (error 'no-core-breakpoint :core *core* :address address))
  (values))

(defun clear-sw-breaks ()
  #+help-ru
  "Удалить все программные точки останова."
  (do-core-traps (nil b *core*)
    (when (typep b 'core:software-breakpoint)
      (disable-trap b)))
  (values))

(defun disable-breaks ()
  #+help-ru
  "Отключить все точки останова."
  (do-core-traps (nil b *core*)
    (disable-trap b))
  (values))

(defun describe-breaks ()
  #+help-ru
  "Описать все активные точки останова."
  (let ((*print-right-margin* 200))
    (do-core-traps (addr b *core*)
      (when (trap-enabled-p b)
        (format t "~A~%~4T" b)
        (addr addr))))
  (values))
