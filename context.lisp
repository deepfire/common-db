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


(defvar *target-contexts*  nil)
(defvar *current*          nil)
(defvar *interface*        nil)
(defvar *target*           nil)
(defvar *core*             nil)

(defclass target-context ()
  ((interface :reader ctx-interface :initarg :interface)
   (target :reader ctx-target :initarg :target)
   (core :reader ctx-core :initarg :core)
   (display-list :accessor ctx-display-list :initarg :display-list)
   (initargs :reader ctx-initargs :initarg :initargs)))

(defun ctxs-of-type (type)
  (remove-if-not (of-type type) *target-contexts* :key (compose #'target-platform #'ctx-target)))

(defun ctx-by-id (ctx-id)
  #+help-ru
  "Найти контекст по идентификатору."
  #-help-ru
  "Find a target context by its identificator."
  (destructuring-bind (type nr) ctx-id
    (nth nr (ctxs-of-type type))))

(defun ctx-id (ctx)
  "Compute CTX's unique identificator."
  (let ((type (type-of (target-platform (ctx-target ctx)))))
    (list type (position type (ctxs-of-type type) :key (compose #'type-of #'target-platform #'ctx-target)))))

(define-print-object-method ((o target-context) interface target core initargs)
    "~@<#<CTX~; ~A  interface: ~S, target: ~S, core: ~S, initargs: ~S~:@>"
  (ctx-id o) interface target core initargs)

(defmethod initialize-instance :after ((o target-context) &key interface target core (display-list nil dlist-specifiedp) &allow-other-keys)
  (setf (ctx-display-list o) (if dlist-specifiedp
                                 display-list
                                 (default-display-list (target-platform target) interface target core))))

(defun set-context (ctx)
  "Make all UI functions operate on devices specified by CTX."
  (setf *current* ctx
        *interface* (ctx-interface ctx)
        *target* (ctx-target ctx)
        *core* (ctx-core ctx)))

(defun remove-context (ctx)
  "Remove target context CTX."
  (removef *target-contexts* ctx)
  (when (eq ctx *current*)
    (setf *interface* nil
          *target* nil
          *core* nil)))

(defmacro do-all-target-contexts ((&optional ctx) &body body)
  "Iterate BODY over contexts of all detected targets, with
CTX optionally bound to successive contexts."
  (let ((ctx (or ctx (gensym))))
    `(iter (for ,ctx in *target-contexts*)
           (let ((*core* (ctx-core ,ctx))
                 (*target* (ctx-target ,ctx))
                 (*interface* (ctx-interface ,ctx)))
             ,@body))))

(defun list-contexts ()
  #+help-ru
  "Вывести список контекстов обнаруженных целевых устройств."
  (write-line "Detected target device contexts:")
  (iter (for ctx in *target-contexts*)
        (for nr from 0)
        (format t "Context #~D: id ~A, core ~A~%" nr (ctx-id ctx) (ctx-core ctx))))

(defun ctx (&optional id)
  #+help-ru
  "При отсутствии параметров, вывести список целевых контекстов,
содержащийся в переменной =*TARGET-CONTEXTS*=.
При наличии параметра, интерпретировать его как номер контекста,
если это число, либо идентификатор контекста, если это список,
который следует сделать теукщим."
  (etypecase id
    (null    *target-contexts*)
    (cons    (set-context (ctx-by-id id)))
    (integer (set-context (nth id *target-contexts*)))))

(defun display-list ()
  #+help-ru
  "/Место/ предоставляющее доступ к текущему списку отображения."
  (ctx-display-list *current*))

(defun set-display-list (new-list)
  (setf (ctx-display-list *current*) new-list))

(defsetf display-list set-display-list)