;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TRACE; Base: 10 -*-
;;;
;;;  (c) copyright 2010 by
;;;           Elvees (http://www.elvees.ru)
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

(in-package :trace)

(defvar *trace-stream* nil)
(defvar *trace-calls* nil)
(defvar *trace-results* nil)

(defun setup-trace (filename package-name)
  (setf *trace-stream* (open filename :direction :output :if-exists :supersede))
  (format *trace-stream*
          ";;; -*- Mode: Lisp -*-~%(in-package :~A)~%(setf *read-base* #x10)~%"
          (string-downcase (string package-name))))

(defun enable-call-trace ()
  (setf *trace-calls* t)
  (values))

(defun disable-call-trace ()
  (setf *trace-calls* nil)
  (values))

(defun enable-result-trace ()
  (setf *trace-results* t)
  (values))

(defun disable-result-trace ()
  (setf *trace-results* nil)
  (values))

(defun trace-fun-call (name format-control format-arguments &aux
                       (*print-base* #x10)
                       (*standard-output* *trace-stream*))
  (write-char #\()
  (write-string (symbol-name name))
  (when format-control
    (write-char #\Space)
    (apply #'format t format-control format-arguments))
  (write-char #\))
  (terpri)
  (finish-output))

(defun trace-fun-result (result &aux
                         (*print-base* #x10)
                         (*standard-output* *trace-stream*))
  (declare (optimize debug safety (speed 0)))
  (when (arrayp result)
    (write-char #\()
    (write-string (string 'quote))
    (write-char #\Space)
    (write result)
    (write-char #\))
    (terpri)
    (finish-output)))

(defmacro define-traced-fun (name lambda-list (&optional format-control &rest format-arguments) &body body)
  (multiple-value-bind (documentation declarations body) (destructure-def-body body)
    `(defun ,name ,lambda-list
       ,@(when documentation `(,documentation))
       ,@(when declarations `((declare ,@declarations)))
       (when *trace-calls*
         (trace:trace-fun-call ',name ,format-control ,(when format-control `(list ,@format-arguments))))
       (let ((results (multiple-value-list (progn ,@body))))
         (when *trace-results*
           (trace:trace-fun-result (first results)))
         (values-list results)))))