;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BUS; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :options)

(defvar *log-stream* t 
  "Global log stream for the whole debugger.")

(defvar *trace-exchange* nil
  "Whether to report wire exchanges to *STANDARD-OUTPUT*.")

(defvar *virtual-target-enabled* nil
  "Whether the virt target shows up on the virt interface.")

(defvar *initargs*         nil
  "Option argument list.")


(defun args ()
  *initargs*)

(defun set-args (value)
  (setf *initargs* value))


(defun arg (name)
  (getf *initargs* name))

(defun set-arg (name value)
  (setf (getf *initargs* name) value))


(defsetf args set-args)
(defsetf arg set-arg)