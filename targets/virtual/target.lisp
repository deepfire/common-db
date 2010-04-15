;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: VIRTCORE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :virtcore)

(set-namespace :target :interface)


(defvar *virtual-target-platform-known* t
  "Whether the virt target has a known platform.")

;;;;
;;;; Custom target types
;;;;
(define-device-class generic-virtual-target :target (32bit-bus-target)
  ((memory :accessor target-memory :initarg :memory))
  (:default-initargs
   :memory (make-hash-table :test 'eq)
   :mapped-artifact-map (symtable:make-symtable #x0 #x20000000)))

(define-device-class virtual-target :target (generic-virtual-target) ())

;;;;
;;;; Target discrimination
(defun discriminate-by-target-enabledness (interface)
  (declare (ignore interface))
  (when *virtual-target-enabled*
    :virt))

#-disable-virtcore
(defmethod interface-target-discrimination-tree ((o virtif-interface))
  (make-discrimination-tree
   (node :root #'discriminate-by-target-enabledness
         (node :virt virtual-target))))

;;;;
;;;; Platform discrimination: see definitions.lisp
(defun discriminate-by-platform-knownness (target)
  (declare (ignore target))
  (when *virtual-target-platform-known*
    :certified-virt))

(defmethod target-platform-discrimination-tree ((o virtual-target))
  (make-discrimination-tree
   (node :root #'discriminate-by-platform-knownness
         (node nil generic-virtual-platform)
         (node :certified-virt basic-virtual-platform))))

(defmethod detect-target-platform ((o generic-virtual-target) &optional if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (discriminate (target-platform-discrimination-tree o) o))

;;;;
;;;; Execution
;;;;
(defmethod exec-raw ((o virtual-target) insn &optional address)
  (declare (ignore insn address)))

;;;;
;;;; Address space
;;;;
(defmethod fixmap-address ((o virtual-target) (x integer))
  "Provide MIPS32-style fixed mapping."
  (if (ksegp x)
      (remap-to-kuseg x)
      x))
