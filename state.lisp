;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIPS; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :core)

;;;;
;;;; Page-granular access
;;;;
(defun core-physical-pages (core address-list page-size)
  (iter (for addr in address-list)
        (u8-extent (backend core) (extent (logandc1 (1- page-size) addr) page-size))))

(defun set-core-physical-pages (core address-list page-size extents)
  (declare (ignore page-size))
  (iter (for addr in address-list)
        (for extent in extents)
        (write-u8-extent core (rebase (constantly addr) extent))))

(defsetf core-physical-pages set-core-physical-pages)

(defun core-virtual-pages (core address-map)
  "Read from CORE the pages designated by ADDRESS-MAP."
  (lret ((pagesize (address-map-page-size address-map))
         extents)
    (map-address-map (lambda (virt phys)
                       (let ((extent (first (push (make-extent 'extent virt pagesize) extents))))
                         (read-block core phys (extent-data extent))))
                     address-map)))

(defun set-core-virtual-pages (core address-map extents)
  "Write all the virtual-addressed EXTENTS into CORE, using the ADDRESS-MAP."
  (let ((pagesize (address-map-page-size address-map)))
    (dolist (extent extents)
      (let ((physaddr (virt-to-phys address-map (base extent))))
        (format t "loading virtual page ~08X to physaddr ~08X~%" (base extent) physaddr)
        (write-block core physaddr (extent-data extent) 0 pagesize)))))

(defsetf core-virtual-pages set-core-virtual-pages)

;;;;
;;;; State
;;;;
(defclass state ()
  ((moment :accessor state-moment :type moment :initarg :moment)
   (trail :accessor state-trail :type trail :initarg :trail)
   (gpr :accessor state-gpr :type list :initarg :gpr)
   (regs :accessor state-regs :type list :initarg :regs)
   (fpr :accessor state-fpr :type list :initarg :fpr)
   (tlb :accessor state-tlb :type list :initarg :tlb)
   (page-size :accessor state-page-size :type (or null integer) :initarg :page-size)
   (virtual-pages :accessor state-virtual-pages :type list :initarg :virtual-pages)
   (physical-pages :accessor state-physical-pages :type list :initarg :physical-pages)
   (physical-cells :accessor state-physical-cells :type list :initarg :physical-cells)))

(defgeneric capture-state-using-state (core state &key regs fpr tlb page-size physical-pages physical-cells virtual-pages))
(defgeneric apply-state (core state))
(defgeneric write-state-to-stream (stream state) (:method (stream (o state))))
(defgeneric emit-nonmemory-state-restorer (segment state)
  (:documentation
   "Emit code to restore STATE (obviously, excluding memory state) into SEGMENT."))

(defmethod capture-state-using-state ((o mmu-core) state &key regs fpr tlb page-size physical-pages physical-cells virtual-pages)
  (declare (ignore regs fpr tlb page-size physical-pages physical-cells virtual-pages)))

(defun capture-state (core &rest args &key regs fpr page-size physical-pages physical-cells virtual-pages (tlb virtual-pages))
  "Assume :DEBUG state."
  (when (and (not page-size)
             (or physical-pages virtual-pages))
    (error "Cannot capture memory pages without PAGE-SIZE specified."))
  (lret* ((tlb (when tlb
                 (get-tlb core)))
          (state (make-instance (core-default-state-type core)
                                :moment (saved-core-moment core) :trail (saved-core-trail core)
                                :gpr (mapcar (curry #'gpr core) (iota (isa-gpr-count (core-isa core))))
                                :regs (iter (for reg in regs) (collect (list reg (devreg core reg))))
                                :fpr (when fpr
                                       (mapcar (curry #'fpr core) (iota (isa-fpr-count (core-isa core)))))
                                :tlb tlb
                                :page-size page-size
                                :physical-pages (core-physical-pages core physical-pages page-size)
                                :physical-cells (iter (for addr in physical-cells) (collect (list addr (memory-ref (backend core) addr))))
                                :virtual-pages (core-virtual-pages core (tlb-address-map core tlb page-size)))))
    (apply #'capture-state-using-state core state args)))

(defmethod apply-state ((o mmu-core) state)
  (let ((space (device-space o))
        (target (backend o)))
    (with-slots (moment trail gpr regs fpr tlb physical-pages physical-cells page-size virtual-pages) state
      (iter (for (addr val) in physical-cells)
            (format t ";; restoring memory cell at 0x~8,'0X~%" addr)
            (setf (memory-ref target addr) val))
      (setf (values) (format t ";; restoring TLB-mapped pages:~{ ~8,'0X~}~%" (mapcar #'base virtual-pages))
            (core-virtual-pages o (tlb-address-map o tlb page-size)) virtual-pages
            (values) (format t ";; restoring direct-mapped pages:~{ ~8,'0X~}~%" (mapcar #'base physical-pages))
            (core-physical-pages o nil page-size) physical-pages
            (values) (format t ";; restoring TLB~%")
            (get-tlb o) tlb)
      (format t ";; restoring FPR~%")
      (iter (for val in fpr) (for i below (isa-fpr-count (core-isa o))) 
            (set-gpr o i val))
      (format t ";; restoring miscellaneous registers~%")
      (iter (for (reg val) in regs)
            (setf (device-register o (register-id space reg)) val))
      (format t ";; restoring GPR~%")
      (iter (for val in gpr) (for i below (isa-gpr-count (core-isa o))) 
            (set-gpr o i val))
      (format t ";; restoring moment (~A) and trail~%" moment)
      (setf (saved-core-trail o) trail
            (core-trail o) trail
            (saved-core-moment o) moment
            (core-moment o) moment))))

;;;;
;;;; Serialisation/deserialisation
;;;;
(defmethod write-state-to-stream :around (stream (o state))
  (let ((*print-base* #x10))
    (mapcar (curry #'format stream "~S~%")
            (remove nil (list (type-of o)
                              (list :moment (moment-fetch (state-moment o)) (moment-opcode (state-moment o)))
                              (list* :trail (listify-trail (state-trail o)))
                              (list* :gpr (state-gpr o))
                              (list* :regs (state-regs o))
                              (when (state-fpr o)
                                (list :fpr (state-fpr o)))
                              (when (state-tlb o)
                                (list :tlb (state-tlb o)))
                              (when (state-page-size o)
                                (list :page-size (state-page-size o)))
                              (when (state-physical-cells o)
                                (list* :physical-cells (state-physical-cells o))))))
    (when (state-physical-pages o)
      (format stream "~S~%" (list :physical-pages))
      (serialize-extent-list stream (state-physical-pages o)))
    (when (state-virtual-pages o)
      (format stream "~S~%" (list :virtual-pages))
      (serialize-extent-list stream (state-virtual-pages o)))
    (call-next-method)))

(defun write-state (state filename)
  (with-output-to-file (stream filename)
    (write-state-to-stream stream state)))

(defun write-core-state (core filename &rest args)
  (write-state (apply #'capture-state core args) filename))

(defun read-state (stream)
  (let (pc gpr regs physical-cells page-size tlb virtual-pages physical-pages (*read-base* #x10))
    (let ((type (read stream)))
      (iter (for form = (read stream nil nil))
            (while form)
            (ecase (first form)
              (:pc (setf pc (second form)))
              (:gpr (setf gpr (rest form)))
              (:regs (setf regs (rest form)))
              (:physical-cell (push (rest form) physical-cells))
              (:page-size  (setf page-size (second form)))
              (:tlb (setf tlb (second form)))
              (:virtual-pages (setf virtual-pages (read-extent-list stream)))
              (:physical-pages (setf physical-pages (read-extent-list stream)))))
      (make-instance type :pc pc :gpr gpr :regs regs :tlb tlb :page-size page-size
                     :virtual-pages virtual-pages :physical-pages physical-pages :physical-cells (nreverse physical-cells)))))
