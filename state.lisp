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
        (collect (u8-extent (backend core) (extent (logandc1 (1- page-size) addr) page-size)))))

(defun core-virtual-pages (core address-map)
  "Read from CORE the pages designated by ADDRESS-MAP."
  (lret ((page-size (address-map-page-size address-map))
         extents)
    (map-address-map (lambda (virt phys)
                       (let ((extent (first (push (make-extent 'extent virt page-size) extents))))
                         (read-block core phys (extent-data extent))))
                     address-map)))

(defun set-core-virtual-pages (core address-map verbose extents)
  "Write all the virtual-addressed EXTENTS into CORE, using the ADDRESS-MAP."
  (let ((page-size (address-map-page-size address-map)))
    (dolist (extent extents)
      (let ((physaddr (virt-to-phys address-map (base extent))))
        (when verbose
          (format t ";;;; loading virtual page ~08X to physaddr ~08X~%" (base extent) physaddr))
        (write-block core physaddr (extent-data extent) 0 page-size)))))

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
   (physical-cells :accessor state-physical-cells :type list :initarg :physical-cells))
  (:default-initargs
   :regs nil :fpr nil :tlb nil
   :page-size nil
   :physical-pages nil :physical-cells nil
   :virtual-pages nil))

(defgeneric capture-state-using-state (core state &key regs fpr tlb page-size physical-pages physical-cells virtual-pages))
(defgeneric apply-state (core state &key verbose)
  (:method :around (core (o stream) &key verbose)
    (declare (ignore verbose))
    (apply-state core (read-state-for-core core o)))
  (:method :around (core (o string) &key verbose)
    (declare (ignore verbose))
    (when (string= "bank" (pathname-type o))
      (error "~@<File ~S has type 'bank', which is supposed, by convention, to contain a state restorer, not a state.~:@>" o))
    (apply-state core (read-state-for-core core o)))
  (:method :around (core (o pathname) &key verbose)
    (declare (ignore verbose))
    (apply-state core (read-state-for-core core o))))
(defgeneric write-state-to-stream (stream state) (:method (stream (o state)) (declare (ignore stream))))
(defgeneric emit-state-restorer (core segment state)
  (:documentation
   "Emit code into SEGMENT to restore CORE's STATE."))

(defmethod capture-state-using-state ((o mmu-core) state &key regs fpr tlb page-size physical-pages physical-cells virtual-pages)
  (declare (ignore state regs fpr tlb page-size physical-pages physical-cells virtual-pages)))

(defun capture-state (core &rest args &key regs fpr (page-size (current-page-size core)) physical-pages physical-cells (virtual-pages t) (tlb virtual-pages))
  "Assume :DEBUG state."
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

(defmethod apply-state ((o mmu-core) state &key verbose)
  (let ((space (device-space o)))
    (with-slots (moment trail gpr regs fpr tlb physical-pages physical-cells page-size virtual-pages) state
      (iter (for (addr val) in physical-cells)
            (when verbose
              (format t ";; restoring memory cell at 0x~8,'0X~%" addr))
            (setf (memory-ref (backend o) addr) val))
      (setf (values) (when verbose
                       (format t ";; restoring TLB-mapped pages:~{ ~8,'0X~}~%" (mapcar #'base virtual-pages)))
            (core-virtual-pages o (tlb-address-map o tlb page-size) verbose) virtual-pages
            (values) (when verbose
                       (format t ";; restoring direct-mapped pages:~{ ~8,'0X~}~%" (mapcar #'base physical-pages)))
            (values) (mapc (curry #'write-u8-extent o) physical-pages)
            (values) (when verbose
                       (format t ";; restoring TLB~%"))
            (get-tlb o) tlb)
      (when fpr
        (when verbose
          (format t ";; restoring FPR~%"))
        (iter (for val in fpr) (for i below (isa-fpr-count (core-isa o))) 
              (set-gpr o i val)))
      (when verbose
        (format t ";; restoring miscellaneous registers: ~A~%" (mapcar #'car regs)))
      (iter (for (reg val) in regs)
            (setf (device-register o (register-id space reg)) val))
      (when verbose
        (format t ";; restoring GPR~%"))
      (iter (for val in gpr) (for i below (isa-gpr-count (core-isa o))) 
            (set-gpr o i val))
      (when verbose
        (format t ";; restoring moment ~A and trail ~A~%" moment trail))
      (setf (saved-core-trail o) trail
            (saved-core-moment o) moment)
      (restore-core-trail o)
      (restore-core-moment o)
      state)))

(defun apply-bank (core bank)
  "This function expects a clean, post-reset core."
  (write-u8-extents core
                    (etypecase bank
                      (list bank)
                      (loadable:loadable
                       (loadable:loadable-sections bank))
                      ((or string pathname)
                       (loadable:loadable-sections (loadable:extract-loadable :bank bank :entry-point (default-core-pc core)))))))

;;;;
;;;; Serialisation/deserialisation
;;;;
(defmethod write-state-to-stream :around (stream (o state))
  (let ((*print-base* #x10))
    (with-slots (moment trail gpr fpr regs tlb physical-cells page-size physical-pages virtual-pages) o
      (mapcar (curry #'format stream "~S~%")
              (remove nil (list (type-of o)
                                (list :moment (moment-fetch moment) (moment-opcode moment))
                                (list* :trail (listify-trail trail))
                                (list* :gpr gpr)
                                (list* :regs regs)
                                (when fpr
                                  (list :fpr fpr))
                                (when tlb
                                  (list :tlb (make-keyword (type-of (first tlb)))
                                        (mapcar #'listify-tlb-entry tlb)))
                                (when page-size 
                                  (list :page-size page-size))
                                (when physical-cells
                                  (list* :physical-cells physical-cells)))))
      (when physical-pages 
        (format stream "~S~%" (list :physical-pages))
        (serialize-extent-list stream physical-pages))
      (when virtual-pages 
        (format stream "~S~%" (list :virtual-pages))
        (serialize-extent-list stream virtual-pages))
      (call-next-method))))

(defun write-state (state filename)
  (with-output-to-file (stream filename)
    (write-state-to-stream stream state)))

(defun write-core-state (core filename &rest args)
  (write-state (apply #'capture-state core args) filename))

(defun state-restorer-extent (core state &key (entry-point (default-core-pc core)))
  "Produce a non-relocatable memory extent, combining code and data, which when activated
will rebuild STATE on CORE.
ENTRY-POINT specifies the entry point of the resulting code."
  (declare (type (or null (integer 0)) entry-point))
  (make-extent 'extent entry-point (segment-active-vector (emit-state-restorer core (make-instance 'pinned-segment :base entry-point) state))))

(defun write-state-restorer-bank (core state filename &key (entry-point (default-core-pc core)))
  (lret ((restorer-extent (state-restorer-extent core state :entry-point entry-point)))
    (bank:write-extents-as-bank filename (list restorer-extent))))

(defun read-state-for-core (core state-stream-or-filename &aux (*read-base* #x10))
  (with-open-file (stream state-stream-or-filename)
    (lret* ((type (read stream))
            (state (progn
                     (unless (symbolp type)
                       (error "~@<First token in SEXP state file must be a symbol type designator.~:@>"))
                     (make-instance type))))
      (with-slots (moment trail gpr fpr regs physical-cells page-size tlb virtual-pages physical-pages) state
        (iter (for form = (read stream nil nil))
              (while form)
              (ecase (first form)
                (:moment (setf moment (make-moment 'moment (second form) (third form))))
                (:trail (setf trail (parse-trail core (rest form))))
                (:gpr (setf gpr (rest form)))
                (:fpr (setf fpr (rest form)))
                (:regs (setf regs (rest form)))
                (:physical-cells (setf physical-cells (rest form)))
                (:page-size  (setf page-size (second form)))
                (:tlb (setf tlb (mapcar (curry #'parse-tlb-entry (second form)) (third form))))
                (:virtual-pages (setf virtual-pages (read-extent-list stream)))
                (:physical-pages (setf physical-pages (read-extent-list stream)))))))))
