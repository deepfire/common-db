;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIPS; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :mips)

(set-namespace :core :target :interface)


(defun core-phys-pages (core address-list page-size)
  (iter (for addr in address-list)
        (u8-extent (backend core) (extent (logandc1 (1- page-size) addr) page-size))))

(defun (setf core-phys-pages) (extents core address-list page-size)
  (declare (ignore page-size))
  (iter (for addr in address-list)
        (for extent in extents)
        (write-u8-extent core (rebase (constantly addr) extent))))

(defun core-virtual-pages (core address-map)
  "Read from CORE the pages designated by ADDRESS-MAP."
  (lret ((pagesize (address-map-page-size address-map))
         extents)
    (map-address-map (lambda (virt phys)
                       (let ((extent (first (push (make-extent 'extent virt pagesize) extents))))
                         (read-block core phys (extent-data extent))))
                     address-map)))

(defun (setf core-virtual-pages) (extents core address-map)
  "Write all the virtual-addressed EXTENTS into CORE, using the ADDRESS-MAP."
  (let ((pagesize (address-map-page-size address-map)))
    (dolist (extent extents)
      (let ((physaddr (virt-to-phys address-map (base extent))))
        (format t "loading virtual page ~08X to physaddr ~08X~%" (base extent) physaddr)
        (write-block core physaddr (extent-data extent) 0 pagesize)))))

(defclass state ()
  ((pc :accessor state-pc :type (or null (unsigned-byte 32)) :initarg :pc)
   (gpr :accessor state-gpr :type list :initarg :gpr)
   (regs :accessor state-regs :type list :initarg :regs)
   (tlb :accessor state-tlb :type list :initarg :tlb)
   (page-size :accessor state-page-size :type (or null (unsigned-byte 32)) :initarg :page-size)
   (virtual-pages :accessor state-virtual-pages :type list :initarg :virtual-pages)
   (phys-pages :accessor state-phys-pages :type list :initarg :phys-pages)
   (phys-cells :accessor state-phys-cells :type list :initarg :phys-cells))
  (:default-initargs
   :pc nil :gpr nil :regs nil :tlb nil :page-size nil :virtual-pages nil :phys-pages nil :phys-cells nil))

(defun capture-core-state (core &key pc page-size regs phys-pages phys-cells capture-tlb)
  (error "~@<Fatal missing functionality: MIPS cop0/hilo capture missing.~:@>")
  (let* ((target (backend core))
         (pc (or pc (devreg (backend target) :pcdec)))
         (gpr (iter (for i below 32) (collect (gpr core i))))
         ;; (regs (iter (for reg in regs) (collect (list reg (mips:reg core reg)))))
         (tlb (when capture-tlb (get-tlb core))))
    (make-instance 'state :pc pc :gpr gpr :regs regs :tlb tlb :page-size page-size
                   :virtual-pages (core-virtual-pages core (tlb-address-map core tlb page-size))
                   :phys-pages (core-phys-pages core phys-pages page-size)
                   :phys-cells (iter (for addr in phys-cells) (collect (list addr (memory-ref target addr)))))))

(defun serialize-core-state (stream state)
  (let ((*print-base* #x10))
    (format stream "~S~%" (list :pc (state-pc state)))
    (format stream "~S~%" (list* :gpr (state-gpr state)))
    (iter (for (reg val) in (state-regs state))
          (format stream "~S~%" (list :regs reg val)))
    (iter (for (addr val) in (state-phys-cells state))
          (format stream "~S~%" (list :phys-cell addr val)))
    (format stream "~S~%" (list :page-size (state-page-size state)))
    (format stream "~S~%" (list :tlb (state-tlb state)))
    (format stream "~S~%" (list :virtual-pages))
    (serialize-extent-list stream (state-virtual-pages state))
    (when (state-phys-pages state)
      (format stream "~S~%" (list :physical-pages))
      (serialize-extent-list stream (state-phys-pages state)))))

(defun write-core-state (state filename)
  (with-open-file (stream filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (serialize-core-state stream state)))

(defun unserialize-core-state (stream)
  (let (pc gpr regs phys-cells page-size tlb virtual-pages phys-pages (*read-base* #x10))
    (iter (for form = (read stream nil nil))
          (while form)
          (ecase (first form)
            (:pc (setf pc (second form)))
            (:gpr (setf gpr (rest form)))
            (:regs (setf regs (rest form)))
            (:phys-cell (push (rest form) phys-cells))
            (:page-size  (setf page-size (second form)))
            (:tlb (setf tlb (second form)))
            (:virtual-pages (setf virtual-pages (read-extent-list stream)))
            (:physical-pages (setf phys-pages (read-extent-list stream)))))
    (make-instance 'state :pc pc :gpr gpr :regs regs :tlb tlb :page-size page-size
                   :virtual-pages virtual-pages :phys-pages phys-pages :phys-cells (nreverse phys-cells))))

(defun apply-core-state (core state)
  (let ((space (device-space core))
        (target (backend core)))
    (with-slots (pc gpr regs phys-cells page-size tlb virtual-pages phys-pages) state
      (iter (for (addr val) in phys-cells)
            (format t "Restoring memory cell at 0x~8,'0X.~%" addr)
            (setf (memory-ref target addr) val))
      (setf (values) (format t "Restoring TLB pages.~%")
            (core-virtual-pages core (tlb-address-map core tlb page-size)) virtual-pages
            (values) (format t "Restoring the zeroth 'magic' page.~%")
            (core-phys-pages core nil page-size) phys-pages
            (values) (format t "Restoring TLB.~%")
            (get-tlb core) tlb)
      (format t "Restoring Cop0.~%")
      (iter (for (reg val) in regs)
            (setf (device-register core (register-id space reg)) val))
      (format t "Restoring GPR.~%")
      (iter (for val in gpr) (for i below 32) 
            (set-gpr core i val))
      (format t "Clearing pipeline and setting PC: ~8,'0X.~%" pc)
      (let ((trail (make-neutral-trail core))
            (moment (make-neutral-moment core pc)))
        (setf (saved-core-trail core) trail
              (core-trail core) trail
              (core-trail-important-p core) nil
              (saved-core-moment core) moment
              (core-moment core) moment))))) ; just done it manually

(defun emit-core-state-restorer (segment state)
  "Emit code to restore STATE (obviously, excluding memory state) into SEGMENT."
  (with-slots (pc gpr regs phys-cells tlb) state
    ;; That's somewhat ugly, but the requirements to shape it didn't have come up yet...
    (with-segment-emission (*mips-isa* segment) ()
      (iter (for (addr val) in phys-cells)
            (emit-store32 val addr))
      (iter (for entry in tlb) (for i from 0)
            (emit-set-tlb-entry i entry)
            (emit-nops 1))
      ;; (iter (for (reg val) in (remove-if (rcurry #'member '(:status :hi :lo)) regs :key #'car))
      ;;       (emit-set-cop0 reg val))
      ;; (if-let ((sr (second (find-if (curry #'eq :status) regs :key #'car))))
      ;;   (emit-set-cop0 :status (logior sr (bits (:exl) (not (null pc))))))
      ;; (when pc
      ;;   (emit-set-cop0 :epc pc))

      ;; Yeah, we know, :ZERO..
      (mapc #'emit-set-gpr
            '(:zero :at :v0 :v1 :a0 :a1 :a2 :a3 :t0 :t1 :t2 :t3 :t4 :t5 :t6 :t7 :s0 :s1 :s2 :s3 :s4 :s5 :s6 :s7 :t8 :t9 :kt0 :kt1 :gp :sp :s8 :ra)
            gpr)
      (when pc
        (emit* :nop)
        (emit* :nop)
        (emit* :eret)
        (emit* :nop)))))
  
(defun core-state-restorer (state &key (entry-point #x1fc00000))
  "Produce a memory extent list suitable for reproduction of STATE, including memory (because we can!).
   ENTRY-POINT specifies the extent list's restoration routine entry point."
  (declare (type (or null (unsigned-byte 32)) entry-point))
  (with-slots (tlb virtual-pages phys-pages page-size) state
    (append
     (list (make-extent 'extent entry-point (emit-core-state-restorer (make-instance 'segment) state)))
     (when virtual-pages
       (unless page-size
         (error "Cannot emit virtual page restore: page size not specified."))
       ;; XXX: virtual pages!
       (mapcar (curry #'rebase ) (tlb-address-map tlb page-size) virtual-pages))
     (list phys-pages))))

(defun write-core-state-restorer-eltext (state filename &key (entry-point #x0) (emit-trampoline t))
  (eltext:write-extents-eltext
   filename
   (append
    (list (core-state-restorer state :entry-point (logandc1 #x80000000 entry-point)))
    (when emit-trampoline
      (list (make-extent 'extent #xbfc00000
                         (segment-active-vector
                          (with-segment-emission (*mips-isa*) ()
                                                 (emit-long-jump (logior #x80000000 entry-point))))))))))
