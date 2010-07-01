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

(in-package :mips)


(defclass mips-state (state)
  ())

(defmethod emit-nonmemory-state-restorer (segment (o mips-state))
  (with-slots (pc gpr regs physical-cells tlb) o
    ;; That's somewhat ugly, but the requirements to shape it didn't have come up yet...
    (with-segment-emission (*mips-isa* segment) ()
      (iter (for (addr val) in physical-cells)
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
  
(defun state-restorer-as-memory (core state &key (entry-point (default-core-pc core)))
  "Produce a memory extent list suitable for reproduction of STATE on CORE, including memory (because we can!).
ENTRY-POINT specifies the extent list's restoration routine entry point."
  (declare (type (or null (unsigned-byte 32)) entry-point))
  (with-slots (tlb virtual-pages physical-pages page-size) state
    (append
     (list (make-extent 'extent entry-point (emit-nonmemory-state-restorer (make-instance 'segment) state)))
     (when virtual-pages
       (unless page-size
         (error "Cannot emit virtual page restore: page size not specified."))
       ;; XXX: virtual pages!
       (mapcar (curry #'rebase ) (tlb-address-map core tlb page-size) virtual-pages))
     (list physical-pages))))

(defun write-state-restorer-eltext (core state filename &key (entry-point #x0) (emit-trampoline t))
  (eltext:write-extents-eltext
   filename
   (append
    (list (state-restorer-as-memory core state :entry-point (logandc1 #x80000000 entry-point)))
    (when emit-trampoline
      (list (make-extent 'extent (default-core-pc core)
                         (segment-active-vector
                          (with-segment-emission (*mips-isa*) ()
                                                 (emit-long-jump (logior #x80000000 entry-point))))))))))
