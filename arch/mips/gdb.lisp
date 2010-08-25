;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GDB; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :gdb)

(defvar *name->gdb-register-id*
  (alexandria:alist-hash-table
   '((:r0 .   0) (:r1  .  1) (:r2  .  2) (:r3  .  3) (:r4  .  4) (:r5  .  5) (:r6  .  6) (:r7  .  7)
     (:r8 .   8) (:r9  .  9) (:r10 . 10) (:r11 . 11) (:r12 . 12) (:r13 . 13) (:r14 . 14) (:r15 . 15)
     (:r16 . 16) (:r17 . 17) (:r18 . 18) (:r19 . 19) (:r20 . 20) (:r21 . 21) (:r22 . 22) (:r23 . 23)
     (:r24 . 24) (:r25 . 25) (:r26 . 26) (:r27 . 27) (:r28 . 28) (:r29 . 29) (:r30 . 30) (:r31 . 31)
     (:status . 32) (:lo . 33) (:hi . 34) (:badvaddr . 35) (:cause . 36) (:pc . 37)
     (:f0  . 38) (:f1  . 39) (:f2  . 40) (:f3  . 41) (:f4  . 42) (:f5  . 43) (:f6  . 44) (:f7  . xs45)
     (:f8  . 46) (:f9  . 47) (:f10 . 48) (:f11 . 49) (:f12 . 50) (:f13 . 51) (:f14 . 52) (:f15 . 53)
     (:f16 . 54) (:f17 . 55) (:f18 . 56) (:f19 . 57) (:f20 . 58) (:f21 . 59) (:f22 . 60) (:f23 . 61)
     (:f24 . 62) (:f25 . 63) (:f26 . 64) (:f27 . 65) (:f28 . 66) (:f29 . 67) (:f30 . 68) (:f31 . 69)
     (:fcsr . 70) (:fir . 71))))

(defmethod core-register-id ((o mips-core) name)
  (gethash name *name->gdb-register-id*))

(defmethod describe-core ((o mips-core) reginstance-cb &aux
                          (space (space :core)))
  (with-html-output-to-string (s)
    (flet ((export-layout (name type group prefer-aliases &rest layouts-or-registers)
             (htm (:feature :name name (terpri s)
                            (iter (for l-or-r in layouts-or-registers)
                                  (destructuring-bind (l &rest rs) (ensure-cons l-or-r)
                                    (labels ((resolve-ri (x)
                                               (etypecase x
                                                 (register-instance x)
                                                 (keyword (find x (device-layout-register-instances o l) :key #'name))
                                                 (cons (cons (resolve-ri (car x))
                                                             (cdr x))))))
                                      (dolist (rispec (if rs
                                                          (mapcar #'resolve-ri rs)
                                                          (reverse (device-layout-register-instances o l))))
                                        (destructuring-bind (ri &optional name) (ensure-cons rispec)
                                          (let ((name (or name
                                                          (and prefer-aliases
                                                               (first (reginstance-aliases ri)))
                                                          (name ri))))
                                            (funcall reginstance-cb ri name)
                                            (str (describe-register o name 32 (core-register-id o name) type group))))))))))
             (terpri s)))
      (export-layout "org.gnu.gdb.mips.cpu" nil    :general t
                     (layout space :gpr)
                     (list (layout space :hilo) :hi :lo)
                     (layout space :control))
      (export-layout "org.gnu.gdb.mips.fpu" :ieee_single :float   nil
                     (layout space :fpr)
                     (layout space :cop1control))
      (export-layout "org.gnu.gdb.mips.cp0" nil    :general nil
                     (list (layout space :cop0) :badvaddr :status :cause :epc))
      (when (next-method-p)
        (call-next-method)))))

(defmethod describe-memory-map ((o mips-core) &aux
                                (target (backend o)))
  (let ((extents (mapcar #'memory-region-extent
                         (append (target-devices-by-type target 'ram)
                                 (target-devices-by-type target 'internal-memory)))))
    (with-output-to-string (str)
      (dolist (seg (list mips:kuseg mips:x32kseg0 mips:x32kseg1))
        (let ((extents (mapcar (lambda (e)
                                 (if (= seg mips:kuseg)
                                     (mips:extent-to-seg32 seg e)
                                     (mips:extent-to-x32seg32 seg e)))
                               extents)))
          (dolist (e extents)
            (write-line (describe-core-memory-region o "ram" (base e) (size e)) str)))))))

(defmethod core-register-order ((o mips-core))
  "As per http://sourceware.org/gdb/current/onlinedocs/gdb/Architecture_002dSpecific-Protocol-Details.html"
  (list :r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15 :r16 :r17 :r18 :r19 :r20 :r21 :r22 :r23 :r24 :r25 :r26 :r27 :r28 :r29 :r30 :r31
        :status :lo :hi :badvaddr :cause :pc
        :f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15 :f16 :f17 :f18 :f19 :f20 :f21 :f22 :f23 :f24 :f25 :f26 :f27 :f28 :f29 :f30 :f31
        :fcsr :fir))