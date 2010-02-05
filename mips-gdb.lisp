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


(defmethod describe-core ((o mips-core) &aux
                          (space (space :core)))
  (with-html-output-to-string (s)
    (flet ((export-layout (name group prefer-aliases &rest layouts-or-registers)
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
                                            (str (describe-register o name 32 (reginstance-id ri) group))))))))))
             (terpri s)))
      (export-layout "org.gnu.gdb.mips.cpu" :general t
                     (layout space :gpr)
                     (list (layout space :hilo) :hi :lo)
                     (list nil `(,(device-register-instance (backend (backend o)) :pcdec) :pc)))
      (export-layout "org.gnu.gdb.mips.fpu" :general nil
                     (layout space :fpr)
                     (layout space :cop1))
      (export-layout "org.gnu.gdb.mips.cp0" :general nil
                     (list (layout space :cop0) :badvaddr :status :cause))
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
  (list :r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15 :r16 :r17 :r18 :r19 :r20 :r21 :r22 :r23 :r24 :r25 :r26 :r27 :r28 :r29 :r30 :r31
        :hi :lo :pc
        :f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15 :f16 :f17 :f18 :f19 :f20 :f21 :f22 :f23 :f24 :f25 :f26 :f27 :f28 :f29 :f30 :f31
        :fir :fcsr
        :badvaddr :status :cause))