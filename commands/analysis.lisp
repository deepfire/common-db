;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :common-db)


(defun snapshot-gprs (core)
  #+help-ru
  "Произвести снимок регистрового файла GPR."
  #-help-ru
  "Produce a GPR register file snapshot."
  (map 'vector (curry #'gpr core) (iota 32)))

(defun compare-gpr-snaplists (s0 s1 &optional ignores)
  #+help-ru
  "Сравнить списки снимков регистровых файлов S0 и S1, за возможным исключением
регистров с номерами из списка IGNORES."
  #-help-ru
  "Compare register file snapshots S0 and S1, possibly excluding registers 
with numbers contained in IGNORES."
  (iter (for s0elt in s0) (for s1elt in s1)
        (for i from 0)
        (dotimes (j 32)
          (unless (or (= (aref s0elt j) (aref s1elt j))
                      (member j ignores))
            (format t "snap ~D: difference in r~D: ~8,'0X vs ~8,'0X~%"
                    i j (aref s0elt j) (aref s1elt j))))))

(defun snaptrace (addr-or-sym &optional nsnaps (core *core*))
  #+help-ru
  "Произвести трассировку со съёмом снимков регистрового файла при попадании
на адрес заданный ADDR-OR-SYM."
  #-help-ru
  "Perform a trace by setting a breakpoint on ADDR-OR-SYM, and running either 
indefinitely, or until NSNAPS stops happen, taking a state snapshot every time
a stop happens."
  (with-free-hardware-breakpoints (core) ((b (coerce-to-address addr-or-sym)))
    (iter (for i from 0)
          (when (and nsnaps (= i nsnaps))
            (return snapshots))
          (run :core core)
          (collect (snapshot-gprs core) into snapshots)
          (finally (return snapshots)))))