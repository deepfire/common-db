;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2008, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2007-2008 by
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


(defun loadsyms (pathname &key (type :system-map))
  #+help-ru
  "Загрузить таблицу символов в формате TYPE из файла PATHNAME,
и сделать эту таблицу активной."
  #-help-ru
  "Load symbols from file pointed by PATHNAME, interpreting it as TYPE,
which defaults to :SYSTEM-MAP (and is the only accepted type as of now."
  (ecase type
    (:system-map
     (push (sym:load-system-map (namestring pathname) :reuse-package t :imbue-symbols t) *syms*)))
  (values))

(defun symaddr (symbol)
  #+help-ru
  "Вернуть адрес соответствующий SYMBOL."
  #-help-ru
  "Resolve SYM using the first symbol table in =*SYMS*=."
  (declare (type symbol symbol))
  (when-let ((table (first *syms*)))
    (values (sym:addr table symbol) table)))

;; Before looking for bugs, ensure there are no duplicate symbols.
(defun addrsym (address)
  #+help-ru
  "Вернуть символ с наименьшим адресом, большим или равным ADDRESS."
  #-help-ru
  "Find a symbol preceding ADDRESS, using the first symbol table in =*SYMS*=."
  (declare (type (unsigned-byte 32)))
  (when-let ((table (first *syms*)))
    (values (sym:name table address) table)))

(defun nextsym (symbol)
  #+help-ru
  "Вернуть символ следующий в активной таблице символов за SYMBOL."
  #-help-ru
  "Find a symbol following SYMBOL, using the first symbol table in =*SYMS*=."
  (declare (type symbol symbol))
  (when-let ((table (first *syms*)))
    (values (sym:next-name symbol table) table)))

(defun prevaddr (address)
  #+help-ru
  "Вернуть адрес символа предшедствующего ADDRESS в активной таблице символов."
  #-help-ru
  "Find address of symbol preceding ADDRESS, using the first symbol table
in =*SYMS*=."
  (declare (type (unsigned-byte 32)))
  (when-let ((table (first *syms*)))
    (multiple-value-bind (sym address) (oct-1d:tree-left address (sym::symtable-store table))
      (values address sym))))

(defun nextaddr (address)
  #+help-ru
  "Вернуть адрес символа следующего за ADDRESS в активной таблице символов."
  #-help-ru
  "Find address of symbol following ADDRESS, using the first symbol table
in =*SYMS*=."
  (declare (type (unsigned-byte 32)))
  (when-let ((table (first *syms*)))
    (multiple-value-bind (sym address) (oct-1d:tree-right address (sym::symtable-store table))
      (values address sym))))

(defun symlength (symbol)
  #+help-ru
  "Вернуть разницу между адресами SYMBOL и символа следующего за ним
в активной таблице символов."
  #-help-ru
  "Find the length of the span covered by SYMBOL, in the context of the
first symbol table in =*SYMS*=."
  (when-let ((next (nextsym symbol)))
    (- (symaddr next) (symaddr symbol))))

(defun addr (address-or-symbol)
  #+help-ru
  "Проанализировать адрес заданный через спецификатор адреса ADDRESS-OR-SYMBOL,
и выдать детальную информацию."
  #-help-ru
  "Examine ADDRESS-OR-SYMBOL and give detailed information about it."
  (when-let ((table (first *syms*)))
    (let* ((symp (symbolp address-or-symbol))
           (addrs (if (integerp address-or-symbol)
                      (list address-or-symbol)
                      (sym:addr* table address-or-symbol))))
      (if addrs
          (let* ((sym (xform (not symp) #'addrsym address-or-symbol))
                 (baseaddrs (if symp
                                addrs
                                (let ((candidates (sym:addr* table sym)))
                                  (list (lastcar (sort (copy-list (remove-if (curry #'< address-or-symbol) candidates)) #'<))))))
                 (nextaddrs (mapcar #'nextaddr baseaddrs)))
            (if symp
                (iter (for a in addrs)
                      (for n in nextaddrs)
                      (for l = (when (and a n) (- n a)))
                      (format t "~A: ~8,'0X-~8,'0X, total ~X~%" sym a (when l (+ a l)) l))
                (format t "~8,'0X: ~X from start of ~A (~8,'0X-~8,'0X, total ~X)~%"
                        (first addrs) (- (first addrs) (first baseaddrs)) sym
                        (first baseaddrs) (first nextaddrs) (- (first nextaddrs) (first baseaddrs)))))
          (error "~@<Unknown symbol ~A.~:@>" address-or-symbol))))
  (values))

(define-condition address-not-resolved-error (error)
  ((address :reader address-resolution-condition-address :initarg :address))
  (:report (lambda (cond stream)
             (format stream "Address ~8,'0X could not be resolved by available symbol tables."
                     (address-resolution-condition-address cond)))))

(defun coerce-to-address (address-or-symbol &key (if-not-found :error))
  #+help-ru
  "Преобразовать спецификатор адреса ADDRESS-OR-SYMBOL в адрес."
  #-help-ru
  "Return the address corresponding to ADDRESS-OR-SYMBOL, resolving it using
the first symbol table in =*SYMS*= when it is a symbol."
  (etypecase address-or-symbol
    (symbol
     (if-let ((addr (symaddr address-or-symbol)))
       addr
       (ecase if-not-found
         (:continue)
         (:error (error 'address-not-resolved-error :address address-or-symbol)))))
    (integer
     address-or-symbol)))
