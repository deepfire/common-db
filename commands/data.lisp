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

(defun generate-extent (start-address length value-fn &key (stride 4) big-endian-p)
  #-help-ru
  "Generate an extent, beginning at START-ADDRESS and LENGTH-byte-long,
using VALUE-FN, which expects two arguments, namely the address, and the 
iteration number.
STRIDE determines both the address increment and the width of the generated 
value and must be a factor of LENGTH.
BIG-ENDIAN-P determines endianness."
  #+help-ru
  "Сгенерировать экстент, начинающийся по START-ADDRESS и состоящий из LENGTH байт,
используя значения возвращаемые VALUE-FN, ожидающей два аргумента, а именно адрес
и номер итерации.
STRIDE определяет как икремент адреса, так и ширину значения генерируемого VALUE-FN,
и должно быть делителем LENGTH.
BIG-ENDIAN-P опредеяет порядок байт в слове."
  (lret ((extent (make-extent 'extent start-address length)))
    (iter (for addr from start-address by stride)
          (for offset from 0 by stride)
          (for i from 0)
          (repeat (/ length stride))
          (let ((value (funcall value-fn addr i)))
            (if big-endian-p
                (setf (u8-vector-wordbe (extent-data extent) offset stride) value)
                (setf (u8-vector-wordle (extent-data extent) offset stride) value))))))

(defun generate-file (filename extent-spec-list &optional (type :bank) (stride 4) big-endian-p)
  #-help-ru
  "Generate FILENAME as a loadable of specified TYPE, according to the EXTENT-SPEC-LIST,
which is a list of three-element lists, which are, correspondingly, start addresses, lengths 
and function designators and are passed to GENERATE-EXTENT.
TYPE can be either :BANK, or :RAW, with the former being the default.
STRIDE and BIG-ENDIAN-P have the same semantics as in GENERATE-EXTENT."
  #+help-ru
  "Сгенерировать FILENAME в формате загружаемого объекта TYPE, в соответствии с EXTENT-SPEC-LIST,
являющегося списком трёхэлементных списков, содержащих, соответственно, стартовые адреса, длины
и спецификаторы функций, и передаываемых GENERATE-EXTENT.
TYPE может быть либо :BANK, либо :RAW, при чём :BANK является значением по умолчанию.
STRIDE и BIG-ENDIAN-P имеют тот же смысл как и в GENERATE-EXTENT."
  (ecase type
    (:raw
     (iter (for (base length gen-fn) in extent-spec-list)
           (with-open-file (s (format nil "~A.~X" filename base))
             (write-sequence (extent-data (generate-extent base length gen-fn :stride stride :big-endian-p big-endian-p)) s))))
    (:bank
     (bank:write-extents-as-bank filename
                                 (iter (for (base length gen-fn) in extent-spec-list)
                                       (collect (generate-extent base length gen-fn :stride stride :big-endian-p big-endian-p))))))
  (values))