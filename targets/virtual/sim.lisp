;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DUMMYCORE; Base: 10; indent-tabs-mode: nil -*-
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


;;;;
;;;; Core
;;;;
(defmethod core-frequency-multiplier ((o virtcore))
  2)

;;;;
;;;; Platform
;;;;
(defmethod configure-platform-system ((p virtual-platform) (o virtsys) &key &allow-other-keys))

;;;;
;;;; Memory simulation
;;;;
(defconstant memory-poison #xef)
(defconstant memory-block-words #x40)
(defconstant memory-block-bytes (ash memory-block-words 2))
(defconstant memory-addr-shift -8)
(defconstant memory-interblock-addr-mask #xfc)
(defconstant memory-block-base-mask #xffffff00)

(declaim (ftype (function ((unsigned-byte 32)) fixnum) address-hash-key))
(defun address-hash-key (address)
  (ash address memory-addr-shift))

(defun populate-memory (hash key)
  (declare (type fixnum key))
  (lret* ((size (ash memory-block-words 2))
          (block (make-array size :element-type '(unsigned-byte 8) :initial-element memory-poison)))
    (setf (gethash key hash) block)))

(defun get-block (target address &aux
                  (key (address-hash-key address)))
  (or (gethash key (target-memory target))
      (populate-memory (target-memory target) key)))

(defun (setf get-block) (block target address &aux
                         (key (address-hash-key address)))
  (setf (gethash key (target-memory target)) block))

(defun clear-memory (target)
  (clrhash (target-memory target)))

(defmethod memory-device-32bit-ref ((o generic-virtual-target) address &aux
                                    (address (fixmap-address o address)))
  (u8-vector-word32le (get-block o address) (logand address memory-interblock-addr-mask)))

(defmethod memory-device-32bit-set ((o generic-virtual-target) address val &aux
                                    (address (fixmap-address o address)))
  (setf (u8-vector-word32le (get-block o address) (logand address memory-interblock-addr-mask)) val))

(defmethod read-block ((o generic-virtual-target) base vector &optional start end &aux
                       (base (fixmap-address o (+ base start)))
                       (len (- end start)))
  (declare (type (vector (unsigned-byte 8)) vector))
  (with-aligned-extent-spec-pieces memory-block-bytes 
      ((nil nil) (headstart headlen) (corestart corelen) (tailstart taillen)) (extent base len)
    (when (plusp headlen)
      (setf (subseq vector start (+ start headlen))
            (subseq (get-block o headstart) (logand headstart memory-interblock-addr-mask))))
    (iter (for coreaddr from corestart by memory-block-bytes below tailstart)
          (for vecaddr from start by memory-block-bytes)
          (setf (subseq vector vecaddr (+ vecaddr memory-block-bytes)) (get-block o coreaddr)))
    (when (plusp taillen)
      (setf (subseq vector (+ start headlen len))
            (subseq (get-block o tailstart) 0 (logand (+ tailstart taillen) memory-interblock-addr-mask))))))

(defmethod write-block ((o generic-virtual-target) base vector &optional start end &aux
                        (base (fixmap-address o (+ base start)))
                        (len (- end start)))
  (declare (type (vector (unsigned-byte 8)) vector))
  (with-aligned-extent-spec-pieces memory-block-bytes 
      ((nil nil) (headstart headlen) (corestart corelen) (tailstart taillen)) (extent base len)
    (when (plusp headlen)
      (setf (subseq (get-block o headstart) (logand headstart memory-interblock-addr-mask))
            (subseq vector start (+ start headlen))))
    (iter (for coreaddr from corestart by memory-block-bytes below tailstart)
          (for vecaddr from start by memory-block-bytes)
          (setf (get-block o coreaddr) (subseq vector vecaddr (+ vecaddr memory-block-bytes))))
    (when (plusp taillen)
      (setf (subseq (get-block o tailstart) 0 (logand (+ tailstart taillen) memory-interblock-addr-mask))
            (subseq vector (+ start headlen len))))))

(defmethod detect-platform-memory-size ((o virtual-platform) base &key &allow-other-keys)
  #x1000000)
