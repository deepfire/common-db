;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ELTEXT; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :eltext)

(defun read-extents-eltext (stream &key (endianness :little-endian))
  (lret ((accessor-fn (case endianness
                        (:little-endian #'(setf u8-vector-word32le))
                        (:big-endian #'(setf u8-vector-word32be))))
         acc
         (extent-length 0)
         prev-address)
    (flet ((pick-accumulated-extent ()
             (when acc
               (let ((data (make-array extent-length :element-type '(unsigned-byte 8))))
                 (iter (for (word0 word1 word2 word3) in acc) (for i from (- extent-length 16) downto 0 by 16)
                       (funcall accessor-fn word0 data (+ i 0))
                       (funcall accessor-fn word1 data (+ i 4))
                       (funcall accessor-fn word2 data (+ i 8))
                       (funcall accessor-fn word3 data (+ i 12)))
                 (make-extent 'extent (- prev-address extent-length -16) data)))))
      (iter (for string = (read-line stream nil nil)) (for i from 0)
            (unless string
              (when-let ((extent (pick-accumulated-extent)))
                (collect extent into extents))
              (leave extents))
            (when (< (length string) 48)
              (error "a string too short, at line ~D~%" i))
            (let ((word0 (parse-integer string :start 0 :end 8 :radix #x10))
                  (word1 (parse-integer string :start 9 :end 17 :radix #x10))
                  (word2 (parse-integer string :start 18 :end 26 :radix #x10))
                  (word3 (parse-integer string :start 27 :end 35 :radix #x10))
                  (address (parse-integer string :start 40 :end 48 :radix #x10)))
              (if-first-time (setf prev-address (- address 16)))
              (unless (= address (+ prev-address 16))
                (when-let ((extent (pick-accumulated-extent)))
                  (collect extent into extents))
                (setf acc nil
                      extent-length 0))
              (push (list word0 word1 word2 word3) acc)
              (incf extent-length 16)
              (setf prev-address address))))))

(defun u8-extent-print-as-eltext (extent stream &optional (endianness :little-endian))
  (let ((data (extent-data extent)))
    (check-address-alignment 4 (base extent))
    (check-size-alignment 4 (size extent))
    (let* ((start 0) (end (length data))
           (total (- end start))
           (base-address (base extent))
           (head (logand #xf base-address))
           (tail (logand #xf (- end head)))
           (main (- total head tail))
           (accessor-fn (case endianness
                          (:little-endian #'u8-vector-word32le)
                          (:big-endian #'u8-vector-word32be))))
      (unless (zerop head)
        (iter (for i below (ash (- #x10 head) -2))
              (format stream "00000000 "))
        (iter (for i below head by 4)
              (format stream "~8,'0X " (funcall accessor-fn data i)))
        (format stream "//0x~8,'0X~%" (logandc1 #xf base-address)))
      (iter (for i from head below main by 16)
            (for address from (+ head base-address) by 16)
            (format stream "~8,'0X ~8,'0X ~8,'0X ~8,'0X //0x~8,'0X~%"
                    (funcall accessor-fn data (+ i 0)) (funcall accessor-fn data (+ i 4))
                    (funcall accessor-fn data (+ i 8)) (funcall accessor-fn data (+ i 12))
                    address))
      (unless (zerop tail)
        (let ((tailstart (+ head main)))
          (iter (for i from tailstart below total by 4)
                (format stream "~8,'0X " (funcall accessor-fn data i)))
          (iter (for i below (ash (- #x10 tail) -2))
                (format stream "00000000 "))
          (format stream "//0x~8,'0X~%" (+ base-address tailstart)))))))

(defun print-extents-eltext (stream list &key (endianness :little-endian))
  (mapcar (rcurry #'u8-extent-print-as-eltext stream endianness) list))

(defun load-extents-eltext (filename &key (endianness :little-endian))
  (with-open-file (stream filename :direction :input)
    (read-extents-eltext stream :endianness endianness)))

(defun write-extents-eltext (filename extents &key (endianness :little-endian))
  (with-open-file (stream filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (print-extents-eltext stream extents :endianness endianness)))

;;;
;;; LOADABLE
;;;
(defmethod extract-loadable ((type (eql :eltext)) filename &key (entry-point #xbfc00000))
  (loadable:make-loadable (load-extents-eltext filename) :filename filename :entry-point entry-point))
