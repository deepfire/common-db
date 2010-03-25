;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GENERIC; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :generic)


(define-protocol-device-class memory-device nil (bioable)
  ()
  (:documentation
   "This mixin introduces the memory device protocol, that is,
    a set of functions for doing I/O at specific device offsets
    with common granularities -- 8, 16, 32 and 64 bits."))

(define-protocol-device-class memory-region nil (bioable)
  ((extent :accessor memory-region-extent :initarg :extent)))
(define-protocol-device-class slave-memory-region nil (slave-device memory-region)
  ())

(define-protocol-device-class 8bit-memory-device nil (memory-device) ())
(define-protocol-device-class 16bit-memory-device nil (memory-device) ())
(define-protocol-device-class 32bit-memory-device nil (memory-device) ())
(define-protocol-device-class 64bit-memory-device nil (memory-device) ())
(define-protocol-device-class little-endian-memory-device nil (memory-device) ())
(define-protocol-device-class big-endian-memory-device nil (memory-device) ())
(define-protocol-device-class bioable-memory-device nil (memory-device) ())

(defgeneric memory-device-8bit-ref (memory-device offset))
(defgeneric memory-device-8bit-set (memory-device offset val))
(defgeneric memory-device-16bit-ref (memory-device offset))
(defgeneric memory-device-16bit-set (memory-device offset val))
(defgeneric memory-device-32bit-ref (memory-device offset))
(defgeneric memory-device-32bit-set (memory-device offset val))
(defgeneric memory-device-64bit-ref (memory-device offset))
(defgeneric memory-device-64bit-set (memory-device offset val))

(defgeneric memory-device-byte-width (memory-device)
  (:method ((o  8bit-memory-device)) 1)
  (:method ((o 16bit-memory-device)) 2)
  (:method ((o 32bit-memory-device)) 4)
  (:method ((o 64bit-memory-device)) 8))

(defgeneric memory-ref (memory-device offset)
  (:method ((o  8bit-memory-device) (offset integer)) (memory-device-8bit-ref o offset))
  (:method ((o 16bit-memory-device) (offset integer)) (memory-device-16bit-ref o offset))
  (:method ((o 32bit-memory-device) (offset integer)) (memory-device-32bit-ref o offset))
  (:method ((o 64bit-memory-device) (offset integer)) (memory-device-64bit-ref o offset)))

(defgeneric memory-set (memory-device offset value)
  (:method ((o  8bit-memory-device) (offset integer) (value integer)) (memory-device-8bit-set o offset value))
  (:method ((o 16bit-memory-device) (offset integer) (value integer)) (memory-device-16bit-set o offset value))
  (:method ((o 32bit-memory-device) (offset integer) (value integer)) (memory-device-32bit-set o offset value))
  (:method ((o 64bit-memory-device) (offset integer) (value integer)) (memory-device-64bit-set o offset value)))

(defsetf memory-device-8bit-ref memory-device-8bit-set)
(defsetf memory-device-16bit-ref memory-device-16bit-set)
(defsetf memory-device-32bit-ref memory-device-32bit-set)
(defsetf memory-device-64bit-ref memory-device-64bit-set)

(defun (setf memory-ref) (value memory-device offset)
  (memory-set memory-device offset value))

(defgeneric merge-u8-extremity (memdev base vector length headp writep)
  (:method ((o little-endian-memory-device) base vector length headp writep)
    (declare (type (vector (unsigned-byte 8)) vector))
    (let ((width (memory-device-byte-width o))
          (extremity (if headp base (+ base length))))
      (with-alignment (granule-base left right mask) width extremity
        (unless (= granule-base extremity)
          (let* ((exvec (make-array width :element-type '(unsigned-byte 8))))
            (setf (u8-vector-wordle exvec 0 width) (memory-ref o granule-base))
            (operate-on-extremity length headp left right
                                  (if writep
                                      (lambda (g i) (setf (aref exvec g) (aref vector i)))
                                      (lambda (g i) (setf (aref vector i) (aref exvec g)))))
            (when writep
              (setf (memory-ref o granule-base) (u8-vector-wordle exvec 0 width))))))))
  (:method ((o big-endian-memory-device) base vector length headp writep)
    (declare (type (vector (unsigned-byte 8)) vector))
    (let ((width (memory-device-byte-width o))
          (extremity (if headp base (+ base length))))
      (with-alignment (granule-base left right mask) width extremity
        (unless (= granule-base extremity)
          (let* ((exvec (make-array width :element-type '(unsigned-byte 8))))
            (setf (u8-vector-wordbe exvec 0 width) (memory-ref o granule-base))
            (operate-on-extremity length headp left right
                                  (if writep
                                      (lambda (g i) (setf (aref exvec g) (aref vector i)))
                                      (lambda (g i) (setf (aref vector i) (aref exvec g)))))
            (when writep
              (setf (memory-ref o granule-base) (u8-vector-wordbe exvec 0 width)))))))))

(defgeneric bioable-memory-io (device address vector size writep)
  (:method ((o bioable-memory-device) address vector size writep)
    (declare (type (vector (unsigned-byte 8)) vector) (type integer address size) (type boolean writep))
    (merge-u8-extremity o address vector size t writep)
    (with-alignment (nil nil head) (memory-device-byte-width o) address
      (with-alignment (nil tail) (memory-device-byte-width o) (+ address size)
        (if writep
            (write-aligned-block o (+ address head) vector head (- size head tail)) 
            (read-aligned-block o (+ address head) vector head (- size head tail)))))
    (merge-u8-extremity o address vector size nil writep)))

;;;;
;;;; Mapped device: base, scale, set-fn, get-fn, device-to-backend-fn
;;;;
(define-protocol-device-class mapped-device nil ()
  ((base :accessor mapped-device-base :initarg :base)
   (scale :accessor mapped-device-scale :initarg :scale)
   (mutator-fn :accessor mapped-device-mutator-fn :initform #'identity :initarg :mutator-fn)
   (get-fn :accessor mapped-device-get-fn :initarg :get-fn)
   (set-fn :accessor mapped-device-set-fn :initarg :set-fn))
  (:default-initargs :scale 4))

(defun mapped-device-p (x)
  (typep x 'mapped-device))

(defmethod print-object ((device mapped-device) stream)
  (labels ((slot (id) (if (slot-boundp device id) (slot-value device id) :unbound-slot)))
    (format stream "~@<#<~;~A-~A base: ~8,'0X backend: ~S~;>~:@>" (type-of device) (slot 'id) (slot 'base) (slot 'backend))))

(defun mapped-ref (device offset)
  (declare (type mapped-device device) (type (integer 0) offset))
  (funcall (mapped-device-get-fn device)
           (funcall (mapped-device-mutator-fn device) device)
           (+ (mapped-device-base device) (* (mapped-device-scale device) offset))))

(defun (setf mapped-ref) (val device offset)
  (declare (type mapped-device device) (type (integer 0) val offset))
  (funcall (mapped-device-set-fn device)
           val
           (funcall (mapped-device-mutator-fn device) device)
           (+ (mapped-device-base device) (* (mapped-device-scale device) offset))))

;; Does not check whether the device has such a register.. produces a run-time error instead ;-)
(defun mapped-device-register-address (device register-name)
  (+ (mapped-device-base device) (* (mapped-device-scale device)
                                    (device-register-selector device (register-id (device-space device) register-name)))))
