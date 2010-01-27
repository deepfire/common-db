;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ADDRESS-MAP; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :address-map)

(defstruct page
  (virt nil :type (integer 0))
  (phys nil :type (integer 0)))

(defstruct (address-map (:print-object
                         (lambda (map stream &aux maps)
                           (maphash (lambda (v p) (push (cons v p) maps)) (address-map-%virt-to-phys map))
                           (let ((*print-base* #x10) (*print-length* nil))
                             (format stream "~@<#<MAP~; page-size: 0x~X maps: ~S~;>~:@>" (address-map-page-size map) maps)))))
  (page-size nil :type (integer 0))
  (%virt-to-phys (make-hash-table) :type hash-table)
  (%phys-to-virt (make-hash-table) :type hash-table))

(defun page-align (address page-size)
  "Align the ADDRESS by PAGE-SIZE. PAGE-SIZE must be a power of two."
  (declare (type (integer 0) address page-size))
  (logandc1 (1- page-size) address))

(defun %address-map-add (map virt phys)
  (declare (type (integer 0) virt phys))
  (setf (gethash virt (address-map-%virt-to-phys map)) phys
        (gethash phys (address-map-%phys-to-virt map)) virt))

(defun address-map-add (map virt phys)
  (declare (type (integer 0) virt phys))
  (unless (zerop (nth-value 1 (ceiling (logior virt phys) (address-map-page-size map))))
    (error "Addresses must be page size aligned, in this particular case -- by 0x~X, was virt: 0x~X, phys: 0x~X."
           (address-map-page-size map) virt phys))
  (%address-map-add map virt phys))

(define-condition address-map-miss () 
  ((map :accessor address-map-miss-map :type address-map :initarg :map)
   (address :accessor address-map-miss-address :type (integer 0) :initarg :address)))

(define-condition address-map-virt-miss (address-map-miss) ()
  (:report (lambda (condition stream)
             (format stream "Address map ~S has no map for virtual address 0x~X" 
                     (address-map-miss-map condition) (address-map-miss-address condition)))))

(define-condition address-map-phys-miss (address-map-miss) ()
  (:report (lambda (condition stream)
             (format stream "Address map ~S has no map for physical address 0x~X" 
                     (address-map-miss-map condition) (address-map-miss-address condition)))))

(defun %virt-to-phys (map addr)
  (declare (type (integer 0) addr))
  (gethash addr (address-map-%virt-to-phys map)))

(defun %phys-to-virt (map addr)
  (declare (type (integer 0) addr))
  (gethash addr (address-map-%phys-to-virt map)))

(defun virt-to-phys (map addr &aux (page-size (address-map-page-size map)))
  (declare (type (integer 0) addr))
  (logior (or (%virt-to-phys map (page-align addr page-size))
              (error 'address-map-virt-miss :map map :address addr))
          (logand (1- page-size) addr)))

(defun addr-in-map-p (map addr &aux (page-size (address-map-page-size map)))
  (declare (type (integer 0) addr))
  (not (null (%virt-to-phys map (page-align addr page-size)))))

(defun phys-to-virt (map addr &aux (page-size (address-map-page-size map)))
  (declare (type (integer 0) addr))
  (logior (or (%phys-to-virt map (page-align addr page-size))
              (error 'address-map-phys-miss :map map :address addr))
          (logand (1- page-size) addr)))

(defun copy-mapping-by-phys-addr (source-map dest-map phys)
  (declare (type (integer 0) phys))
  (%address-map-add dest-map (%phys-to-virt source-map phys) phys))

(defun copy-mapping-by-virt-addr (source-map dest-map virt)
  (declare (type (integer 0) virt))
  (%address-map-add dest-map virt (%virt-to-phys source-map virt)))

(defun trim-address-map-virtual (map address-list)
  (let* ((page-size (address-map-page-size map))
         (new-map (make-address-map :page-size page-size)))
    (dolist (addr address-list)
      (copy-mapping-by-virt-addr map new-map (page-align addr page-size)))
    new-map))

(defun trim-address-map-physical (map address-list)
  (let* ((page-size (address-map-page-size map))
         (new-map (make-address-map :page-size page-size)))
    (dolist (addr address-list)
      (copy-mapping-by-phys-addr map new-map (page-align addr page-size)))
    new-map))

(defun map-address-map (fn map)
  "Call FN on every page in MAP, passing it the virtual and physical addresses."
  (maphash fn (address-map-%virt-to-phys map)))
