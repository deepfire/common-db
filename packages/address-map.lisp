;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:address-map ;; needed-by mips, common-db
  (:use :common-lisp :pergamum
        :options)
  (:export
   #:page-align
   #:address-map
   #:make-address-map
   #:address-map-page-size
   #:address-map-add
   #:address-map-miss
   #:virt-to-phys
   #:addr-in-map-p
   #:phys-to-virt
   #:copy-mapping-by-virt-addr
   #:copy-mapping-by-phys-addr
   #:trim-address-map-virtual
   #:trim-address-map-physical
   #:map-address-map))

