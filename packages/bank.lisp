;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:bank ;; standalone
  (:use :common-lisp :alexandria :iterate :pergamum
        :options)
  (:shadowing-import-from :options #:arg)
  (:export
   #:read-bank-extents
   #:u8-extent-print-as-bank
   #:print-bank-extents
   #:load-bank-extents
   #:write-extents-as-bank))

