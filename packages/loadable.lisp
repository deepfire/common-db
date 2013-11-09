;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:loadable ;; needed by common-db
  (:use :common-lisp :alexandria :pergamum :iterate :bintype :elf
        :options :bank)
  (:shadowing-import-from :options #:arg)
  (:export
   #:loadable
   #:loadable-entry-point
   #:loadable-sections
   #:loadable-load-size
   #:loadable-error
   #:make-loadable
   #:extract-loadable
   #:report-section
   #:report-checksummed-section
   #:dump-section
   #:upload-loadable))

