;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:generic ;; needed by target, platform, common-db, test-hardware
  (:use :common-lisp :pergamum :bitmop :device-model
        :options)
  (:shadowing-import-from :bitmop #:space)
  (:export
   ;; memory-device
   #:memory-device
   #:little-endian-memory-device
   #:big-endian-memory-device
   #:8bit-memory-device
   #:16bit-memory-device
   #:32bit-memory-device
   #:64bit-memory-device
   #:bioable-memory-device
   #:memory-device-byte-width
   #:memory-device-8bit-ref
   #:memory-device-8bit-set
   #:memory-device-16bit-ref
   #:memory-device-16bit-set
   #:memory-device-32bit-ref
   #:memory-device-32bit-set
   #:memory-device-64bit-ref
   #:memory-device-64bit-set
   #:memory-ref
   #:memory-set
   #:merge-u8-extremity
   #:bioable-memory-io
   ;; mapped-device
   #:mapped-device
   #:mapped-device-p
   #:mapped-device-base
   #:mapped-device-scale
   #:mapped-device-mutator-fn
   #:mapped-device-get-fn
   #:mapped-device-set-fn
   #:mapped-ref
   #:mapped-device-register-address
   #:mapped-reginstance-address
   ;; memory-region
   #:memory-region
   #:memory-region-extent
   #:slave-memory-region))

