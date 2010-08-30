;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:spaces                    ; standalone
  (:use :common-lisp :bitmop :device-model)
  (:shadowing-import-from :bitmop #:space)
  (:export
   ;; access
   #:interface
   #:platform
   #:target
   #:32bit-bus-target
   #:64bit-bus-target
   #:little-endian-target
   #:big-endian-target
   #:system
   ;; cores
   #:general-purpose-core
   #:little-endian-core
   #:big-endian-core
   #:mmu-core
   #:mips-core
   #:mipsel
   #:mipseb
   #:mips-mmu-core
   #:torn-pipeline-mips-core
   ;; memory
   #:cache
   #:memory
   #:internal-memory
   #:external-memory
   #:ram
   #:dcache
   #:icache
   #:scache
   ;; devices
   #:flash
   #:standard-flash))

