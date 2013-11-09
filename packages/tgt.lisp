;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:tgt ;; needed by platform, core, system, common-db, test-hardware
  (:use :common-lisp :alexandria :pergamum :iterate :discrimination :isa :isa-mips :assem :assem-mips :setc :bitmop :device-model
        :options :portability :spaces :generic :platform :interface)
  (:shadow #:step)
  (:shadowing-import-from :options #:arg)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   #:target-enumpool
   #:target-platform
   #:target-mapped-artifact-map
   #:target-artifact-extent
   #:find-target-class-for-interface
   #:detect-target-platform
   #:target-platform-discrimination-tree
   #:configure-target-platform
   #:target-artifact-by-address
   #:target-devices
   #:target-devices-by-type
   #:target-device
   ;; enumeration & its fruit
   #:add-target-device
   #:remove-target-device
   #:target-reginstance
   #:target-reginstance-by-id
   #:target-reg-addr
   #:target-reg
   #:set-target-reg
   #:target-compile-raw-register-value
   #:target-decompile-raw-register-value
   ;;
   #:make-target-device
   #:create-target-device-from-spec
   #:with-target-devices
   #:busmem
   ;; conditions
   #:target-condition
   #:target-error
   #:unknown-target-device
   #:target-platform-detection-error
   ;; execution
   #:exec-raw
   ;; address space
   #:fixmap-address))

