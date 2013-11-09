;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:sysdev ;; needed by common-db
  (:use :common-lisp :alexandria :pergamum :iterate :discrimination :isa :isa-mips :assem :assem-mips :setc :bitmop :device-model
        :options :portability :spaces :generic :platform :interface :tgt)
  (:shadow #:step)
  (:shadowing-import-from :options #:arg)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   #:*memory-configurations*
   #:*memory-configuration-order*
   #:memory-config
   #:make-memory-config
   #:memory-config-name
   #:memory-config-register-values
   #:do-memory-configs
   #:parse-memory-config
   #:serialise-memory-config
   #:read-memory-config-file-for-platform
   #:memory-config-valid-for-device-classes-p
   #:memory-config-valid-for-platform-p
   #:apply-memory-config
   #:test-target-memory
   #:platform-no-usable-memory-detected-error
   #:platform-no-usable-memory-detected))

