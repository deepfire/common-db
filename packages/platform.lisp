;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:platform ;; needed by :test-hardware
  (:use :common-lisp :alexandria :pergamum :iterate :bintype :setc :bitmop :device-model
        :portability :options :spaces :generic)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:platform-target
   #:platform-memory-configurations
   #:platform-memory-configuration-order
   #:platform-memory-configuration
   #:platform-predefined-devices
   #:platform-memory-map
   #:define-platform
   #:all-platform-classes
   #:do-platforms
   #:platform-condition
   #:platform-error
   #:simple-platform-error
   #:platform-essentials-missing
   #:platform-invalid-core-frequence-multiplier
   #:platform-device
   #:platform-memory-region
   #:mapped-platform-memory-region
   ;;
   #:detect-platform-memory-size
   ;;
   #:device-platform
   #:configure-target
   #:configure-platform-system
   #:configure-platform-memory
   #:platform-address-region))

