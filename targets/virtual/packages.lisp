;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(cl:defpackage #:virtcore           ; needed by :platform-definitions
  (:use :common-lisp :alexandria :iterate
        :pergamum
        :discrimination :environment
        :isa :isa-mips :assem :assem-mips
        :setc :bitmop :device-model
        :bintype
        :portability :options :spaces :bus :generic :interface :platform :tgt :core :sysdev :mips
        :interface-virtual)
  (:shadowing-import-from :isa #:disassemble)
  (:shadowing-import-from :bitmop #:space)
  (:export
   ;;
   #:virtual-target
   #:virtual-platform
   #:virtual-mapped-device
   #:generic-virtual-platform
   #:basic-virtual-platform
   #:virtual-platform-d02
   ;;
   #:virtport
   #:virtport-dma-channel
   #:virtmport
   #:virtmport-02
   #:virtmemory-dma-engine
   #:virtimer
   #:virtsys
   ;;
   #:virtcore
   #:virtcore-trail
   ))
