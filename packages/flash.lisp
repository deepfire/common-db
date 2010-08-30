;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:flash
  (:use :common-lisp :alexandria :iterate
        :pergamum
        :bintype
        :environment
        :isa :isa-mips :assem :assem-mips
        :setc :bitmop :device-model
        :portability :options :spaces :bus :address-map :loadable :generic :interface :platform :tgt :core
        :mips :sysdev)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   #:flash-error
   #:simple-flash-error
   #:intel-cmdset-flash
   #:amd-cmdset-flash
   #:stmaa-cmdset-flash
   ;;
   #:flash-base
   #:flash-size
   #:flash-code-seg
   #:probe-flash
   ))

