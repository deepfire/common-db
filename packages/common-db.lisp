;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:common-db ;; needed by test-hardware
  (:nicknames :comdb)
  (:use :common-lisp :alexandria :iterate :pergamum :custom-harness :setc :bitmop :device-model :isa :isa-mips :assem :assem-mips
        :portability :options :spaces :bus :address-map :loadable :generic :interface :platform :tgt :core :sysdev
        :mips :dsp :flash)
  (:shadowing-import-from :isa #:disassemble)
  (:shadowing-import-from :bitmop #:space #:*space*)
  (:shadow #:get #:set #:step #:catch #:trace #:reset)
  (:export
   ;; re-export
   apropos describe load sleep quit
   ;;
   #:*syms*
   #:*default-initargs*
   ;; target contexts
   #:target-context
   #:*target-contexts*
   #:*current*
   #:*interface*
   #:*target*
   #:*core*
   #:*examine-test*
   #:ctx-interface
   #:ctx-target
   #:ctx-core
   #:ctx-display-list
   #:do-all-target-contexts
   #:display-list
   #:default-display-list
   ;; for apps
   #:*additional-help-en*
   #:*additional-help-ru*))
 
