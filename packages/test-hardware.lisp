;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:test-hardware ;; standalone
  (:nicknames :testhw)
  (:use :common-lisp :alexandria :pergamum :iterate :setc :bitmop :device-model :isa :isa-mips :assem :assem-mips :custom-harness
        :options :spaces :generic :platform :interface :tgt :core :sysdev :mips :dsp
        :common-db)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:shadowing-import-from :common-db #:get #:set #:reset #:trace #:catch #:step)
  (:export
   #:*failure-inspector*
   #:*log2-iota*
   #:*runner-stepper-test-pack*
   #:*interrupt-loop-test-pack*
   #:*interrupt-test-pack*
   #:*debug-stop-debug-test-pack*
   ;;
   #:unexpected-formatted-value
   #:breakpoint-not-reached
   #:invisibility-failure
   #:expect-formatted-value
   #:expect-core-fetch-address
   #:defcomdbtest
   ;;
   #:emit-r1-complex-jumpclear
   #:emit-r1-fff-target+18-end+40-loading-sequence
   #:emit-r1-jumpclear-0x14-0x30
   #:emit-r1-jumpclear-0x14-0x30-jal
   #:emit-r1-jumpclear-0x14-0x30-jr
   #:emit-r1-jumpclear-0x14-0x30-dependent
   #:emit-setup-gpr-standalone-bits
   #:run-test-pack
   #:run-maybe-swbreak-and-dsd-test
   #:make-expect-r1
   #:production-stop-debug-stop))

