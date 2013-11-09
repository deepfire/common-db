;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:mips ;; needed by common-db, test-hardware
  (:use :common-lisp :alexandria :pergamum :iterate :setc :bitmop :device-model :isa :isa-mips :assem :assem-mips
        :options :spaces :address-map :generic :interface :tgt :core)
  (:shadowing-import-from :options #:arg)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   ;; classes & accessors
   #:mips-core-bank
   #:pipeline
   #:mips-core-watchpoint-stop-skew
   #:mips-bank-gpr-shadow
   #:mips-bank-cop0-shadow
   #:torn-pipeline-mips-core-broken-by
   #:mips-software-breakpoint
   #:mips-hardware-breakpoint
   #:mips-state
   ;; address space
   #:kuseg
   ;; sane 32 bit
   #:kseg0
   #:kseg1
   #:kseg2
   #:kseg3
   #:seg32p
   #:kusegp
   #:ksegp
   #:kseg0p
   #:kseg1p
   #:kseg2p
   #:kseg3p
   #:remap-to-seg32
   #:remap-to-kuseg
   #:remap-to-kseg0
   #:remap-to-kseg1
   #:remap-to-kseg2
   #:remap-to-kseg3
   #:extent-to-seg32
   ;; sign-extended 32 bit
   #:x32kseg0
   #:x32kseg1
   #:x32kseg2
   #:x32kseg3
   #:x32seg32p
   #:x32kseg0p
   #:x32kseg1p
   #:x32kseg2p
   #:x32kseg3p
   #:remap-to-x32seg32
   #:remap-to-x32kseg0
   #:remap-to-x32kseg1
   #:remap-to-x32kseg2
   #:remap-to-x32kseg3
   #:extent-to-x32seg32
   ;; execution
   #:exec
   ;; unsorted
   #:patch-core-interface-pipeline-reginstances
   #:save-registers
   #:restore-registers
   #:mips-core-entrance-oscr
   ;; TLB
   #:mips-tlb-entry
   #:mips-tlb-entry-hi
   #:mips-tlb-entry-lo0
   #:mips-tlb-entry-lo1
   ;; insanity-lurks-nearby.lisp
   #:+insane-trampoline-address+))

