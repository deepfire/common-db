;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:dsp ;; needed by common-db, test-hardware
  (:use :common-lisp :alexandria :pergamum :iterate :setc :bitmop :device-model :isa :isa-mips :assem :assem-mips
        :options :spaces :generic :address-map :interface :tgt :core)
  (:shadowing-import-from :options #:arg)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   ;; classes & accessors
   #:dsp
   #:dsp-software-breakpoint
   #:dsp-hardware-breakpoint))

