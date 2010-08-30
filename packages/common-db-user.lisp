;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:common-db-user
  (:nicknames :comdb-user)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:shadowing-import-from :common-db #:get #:set #:reset #:trace #:catch #:step)
  (:use :common-lisp :alexandria :pergamum :iterate :setc :bitmop :device-model :isa :isa-mips :assem :assem-mips
        :options :spaces :generic :platform :interface :tgt :core :mips :dsp
        :common-db))

