;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:platform-definitions                  ; standalone
  (:nicknames :platdefs)
  (:use :common-lisp
        :pergamum
        :spaces :platform :tgt :sysdev :mips))

