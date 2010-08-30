;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:mdb-emulation
  (:use :common-lisp :alexandria :iterate
        :options)
  (:shadow #:step #:catch #:trace))

