;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:portability
  (:use :common-lisp :alexandria :iterate :pergamum)
  (:export
   #:set-and-activate-repl-fun
   #:argv
   #:argv0-executable-name
   #:getenv
   #:function-arglist
   #:*globally-quitting*
   #:quit
   #:with-quit-restart
   #:sigint
   #:handle-sigint
   #:with-sigint-trap
   #:class-finalized-p
   #:finalize-inheritance
   #:class-prototype
   #:class-direct-subclasses
   #:function-lambda-list
   #:with-pinned-objects
   #:without-gcing-and-interrupts
   #:nanosleep
   #:busywait
   #:busywait-interruptible-executing
   #:digest-as-string))

