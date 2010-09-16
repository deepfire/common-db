;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:trace
  (:use :common-lisp :pergamum
        :options)
  (:export
   #:setup-trace
   #:trace-fun-call
   #:trace-fun-result
   #:define-traced-fun
   #:enable-call-trace
   #:disable-call-trace
   #:enable-result-trace
   #:disable-result-trace
   ;;
   #:*trace-stream*
   #:*trace-calls*
   #:*trace-results*))

