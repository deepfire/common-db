;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:graft ;; standalone
  (:use :common-lisp :alexandria :pergamum :setc :bitmop :device-model
        :options :portability :spaces)
  (:shadowing-import-from :options #:arg)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:with-graft-io-capture
   #:set-port-forward
   #:set-port-reverse
   #:port-reset
   #:port-status
   #:port
   #:port-bit
   #:port-bits
   #:get-permission
   #:clear-epp-timeout
   #-disable-parport #:parport-error
   #-disable-parport #:parport-insufficient-privileges-error
   #-disable-parport #:parport-reset-error))

