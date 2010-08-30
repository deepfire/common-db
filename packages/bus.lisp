;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:bus ;; needed by interface host-pci
  (:use :common-lisp :alexandria :iterate :bitmop :device-model :pergamum
        :options :spaces)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:bus
   #:root-bus
   #:enumerating-bus
   #:probe-discovery-bus
   #:bus-name
   #:bus-device
   #:device-bus
   #:device-bus-id
   #:device-bus-address
   #:child-bus
   #:bus-error
   #:simple-bus-error
   #:bus-devices
   #:map-bus-devices
   #:do-bus-devices
   #:bus-probe-address
   #:bus-id-at-address
   #:bus-id-equalp
   #:bus-occupied-addresses
   #:bus-notice-survivor
   #:bus-add
   #:bus-populate-address
   #:bus-remove
   #:bus-scan))

