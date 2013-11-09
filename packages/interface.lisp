;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:interface ;; needed by test-hardware
  (:nicknames :iface)
  (:use :common-lisp :alexandria :iterate :pergamum :bintype :setc :bitmop :device-model
        :options :portability :spaces :bus)
  (:shadowing-import-from :options #:arg)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:interface-bus
   #:*initializing-interface*
   #-disable-virtcore  #:virtif-bus
   #-disable-networking #:tapclient-bus
   #-disable-networking #:client-bus
   #-disable-parport   #:parport-bus
   #:ezusb-bus
   #:interface
   #:iface-name
   #:iface-idcode
   #:iface-targets
   #:iface-version
   #:iface-fastio
   #:interface-error
   #:with-interface-error-trap-and-return
   #:simple-interface-error
   #:interface-status-timeout
   #:interface-memory-timeout
   #:persistent-interface-error
   #:interface-state-transition-timeout
   #:interface-debug-quiescence-timeout
   #:make-interface-for-device-name
   #:interface-reset
   #:interface-target-discrimination-tree
   #:interface-attach-target
   #:interface-stop-target
   #:stop-target-using-interface
   #:interface-reset-target
   #:reset-target-using-interface
   #:interface-close
   #:interface-bus-word
   #:interface-bus-io
   #:scan-interface-busses
   #:interfaces
   ;;
   #:elvees-interface
   #:+oncd-ir-length+
   #:+oncd-ird-length+
   #:+idcode-length+
   #:with-writing-oncd-register-value-as-vector
   #:tap-write-dr-register))

