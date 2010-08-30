;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:host-pci ;; needed by platform-pci
  (:use :common-lisp :cffi
        :options :spaces)
  (:export
   #:pci-system-init
   #:pci-system-cleanup
   #:pci-device-map-region
   #:pci-device-unmap-region
   #:pci-device-probe
   #:pci-device-next
   #:pci-device-find-by-slot
   #:pci-device-config-32bit
   #:+pci-match-any+
   #:pci-id-match
   #:pci-slot-match
   #:pci-id-match-iterator-create
   #:pci-slot-match-iterator-create
   #:pci-iterator-destroy
   #:pci-mem-region))

