;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(cl:defpackage #:interface-virtual
  (:use :common-lisp :alexandria :iterate :pergamum :setc :bitmop :device-model
        :portability :options :bus :interface)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:virtif-interface
   #:*virtual-interface-stop-during-reset*
   ))

(cl:defpackage #:interface-parport
  (:use :common-lisp :alexandria :iterate :pergamum :setc :bitmop :device-model
        :portability :options :bus :interface)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:parport-interface
   #:lpt1-base
   #:lpt2-base
   #:lpt3-base
   #:lpt4-base
   ))

(cl:defpackage #:interface-ezusb-elvees
  (:use :common-lisp :alexandria :iterate :pergamum :setc :bitmop :device-model :bintype
        :portability :options :bus :interface)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:ezusb-interface
   ))
