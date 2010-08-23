;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#-disable-virtcore
(cl:defpackage #:interface-virtual
  (:use :common-lisp :alexandria :iterate :pergamum :setc :bitmop :device-model
        :portability :options :bus :interface)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:virtif-interface
   #:*virtual-interface-stop-during-reset*
   ))

#-disable-parport
(cl:defpackage #:interface-parport
  (:use :common-lisp :alexandria :iterate :pergamum :setc :bitmop :device-model
        :portability :options :bus :interface)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:bind-to-parport-access-library
   #:parport-interface
   #:lpt1-base
   #:lpt2-base
   #:lpt3-base
   #:lpt4-base
   ))

#-disable-networking
(cl:defpackage #:interface-tapclient
  (:use :common-lisp :alexandria :iterate :usocket :pergamum :setc :bitmop :device-model
        :portability :options :bus :interface)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:tapclient-interface
   ))

#-disable-networking
(cl:defpackage #:interface-rtlclient
  (:use :common-lisp :alexandria :iterate :usocket :pergamum :setc :bitmop :device-model
        :portability :options :bus :interface)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:client-interface
   ))

(cl:defpackage #:interface-ezusb-elvees
  (:use :common-lisp :alexandria :iterate :pergamum :setc :bitmop :device-model :bintype
        :portability :options :bus :interface)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:ezusb-interface
   ))
