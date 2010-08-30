;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :common-db.system
  (:use :cl :asdf))

(in-package :common-db.system)

(defsystem :common-db
  :depends-on (alexandria iterate pergamum semi-precious custom-harness bitmop symtable executor assem bintype cl-io-elf cffi opfr
               #-disable-networking usocket
	       #-disable-ironclad ironclad
               #+linux lh-usb
               #+sbcl sb-x86-portio)
  :components
  ((:module "packages"
            :components
            ((:file "options")
             (:file "portability")
             (:file "spaces")
             ;;
             (:file "address-map" :depends-on ("options"))
             (:file "bank" :depends-on ("options"))
             (:file "generic" :depends-on ("options"))
             (:file "mdb-emulation" :depends-on ("options"))
             (:file "bus" :depends-on ("options" "spaces"))
             (:file "host-pci" :depends-on ("options" "spaces"))
             (:file "graft" :depends-on ("options" "portability" "spaces"))
             ;;
             (:file "loadable" :depends-on ("options" "bank"))
             (:file "interface" :depends-on ("options" "portability" "spaces" "bus"))
             (:file "platform" :depends-on ("options" "portability" "spaces" "generic"))
             ;;
             (:file "tgt" :depends-on ("options" "portability" "spaces" "generic" "platform" "interface"))
             ;;
             (:file "platform-definitions" :depends-on ("spaces" "platform" "tgt" "sysdev" "mips"))
             (:file "sysdev" :depends-on ("options" "portability" "spaces" "generic" "platform" "interface" "tgt"))
             (:file "core" :depends-on ("options" "portability" "spaces" "address-map" "generic" "platform" "tgt"))
             (:file "mips" :depends-on ("options" "spaces" "address-map" "generic" "interface" "tgt" "core"))
             (:file "pci" :depends-on ("options" "portability" "spaces" "address-map" "bus" "generic" "platform" "loadable" "interface" "tgt"
                                                 "core" "mips"))
             (:file "dsp" :depends-on ("options" "spaces" "generic" "address-map" "interface" "tgt" "core"))
             (:file "flash" :depends-on ("options" "portability" "spaces" "address-map" "bus" "loadable" "generic" "interface" "platform" "tgt"
                                                   "core" "mips" "sysdev"))
             (:file "common-db" :depends-on ("options" "portability" "spaces" "address-map" "bus" "loadable" "generic" "interface" "platform" "tgt"
                                                       "core" "sysdev" "mips" "dsp" "flash"))
             (:file "test-hardware" :depends-on ("options" "spaces" "generic" "interface" "platform" "tgt" "sysdev" "core" "mips" "dsp" "common-db"))
             (:file "common-db-user" :depends-on ("options" "spaces" "generic" "interface" "platform" "tgt" "core" "mips" "dsp" "common-db"))))
   (:file "portability" :depends-on ("packages"))
   ;;
   (:file "address-map" :depends-on ("packages"))
   (:file "generic" :depends-on ("packages"))
   (:file "options" :depends-on ("packages"))
   ;;
   (:file "loadable" :depends-on ("options"))
   (:file "bus" :depends-on ("options"))
   ;;
   (:file "bank" :depends-on ("loadable"))
   #+(and linux sbcl nil)
   (:file "host-pci" :depends-on ("bus"))
   (:file "interface" :depends-on ("packages" "options" "bus"))
   (:file "platform" :depends-on ("generic" "options" "portability" "loadable"))
   ;;
   (:file "target" :depends-on ("portability" "generic" "interface" "platform"))
   (:module "interfaces"
            :depends-on ("packages" 
                         "bus" "interface" "target")
            :components
            ((:file "packages")
             (:file "elvees")
             ;;
	     #-disable-virtcore
             (:file "virtif" :depends-on ("packages" "elvees"))
             #-disable-networking
             (:file "tapclient" :depends-on ("packages" "elvees"))
             #-disable-networking
             (:file "rtlclient" :depends-on ("packages" "elvees"))
	     #-disable-parport
             (:file "parport" :depends-on ("packages" "elvees"))
             #+linux
             (:file "ezusb-elvees-linux" :depends-on ("packages" "elvees"))
             #+(or win32 mingw32 windows-target)
             (:file "ezusb-elvees-win32" :depends-on ("packages" "elvees"))
             (:file "ezusb-elvees" :depends-on (#+linux
                                                "ezusb-elvees-linux"
                                                #+(or win32 mingw32 windows-target)
                                                "ezusb-elvees-win32"))
             ))
   ;;
   (:file "core" :depends-on ("target"))
   (:file "system" :depends-on ("target"))
   ;;
   (:file "context" :depends-on ("portability" "bus" "core"))
   (:file "dsp" :depends-on ("address-map" "core"))
   (:file "state" :depends-on ("core"))
   (:file "system-devices" :depends-on ("system"))
   ;;
   (:module "arch"
            :depends-on ("address-map" "bank" "state")
            :components
            ((:module "mips"
                      :components
                      ((:file "mips")))))
   ;;
   (:file "flash" :depends-on ("system" "arch"))
   ;;
   ;; Tie it all together
   ;;
   (:file "main" :depends-on ("arch" "flash" "dsp" "system" "system-devices" "interfaces" "context"))
   ;;
   ;; UI
   ;;
   (:file "tui" :depends-on ("main"))
   (:module "commands"
            :depends-on ("bank" "loadable" "main")
            :components
            (;;
             (:file "address")
             (:file "analysis")
             (:file "documentation")
             (:file "dsp")
             (:file "io")
             (:file "query")
             (:file "misc")
             (:file "system")
             (:file "test")
             ;;
             (:file "trap" :depends-on ("address"))
             ;;
             (:file "state" :depends-on ("trap"))
             ;;
             (:file "trace" :depends-on ("state"))
             ;;
             (:file "final" :depends-on ("address" "trap" "documentation" "dsp" "io" "query" "state" "trace" "test"))
             ))
   ;;
   #-disable-tests
   (:module "tests"
            :depends-on ("commands")
            :components
            ((:file "base")
             ;;
             (:file "performance" :depends-on ("base"))
             (:file "hardware" :depends-on ("base"))
             ))
   ;;
   (:module "targets"
            :depends-on ("interfaces" "main")
            :components
            (#-disable-virtcore
	     (:module "virtual"
                      :components
                      ((:file "packages")
                       ;;
                       (:file "space" :depends-on ("packages"))
                       (:file "target" :depends-on ("packages"))
                       ;;
                       (:file "platform" :depends-on ("space"))
                       ;;
                       (:file "virtcore" :depends-on ("space" "target"))
                       (:file "memory" :depends-on ("space" "target"))
                       ;;
                       (:file "sim" :depends-on ("target" "platform" "virtcore"))
                       (:file "definitions" :depends-on ("target" "platform" "virtcore" "memory"))
                       ;;
                       (:file "ancillary" :depends-on ("sim" "definitions"))))))))
