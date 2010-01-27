;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :common-db.system
  (:use :cl :asdf))

(in-package :common-db.system)

#+(and sbcl win32)
(progn
  (defclass encoded-file ()
    ((external-format :reader file-external-format :initarg :external-format)))
  (defclass encoded-cl-source-file (encoded-file cl-source-file) ())

  (defmethod perform ((operation compile-op) (c encoded-cl-source-file))
    (let ((source-file (component-pathname c))
          (output-file (car (output-files operation c))))
      (multiple-value-bind (output warnings-p failure-p)
          (compile-file source-file :output-file output-file :external-format (file-external-format c))
        (when warnings-p
          (case (operation-on-warnings operation)
            (:warn (warn
                    "~@<COMPILE-FILE warned while performing ~A on ~A.~@:>"
                    operation c))
            (:error (error 'compile-warned :component c :operation operation))
            (:ignore nil)))
        (when failure-p
          (case (operation-on-failure operation)
            (:warn (warn
                    "~@<COMPILE-FILE failed while performing ~A on ~A.~@:>"
                    operation c))
            (:error (error 'compile-failed :component c :operation operation))
            (:ignore nil)))
        (unless output
          (error 'compile-error :component c :operation operation))))))

(defsystem :common-db
  :depends-on (alexandria iterate pergamum semi-precious custom-harness bitmop symtable executor assem bintype cl-io-elf cffi opfr ironclad
               #+linux lh-usb
               #+sbcl sb-x86-portio)
  :components
  ((:file "portability")
   ;;
   (:file "packages" :depends-on ("portability"))
   (:file "twilight")
   ;;
   (:file "address-map" :depends-on ("packages"))
   (:file "generic" :depends-on ("packages"))
   (:file "options" :depends-on ("packages"))
   ;;
   (:file "loadable" :depends-on ("options"))
   (:file "bus" :depends-on ("options"))
   ;;
   (:file "eltext" :depends-on ("loadable"))
   #+(and linux sbcl nil)
   (:file "host-pci" :depends-on ("bus"))
   (:file "interface" :depends-on ("options" "portability" "bus"))
   (:file "platform" :depends-on ("generic" "options" "portability" "loadable"))
   ;;
   (:file "target" :depends-on ("portability" "generic" "interface" "platform"))
   (:module "interfaces"
            :depends-on ("portability"
                         "bus" "interface" "target")
            :components
            ((:file "packages")
             (:file "elvees")
             ;;
             (:file "parport" :depends-on ("packages" "elvees"))
             #+linux
             (:file "ezusb-elvees-linux" :depends-on ("packages" "elvees"))
             #+(or win32 mingw32)
             (:file "ezusb-elvees-win32" :depends-on ("packages" "elvees"))
             (:file "ezusb-elvees" :depends-on (#+linux
                                                "ezusb-elvees-linux"
                                                #+(or win32 mingw32)
                                                "ezusb-elvees-win32"))
             ))
   ;;
   (:file "core" :depends-on ("target"))
   (:file "system" :depends-on ("target"))
   ;;
   (:file "mips" :depends-on ("address-map" "core"))
   (:file "dsp" :depends-on ("address-map" "core"))
   (:file "system-devices" :depends-on ("system"))
   ;;
   (:file "mips-state" :depends-on ("interfaces" "eltext" "mips"))
   (:file "flash" :depends-on ("system" "mips"))
   (:file "context" :depends-on ("portability"
                                 "bus" "interface" "platform" "target" "core"))
   ;;
   ;; Tie it all together
   ;;
   (:file "main" :depends-on ("mips-state" "flash" "mips" "dsp" "system" "system-devices" "context"))
   ;;
   ;; UI
   ;;
   (:file "tui" :depends-on ("main"))
   (:module "commands"
            :depends-on ("eltext" "loadable" "main")
            :components
            (;;
             (:file "address")
             (:file "documentation")
             (:file "dsp")
             (:file "io")
             (:file "query")
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
   (:module "tests"
            :depends-on ("commands")
            :components
            ((:file "base")
             ;;
             (:file "performance" :depends-on ("base"))
             (:file "test-hardware" :depends-on ("base"))
             ))
   ;;
   (:module "targets"
            :depends-on ("tests"
                         #+(and linux sbcl nil)
                         "host-pci")
            :components
            ())))
