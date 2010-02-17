;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(asdf:defsystem #:common-db-gdbserver
    :depends-on (#:common-db #:gdb-remote #:cl-who)
    :components
    ((:file "gdb")
     (:module "arch" :depends-on ("gdb")
                     :components ((:module "mips" :components ((:file "gdb")))))
     (:module "apps" :depends-on ("arch")
              :components
              ((:file #-win32 "help-unicode"
                      #+win32 "help-cp1251")
               (:file "gdbserver"
                      :depends-on (#-win32
                                   "help-unicode"
                                   #+win32
                                   "help-cp1251"))))))
