;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(asdf:defsystem #:common-db-gdbserver
    :depends-on (#:common-db #:gdb-remote)
    :components
    ((:module "apps"
              :components
              ((:file #-win32 "help-unicode"
                      #+win32 "help-cp1251")
               (:file "gdbserver"
                      :depends-on (#-win32
                                   "help-unicode"
                                   #+win32
                                   "help-cp1251"))))))
