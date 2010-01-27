;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(asdf:defsystem #:common-db-opfr-toplevel
    :depends-on (#:common-db)
    :components
    ((:module "apps"
              :components
              ((:file #-win32 "help-unicode"
                      #+win32 "help-cp1251")
               (:file "opfr-toplevel"
                      :depends-on (#-win32
                                   "help-unicode"
                                   #+win32
                                   "help-cp1251"))))))
