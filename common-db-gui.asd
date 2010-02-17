;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :common-db-gui
  :depends-on (#:mcclim #:mcclim-truetype #:clouseau #:clim-file-selector #:inclime #:alexandria #:iterate #:turing
               #:common-db)
  :components
  ((:module "gui"
            :components
            ((:file "packages")
             (:file "tooltips" :depends-on ("packages"))
             (:file "gui" :depends-on ("tooltips"))))))