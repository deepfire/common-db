;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
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
        (error 'compile-error :component c :operation operation)))))

(asdf:defsystem #:common-db-tapserver
  :depends-on (#:common-db #:usocket)
  :components
  ((:module "apps" :depends-on ()
            :components
            ((:encoded-cl-source-file #-win32 "help-unicode"
                                      #+win32 "help-cp1251"
                                      :external-format #-win32 :utf-8 #+win32 :cp1251)
             (:file "tapserver"
                    :depends-on (#-win32
                                 "help-unicode"
                                 #+win32
                                 "help-cp1251"))))))
