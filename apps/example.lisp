;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: EXAMPLE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2010 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(defpackage #:example
  (:use :common-lisp :cffi :pergamum :iterate :setc :bitmop :device-model :isa :assem
        :portability :options :spaces :generic :interface :common-db)
  (:shadowing-import-from :isa #:disassemble)
  (:shadowing-import-from :bitmop #:space #:*space*)
  (:shadowing-import-from :common-db #:catch #:step #:get #:set #:trace)
  (:export
   #:example-toplevel))

(in-package :example)


(defvar *example-help-en*
  "  Example options:
    --address <dotted-quad>     Address of the interface to accept connections.
                                  Defaults to 127.0.0.1.
    --port <integer>            Number of the TCP port to accept connections on.
                                  Defaults to 9000.
    --foo                       Whether to foo or not.")

(defun example-toplevel ()
  (setf common-db::*additional-help-en* *example-help-en*)
  (comdb::comdb-toplevel-wrapper #'example
                                 '((:address :string) (:port :decimal)) ;; default number base is #x10, aka :hex
                                 '(:foo)))

;; The VERBOSE key is special: you must handle it. 
(defun example (&key verbose (address "127.0.0.1") (port 9000) foo &aux
                (core (or (when *current*
                            (ctx-core *current*))
                          (error "~@<No active target context: cannot proceed with example!~:@>"))))
  (when verbose
    (write-line "; VERBOSE NOTE: doing churn..."))
  (format t "Hello!~%~
             It appears that address is ~S, port is ~D, and foo is~:[ not~;~] there!~%~
             ~%~
             The current context is: ~A~%~
             Its core is: ~A~%~
             ~%~
             Bye!~%"
          address port foo *current* core))