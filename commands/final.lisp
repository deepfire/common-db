;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2009, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2007-2009 by
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

(in-package :common-db)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mar 'print-documentation-entry 'print-api-documentation 'print-help 'display-invocation-help-and-quit
       'coerce-to-address 'address-resolution-condition-address
       'ctxs-of-type 'ctx-id 'ctx-by-id 'ctx-initargs 'ctx-interface 'ctx-target 'ctx-core 'remove-context 'set-context
       'do-all-target-contexts
       'show-value-using-value 'display-code-address-register-value
       'coerce-to-dsp 'coerce-to-register
       'describe-insn-jump-target
       'set-display-list 'default-display-list
       'make-annotating-disassembly-printer
       'symbol-external-p 'display-invocation-help
       'init
       'comdb-tui 'read-option 'read-args-safely 'write-package-readline-completions
       'comdb-toplevel-wrapper
       'comdb-debugger)

  (export-unmarred #'fboundp (find-package :common-db)))
