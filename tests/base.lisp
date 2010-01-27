;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TEST-HARDWARE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :test-hardware)


(set-namespace :interface :core)


(defvar *examine-test* nil)

(define-condition unexpected-formatted-value (unexpected-value)
  ((format :accessor condition-format :initarg :format)
   (structured-p :accessor condition-structured-p :initarg :structured-p))
  (:report (lambda (cond stream)
             (let ((format (condition-format cond))
                   (expected (condition-expected cond))
                   (actual (condition-actual cond))
                   (*print-base* 16))
               (format stream "~@<~:[NON~;~]CRITICAL: unexpected value during test ~A:~3I ~<expected: 0x~X, actual 0x~X~:@>, or in decoded form, ~<expected: ~S, actual: ~S~:@>~:@>"
                       (condition-subtest-critical-p cond) (condition-subtest-id cond) (list expected actual) (list (decode format expected) (decode format actual)))))))

(define-condition breakpoint-not-reached (unexpected-value)
  ((breakpoint :accessor condition-breakpoint :initarg :breakpoint))
  (:report (lambda (cond stream)
             (format stream "~@<~:[NON~;~]CRITICAL: during test ~A: breakpoint at address 0x~8,'0X was not reached, PC was 0x~8,'0X, instead~:@>"
                     (condition-subtest-critical-p cond) (condition-subtest-id cond) (condition-expected cond) (condition-actual cond)))))

(define-condition invisibility-failure (unexpected-test-failure)
  ((kind :accessor condition-kind :initarg :kind))
  (:report (lambda (cond stream)
             (format stream "~@<~:[NON~;~]CRITICAL: test ~A uncovered a failure to preserve the ~A kind of invisibility~:@>"
                     (condition-subtest-critical-p cond) (condition-subtest-id cond) (condition-kind cond)))))

(defun expect-formatted-value (format expected actual &key (test #'eql))
  (if (funcall test expected actual)
      t
      (signal-test-error 'unexpected-formatted-value :format format :expected expected :actual actual)))

(defmacro expect-core-fetch-address (core (actual value-form) (condition &rest condition-parameters) &body failure-body)
  (unless (subtypep condition 'test-error)
    (error "~@<Condition must be a subtype of CUSTOM-HARNESS:TEST-ERROR~:@>"))
  (with-gensyms (once-core expected)
    `(let* ((,once-core ,core)
            (,expected ,value-form)
            (,actual (moment-fetch (saved-core-moment ,once-core))))
       (cond ((= ,actual ,expected))
             (t
              ,@failure-body
              (signal-test-error ',condition ,@condition-parameters :actual ,actual :expected ,expected))))))

(defmacro with-interface-cushion (&body body)
  (with-gensyms (primary interface-error)
    `(multiple-value-bind (,primary ,interface-error) (with-interface-error-trap-and-return nil ,@body)
       (if (typep ,interface-error 'interface-error)
           ,interface-error
           ,primary))))

(defmacro defcomdbtest (suite name prerequisites lambda-list &body body)
  `(deftest ,suite ,name ,prerequisites ,lambda-list
     (with-interface-cushion
       ,@body)))

(defmacro defcomdbtest-expected-failure (suite name prerequisites lambda-list &body body)
  `(deftest-expected-failure ,suite ,name ,prerequisites ,lambda-list
     (with-interface-cushion
       ,@body)))

(defmacro defcomdbtest-unstable-failure (suite name prerequisites lambda-list &body body)
  `(deftest-unstable-failure ,suite ,name ,prerequisites ,lambda-list
     (with-interface-cushion
       ,@body)))
