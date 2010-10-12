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


(defvar *examine-test* nil)

(defun testmem (&optional (address 0) (length #x10000) (delay 0) &aux
                (target *target*)
                (*log-platform-processing* t))
  #+help-ru
  "Протестировать диапазон ячеек памяти длиной в LENGTH байт,
начиная с ADDRESS."
  #-help-ru
  "Test a chunk of memory, LENGTH bytes long, at address ADDR."
  (test-target-memory target (fixmap-address target address) length :delay delay)
  (values))

(defun run-tests (&optional all-contexts &rest test-suites)
  #+help-ru
  "Выполнить серию тестов из тестовых наборов, с названиями в остаточном списке
параметров, на всех доступных устройствах.  По умолчанию используются тестовые наборы
:INTERFACE, :TARGET и :MIPS."
  #-help-ru
  "Run a set of TEST-SUITES. Defaults to test suites :INTERFACE, :TARGET and :MIPS."
  (lret ((*log-state-changes* nil)
         (test-suites (or test-suites '(:interface :target :mips)))
         (current-ctx *current*)
         expected-failures other-failures
         (success t))
    (do-all-target-contexts (ctx)
      (when (or all-contexts (eq current-ctx ctx))
        (format t "~&~@<; ~@;Running tests on ~A ~A~:@>~%" (type-of (target-platform (ctx-target ctx))) (ctx-core ctx))
        (with-state ((ctx-core ctx) :entry-state :debug :exit-state :debug)
          (with-measured-time-lapse (seconds)
              (with-condition-printing (t custom-harness:test-error)
                (reset :stop-cores-p t)
                (dolist (suite test-suites)
                  (multiple-value-bind (suite-success-p suite-expected-failures suite-other-failures)
                      (run-test-suite (ecase suite
                                        (:interface *interface*)
                                        (:target *target*)
                                        ((:mips :perf) *core*))
                                      suite)
                    (appendf expected-failures suite-expected-failures)
                    (appendf other-failures suite-other-failures)
                    (andf success suite-success-p))))
            (let ((expected-failure-count (length expected-failures))
                  (unexpected-failure-count (count-if (of-type 'unexpected-test-failure) other-failures))
                  (unexpected-success-count (count-if (of-type 'unexpected-test-success) other-failures)))
              (format t ";;;~%;;; Test summary for suites~{ ~A~}:~%;;;~%" test-suites)
              (format t ";~10T~D expected failures~%" expected-failure-count)
              (format t ";~10T~D unexpected failures~%" unexpected-failure-count)
              (format t ";~10T~D unexpected successes~%" unexpected-success-count)
              (syncformat t ";;;~%;;; running tests took ~D second~:P~%;;;~%" seconds))))))))

(defun examine-test (name &optional (argument *core*))
  #+help-ru
  "Проанализировать тест NAME с пристрастием."
  #-help-ru
  "Analyse test with NAME with maximum thoroughness, which means enabling
all available kinds of logging."
  (let* ((*log-core-pipeline-crit* t)
         (*examine-test* t)
         (sym (find-symbol (string name) :test-hardware)))
    (unless (and sym (fboundp sym))
      (error "~@<Test ~A is not defined.~:@>" name))
    (funcall sym argument)))