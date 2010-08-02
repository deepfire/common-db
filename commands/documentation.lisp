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


(defvar *docstring-overrides*
  '((apropos "Коротко описать все символы содержащие указанную STRING.
Если указан PACKAGE, то описать лишь символы присутствующие в этом пакете.  
Если указан EXTERNAL-ONLY, тогда описывать только символы эскпортируемые данным пакетом.")
    (describe "Вывести описание OBJECT.")
    (load "Загрузить файл указанный с помощью PATHSPEC, последовательно считывая
и исполняя содержащиеся в нём формы.")
    (quit "Выйти из отладчика, не меняя состояние целевого устройства.")
    (sleep    "Эта функция приостанавливает исполнение на SECONDS.  
SECONDS могут быть любым неотрицательным реальным числом.")))

(defvar *lambda-list-overrides*
  '((? (what))
    (help (&optional what))))

(defvar *documentation-categories*
  `(((documentation #-help-ru "Documentation"
                    #+help-ru "Документация"
                    :conc-name doc)
     ? help apropos describe)
    ((devices #-help-ru "Devices"
              #+help-ru "Устройства"
              :conc-name dev)
     scan
     compile-memconfig list-memconfigs set-memconfig explain-memconfig
     list-contexts ctx)
    ((input/output #-help-ru "Input/output"
                   #+help-ru "Ввод/вывод"
                   :conc-name io)
     clearmem loadelf
     loadbin savebin
     loadbank saveeltext
     loadldr)
    ((state #-help-ru "State control"
            #+help-ru "Управление состоянием")
     reset
     dive rise
     run run-async run-until-stopped-by run-while-boring run-while-step-in
     step stepw trace
     dspreset dsprun dspstep dspstop
     explain
     ret)
    ((traps #-help-ru "Traps"
            #+help-ru "Ловушки")
     hbreak sw-break watch catch trace settrace
     clear-break clear-sw-breaks disable-breaks describe-breaks
     dspbreak)
    ((dsp #-help-ru "DSP"
          #+help-ru "DSP")
     dsp dspbreak dspreset dsprun dspstep dspstop)
    ((addresses #-help-ru "Addresses & symbols"
                #+help-ru "Адреса и символы"
               :conc-name addr)
     addr addrsym nextaddr nextsym prevaddr symaddr symlength
     loadsyms
     register-address)
    ((analysis #-help-ru "Analysis"
               #+help-ru "Анализ"
               :conc-name query)
     disasm displash dispack
     dump print-memory print-tlb
     display edisplay display-list
     explain
     get set peek show
     pipeline pipesyms)
    ((tracing #-help-ru "Tracing"
              #+help-ru "Трассировка"
              :conc-name trc)
     trace block-trace count-trace insn-trace staircase-trace)
    ((miscellaneous #-help-ru "Miscellaneous"
                    #+help-ru "Прочее"
                    :conc-name misc)
     sleep write-completions
     list-platforms version)
    ((control #-help-ru "Rough control"
              #+help-ru "Грубое управление"
              :conc-name ctl)
     load quit)
    ((testing #-help-ru "Testing"
              #+help-ru "Тестирование"
              :conc-name test)
     run-tests testir testmem)))

(defun print-documentation-entry (name type &optional lambda-list print-category orgify (indentation 5))
  (let* ((indent (make-string indentation :initial-element #\Space))
         (category (find-if (curry #'member name) *documentation-categories* :key #'rest))
         (header-control (concatenate 'string "~@<~@;" indent 
                                      "- ~(~:[" (when orgify #+nil "<<<") "~A" (when orgify #+nil ">>>")
                                      "~;=~A=~]~:[~;~:*~{ ~A~}~]~) " (when orgify "::") "~:[~; ~A: ~S~]~:@>~%"))
         (body-control (concatenate 'string "~@<" indent "  ~@;~A~:@>~%~%")))
    (format t header-control (eq type 'variable) name (when lambda-list
                                                        (map-list-tree
                                                         (lambda (sym &aux (*print-case* :downcase))
                                                           (let ((symstr (princ-to-string sym)))
                                                             (cond ((keywordp sym)
                                                                    (concatenate 'string ":" symstr))
                                                                   ((and orgify (char= #\* (schar symstr 0)))
                                                                    (concatenate 'string "=" symstr "="))
                                                                   (t
                                                                    symstr))))
                                                         (or (second (assoc name *lambda-list-overrides*))
                                                             lambda-list)))
            (and category print-category)
            #+help-ru "Категория" #-help-ru "Category"
            (cadar category))
    (if-let ((documentation (or #+help-ru (second (assoc name *docstring-overrides*))
                                (documentation name type))))
      (format t body-control documentation)
      (terpri))))

(defun print-api-documentation ()
  #+help-ru
  "Непечатать эту документацию в формате пригодном для потребления Emacs Orgmode."
  (let ((sym (second (argv)))
        fsyms
        syms)
    (cond (sym
           (setf sym (find-symbol (string-upcase sym) :common-db)))
          (t
           (do-external-symbols (sym (find-package :common-db))
             (when (fboundp sym)
               (push sym fsyms)))
           (do-external-symbols (sym (find-package :options))
             (when (boundp sym)
               (push sym syms)))))
    (setf fsyms (sort fsyms #'string<))
    (setf syms (sort syms #'string<))
    (if sym
        (print-documentation-entry sym (if (fboundp sym) 'function 'variable)
                                   (when (fboundp sym)
                                     (function-lambda-list (fdefinition sym)))
                                   nil t)
        (let* ((*print-base* #x10)
               (*print-right-margin* 200))
          (format t "*** Управляющие переменные~%")
          (iter (for sym in syms)
                (print-documentation-entry sym 'variable nil nil t))
      
          (iter (for category in *documentation-categories*)
                (destructuring-bind ((name title &key conc-name) &rest symbols) category
                  (declare (ignore name conc-name))
                  (format t "*** ~A~%" title)
                  (iter (for fsym in symbols)
                        (print-documentation-entry fsym 'function (function-lambda-list (fdefinition fsym)) nil t)))))))
  (quit))

(defun print-help (&optional what)
  "Display help on WHAT, when it is specified, or list all available commands."
  (let* ((known-commands (iter (for sym in-package (find-package :common-db) external-only t)
                               (when (fboundp sym)
                                 (collect sym))))
         (category (and what (iter (for category in *documentation-categories*)
                                   (destructuring-bind ((name title &key conc-name) &rest symbols) category
                                     (declare (ignore title symbols))
                                     (when (or (eq what name) (eq what conc-name))
                                       (return category))))))
         (commands (when what
                     (cond (category
                            (cdr category))
                           ((member what known-commands)
                            (list what))
                           (t
                            (format t "~@<Unknown command: ~A~:@>~%~%" what)
                            (return-from print-help (values))))))
         (*print-right-margin* 200))
    (if commands
        (iter (for cmd in (sort (copy-list commands) #'string<))
              (initially
               (when category
                 (format t
                         #+help-ru "Справка по категории команд ~S:~%~%"
                         #-help-ru "Help on command category ~S:~%~%"
                         (cadar category))))
              (print-documentation-entry cmd 'function (function-lambda-list cmd) (and what (not category)) nil 0))
        (iter (for category in *documentation-categories*)
              (initially
               (format t
                       #+help-ru "Слишком много команд чтобы показать сразу, пожалуйста выберите категорию.~%~%"
                       #-help-ru "There are too many commands to show at once, please choose a category.~%~%"))
              (destructuring-bind ((name title &key conc-name) &rest symbols) category
                (format t "~@<   ~(~A~:[~; ~:*(~A)~]~)~30T~A~:@>~%" name conc-name title)
                (format t "~@<       ~@;~{ ~(~A~)~}~:@>~%~%" symbols))
              (finally
               (format t
                       #+help-ru "Выбрать категорию можно либо по её полному имени, либо по сокращённому (указанному в скобках).~%"
                       #-help-ru "You can choose a category either by its long name, or by the short one (shown in parentheses).~%"))))
    
    (unless what                        ; do some consistency checks
      (let ((categorised (remove-duplicates (iter (for ((nil nil . nil) . symbols) in *documentation-categories*)
                                                  ;; ((name title . rest) . symbols)
                                                  (appending symbols)))))
        (when-let ((stale (set-difference categorised known-commands)))
          (format t "~@<WARNING: ~@;stale categories:~{ ~A~}~:@>~%" stale))
        (when-let ((uncategorised (set-difference known-commands categorised)))
          (format t "~@<WARNING: ~@;uncategorised commands:~{ ~A~}~:@>~%" uncategorised))))
    (values)))

(defmacro help (&optional what)
  #+help-ru
  "Вывести справку по связанной с символом WHAT командой или категорией команд, 
если он задан.  Если WHAT не указан, вывести перечень категорий."
  `(print-help ,(if (and (consp what) (eq 'quote (car what)))
                    what
                    (list 'quote what))))

(defmacro ? (what)
  #+help-ru
  "Вывести справку по связанной с символом WHAT командой или категорией команд."
  `(print-help ,(if (and (consp what) (eq 'quote (car what)))
                    what
                    (list 'quote what))))
