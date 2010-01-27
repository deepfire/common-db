;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2008-2009, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2008-2009 by
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


(defun list-platforms ()
  #+help-ru
  "Вывести список всех известных платформ."
  (write-line "Known platforms:")
  (dolist (platclass (all-platform-classes))
    (write-string "  ")
    (princ (class-name platclass))
    (terpri)))

(defun list-memconfigs (&aux
                        (target *target*))
  #+help-ru
  "Вывести список имён конфигураций памяти потенциально применимых
к текущему целевому устройству."
  #-help-ru
  "List names of memory configurations valid for current target."
  (mapcar #'memory-config-name
          (xform target (curry #'remove-if-not (curry #'memory-config-valid-for-platform-p (target-platform target)))
                 (hash-table-values *memory-configurations*))))

(defun compile-memconfig (name)
  #+help-ru
  "Вывести список пар адрес-значение соответствующих названной конфигурации памяти."
  (iter (for (regname regbitnames regbitvalues) in (memory-config-register-values (memory-config name)))
        (multiple-value-bind (address value) (compute-raw-register-value regname regbitnames regbitvalues)
          (collect (cons address value)))))

(defun explain-memconfig (&optional name)
  #+help-ru
  "Детально, по битовым полям, разобрать структуру конфигурации памяти
с именем NAME.  Если имя не указано, разобрать текущую настройку."
  (let ((config (if name
                    (memory-config name)
                    (platform-memory-configuration (target-platform *target*)))))
    (format *log-stream* "Memory config ~A:~%" (memory-config-name config))
    (iter (for (regname fieldnames fieldvalues) in (memory-config-register-values config))
          (let* ((space (space :platform))
                 (fields (mapcar (curry #'bitfield space) fieldnames))
                 (bytes (mapcar (curry #'bitfield-byte space) fieldnames))
                 (bytemasks (mapcar #'byte-bitmask bytes)))
            (format *log-stream* "  ~A:~18T ~8,'0X ~:{ ~A(~X)~}~%~28T <= ~:{ ~A(~X)~}~%"
                    regname (fbits fieldnames fieldvalues)
                    (mapcar #'list fieldnames bytemasks)
                    (iter (for field in fields)
                          (for byte in bytes)
                          (for value in fieldvalues)
                          (collect (list value (interpret-field-value field byte value)))))))))

(defun set-memconfig (name &key test (test-size #x10000) remember)
  #+help-ru
  "Применить конфигурацию памяти с именем NAME.

Принимаются следующие ключевые аргументы:
  - TEST ::
    По применении конфигурации проверить её работоспособность,
    произведя тест памяти.
  - TEST-SIZE ::
    Задать размер тестирования.
  - REMEMBER ::
    Запомнить данную конфигурацию для активного целевого устройства,
    что приведёт к её использованию во время перенастройки при сбросе."
  #-help-ru
  "Set a memory configuration by NAME.
When TRY is specified, and non-NIL a TESTMEM is performed afterwards.
When REMEMBER is specified, and non-NIL, the configuration is
remembered for the current target."
  (let ((platform (target-platform *target*))
        (config (memory-config name)))
    (apply-memory-config platform config)
    (when test
      (testmem 0 test-size))
    (when remember
      (setf (platform-memory-configuration platform) config)))
  (values))
