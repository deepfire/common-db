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
                 (mapcar #'cdr (platform-memory-configurations (target-platform target))))))

(defun memconfig (&optional name)
  #+help-ru
  "Найти конфигурацию памяти с заданным именем, среди множества доступного для
платформы текущего целевого устройства."
  (if name
      (if (sysdev::memory-config-p name)
          name
          (memory-config (target-platform *target*) name))
      (platform-memory-configuration (target-platform *target*))))

(defun compile-memconfig (&optional name)
  #+help-ru
  "Вывести список пар адрес-значение соответствующих названной конфигурации памяти.
Если имя не указано, использовать активную конфигурацию памяти."
  (iter (for (regname regbitnames regbitvalues) in (memory-config-register-values (memconfig name)))
        (multiple-value-bind (address value) (target-compile-raw-register-value *target* regname regbitnames regbitvalues)
          (collect (list address value)))))

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
  (let* ((platform (target-platform *target*))
         (config (memory-config platform name)))
    (apply-memory-config platform config)
    (when test
      (testmem 0 test-size))
    (when remember
      (setf (platform-memory-configuration platform) config)))
  (values))

(defun decompile-memconfig (address/value-pair-list)
  #+help-ru
  "Декодировать набор пар адрес-или-имя-регистра/значение как конфигурацию памяти
для текущей платформы."
  (let ((target *target*))
    (apply #'make-memory-config :decompiled
           (iter (for (reg/addr regval) in address/value-pair-list)
                 (collect (remove nil
                                  (handler-case (multiple-value-call
                                                    #'list (target-decompile-raw-register-value target
                                                            (etypecase reg/addr
                                                              (keyword (target-reg-addr target reg/addr))
                                                              (integer reg/addr))
                                                            regval))
                                    (error ()
                                      (format t "The ~X => ~X value/register pair is not valid for the current platform ~S~%"
                                              regval reg/addr (target-platform target))
                                      nil))))))))

(defun explain-memconfig (&optional name)
  #+help-ru
  "Детально, по битовым полям, разобрать структуру конфигурации памяти
с именем NAME.  Если имя не указано, разобрать текущую настройку."
  (let* ((config (memconfig name))
         (target *target*))
    (format *log-stream* "Memory config ~A:~%" (memory-config-name config))
    (iter (for (regname fieldnames fieldvalues) in (memory-config-register-values config))
          (let* ((space (space :platform))
                 (fields (mapcar (curry #'bitfield space) fieldnames))
                 (bytes (mapcar (curry #'bitfield-byte space) fieldnames))
                 (bytemasks (mapcar #'byte-bitmask bytes)))
            (format *log-stream* "  ~8,'0X ~A:~18T ~8,'0X ~:{ ~A(~X)~}~%~28T <= ~:{ ~A(~X)~}~%"
                    (target-reg-addr target regname) regname (fbits fieldnames fieldvalues)
                    (mapcar #'list fieldnames bytemasks)
                    (iter (for field in fields)
                          (for byte in bytes)
                          (for value in fieldvalues)
                          (collect (list value (interpret-field-value field byte value)))))))))

(defun print-memconfig (&optional name)
  #+help-ru
  "Напечатать конфигурацию памяти в тех двух вариантах, в котором она может
храниться в файлах."
  (let* ((config (memconfig name))
         (cooked (memory-config-register-values config))
         (raw (compile-memconfig config))
         (size (size (memory-region-extent (target-device *target* '(ram 0))))))
    (syncformat t "~%~
                   Current memory configuration~%~
                   ~%~
                   Structured:~%~
                   -----------------------------------  8<  -----------------------------------~%~
                   (~S~%~
                   ~{  ~S~%~}  ~S)~%~
                   ~:[~;(:SIZE ~:*~X)~%~]~
                   -----------------------------------  >8  -----------------------------------~%~
                   ~%~
                   Raw:~%~
                   -----------------------------------  8<  -----------------------------------~%~
                   (~S~%~
                   ~{  ~S~%~}  ~S)~%~
                   ~:[~;(:SIZE ~:*~X)~%~]~
                   -----------------------------------  >8  -----------------------------------~%"
                (memory-config-name config)
                (butlast cooked) (lastcar cooked) size
                (memory-config-name config)
                (butlast raw) (lastcar raw) size)))