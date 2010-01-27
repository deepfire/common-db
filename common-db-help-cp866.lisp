;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10 -*-
;;;
;;;  (c) copyright 2009 by
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

(in-package :comdb)


(defvar *comdb-help-ru*
  "~%Формат запуска: ~A [АРГУМЕНТЫ...]

  Настройки Лисп-среды:
    --disable-debugger          Отключить отладчик Лиспа.  При ошибках
                                  печатать дамп стека и выходить.
    --break-on-signals          Останавливаться на исключительных ситуациях
                                  имеющих обработчик.
    --early-break-on-signals    Включать остановку на исключительных
                                  ситуациях раньше.

  Настройки общего характера:
    --before-hook <lisp-expr>   Выполнить Лисп-выражение перед сканированием
                                  интерфейсов.
    --core-multiplier <integer> Установить множитель частоты кристалла.
    --no-scan                   Не сканировать интерфейсы.
    --run-tests                 Выполнить предварительные тесты.
                                  При возникновении ошибок выйти.
    --ignore-test-failures      Игнорировать ошибки при исполнении тестов.
    --quit-after-tests          Выйти после исполнения тестов.
    --no-rc                     Не загружать ~~/.comdbrc
    --load <filename>           Исполнить команды из файла.
    --quit-after-load           Выйти после исполнения команд из файла.
    --disable-usb               Не использовать USB-адаптеры JTAG.
    --no-memory-detection       Не пытаться определить тип основной памяти.
    --memory-detection-threshold  При определении типа основной памяти
                                  тестировать безошибочность ввода/вывода
                                  такого объёма памяти.
    --verbose                   Печатать БОЛЬШЕ ИНФОРМАЦИИ во время работы.
    --help                      Вывести эту справку.
    --help-en                   Вывести справку, на английском.")

(defvar *flasher-help-ru*
  "  Настройки флэшера:
    --disable-debugger          Отключить отладчик Лиспа.  При ошибках
    --raw-file <raw-filename>   Загрузить файл как есть, без разбора,
                                  по адресу заданному с помощью ключа
                                  --raw-base.
    --raw-base <raw-filename-base>  Указать адрес загрузки, используется
                                    совместно с ключём --raw-file.
    --elf-file <ELF-filename>   Загрузить секции кода и данных из ELF-файла.
    --preserve-holes            Сохранять данные соседствующие с 
                                  записываемыми по блокам стирания.
    --flash-base                Указать базовый адрес флэш-памяти.
                                  По умолчанию равен #xBFC00000.
    --dump-base <address>       Печатать содержимое памяти целевого устройства.
    --dump-size <bytes>         Указать количество памяти для печати.
                                  По умолчанию равно #x100.
    --no-check                  Не проверять целостность записанных данных.
    --dry-run                   Симулировать запись: делать всё кроме неё.")