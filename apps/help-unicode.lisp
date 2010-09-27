;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10 -*-
;;;
;;;  (c) copyright 2009-2010 by
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


(setf *comdb-help-ru*
      "~%Формат запуска: ~A [АРГУМЕНТЫ...]

  Настройки Лисп-среды:
    --disable-debugger          Отключить отладчик Лиспа.  Выходить при ошибках.
    --print-backtrace-on-errors Печатать стек вызовов при выходе вызванном
                                  ошибкой при включенной опции
                                   --disable-debugger.
    --break-on-signals          Останавливаться на исключительных ситуациях
                                  имеющих обработчик.
    --early-break-on-signals    Включать остановку на исключительных
                                  ситуациях раньше.

  Настройки общего характера:
    --early-eval <lisp-expr>    Выполнить Лисп-выражение перед сканированием
                                  интерфейсов.
    --core-multiplier <integer> Установить множитель частоты кристалла.
    --list-platforms            Перечислить известные платформы и выйти.
    --platform <platform-name>  Использовать указанную платформу, вместо
                                  автоматического определения.
    --physical                  Искать физические целевые устройства.
                                  По умолчанию включено, если не указан один из
                                  следующих ключей:
                                  --virtual, --tapserver или --rtlserver.
    --virtual                   Активировать виртуальные интерфейс, целевое
                                  устройство и ядро.  Фиксирует умолчание
                                  --physical в NIL.
    --tapserver [<dotted-quad>] Присоединиться к TAP-серверу, по опционально-
                                  указанному адресу, равному по умолчанию
                                  127.0.0.1
                                  Фиксирует умолчание --physical в NIL.
    --rtlserver [<dotted-quad>] Присоединиться к RTL-серверу, по опционально-
                                  указанному адресу, равному по умолчанию
                                  127.0.0.1
                                  Фиксирует умолчание --physical в NIL.
    --trace-exchange [<integer>]
                                Вести протокол обменов через сеть, тогда когда
                                  это применимо.  В некоторых случаях, можно
                                  задать лимит на размер протоколируемой части
                                  сетевых пакетов.  Лимит по-умолчанию равен
                                  1024 байтам.
    --tapserver-port <integer>  Номер TCP-порта TAP-сервера.
                                  Равен по-умолчанию 9001.
    --rtlserver-port <integer>  Номер TCP-порта RTL-сервера.
                                  Равен по-умолчанию 9001.
    --no-parport                Не искать целевые устройства на портах EPP.
    --no-usb                    Не искать целевые устройства на шине USB.
    --no-scan                   Не сканировать интерфейсы.
    --no-platform-init          Не осуществлять инициализации уровня платформы.
    --memory-config <filename>  Прочитать конфигурацию памяти из указанного
                                  файла, установив её первой в списке
                                  перебираемых вариантов.
    --print-memory-config       Вывести вариант конфигурации памяти выбранный
                                  в конечном счёте для использования.
    --no-memory-configuration   Не пытаться определить тип основной памяти.
    --no-memory-detection       Не пытаться определять функционирование
                                  заданной конфигурации памяти.
                                  Требует задания аргумента --memory-config.
    --memory-configuration-failure-error-p
                                Считать ошибкой невозможность найти рабочую
                                  конфигурацию памяти.  По умолчанию равно T.
    --keep-target-intact        Свести инициализацию аппаратного обеспечения
                                  к абсолютному минимуму.
                                  Лучше всего функционирует при задании
                                  аргументов --platform и --memory-config
                                  (либо --no-memory-configuration).
    --memory-detection-threshold  При определении типа основной памяти
                                  тестировать безошибочность ввода/вывода
                                  такого объёма памяти.
    --list-contexts             После сканирования интерфейсов вывести список
                                  контекстов целевых устройств и выйти.
    --context <context-id>      После сканирования интерфейсов активировать
                                  контекст обозначенного целевого устройства.
    --no-rc                     Не загружать ~~/.comdbrc
    --load <filename>           Исполнить команды из файла.
    --eval <lisp-expr>          Выполнить Лисп-выражение.
    --run-tests                 Выполнить предварительные тесты.
                                  При возникновении ошибок выйти.
    --ignore-test-failures      Игнорировать ошибки при исполнении тестов.
    --examine-tlb               Использовать TLB для разрешения вирт. адресов.
    --quit                      Выйти, вместо входа в отладчик.
    --verbose                   Печатать БОЛЬШЕ ИНФОРМАЦИИ во время работы.
                                  Подразумевает --print-backtrace-on-errors.
    --version                   Вывести версию и выйти.
    --help                      Вывести справку на английском языке и выйти.
    --help-ru                   Вывести эту справку и выйти.")

(defvar *flasher-help-ru*
  "  Настройки флэшера:
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
    --print-checksums           Печатать контрольные суммы при записи/чтении.
    --dry-run                   Симулировать запись: делать всё кроме неё.")

(defvar *gdbserver-help-ru*
  "  Настройки сервера GDB:
    --address <dotted-quad>     Адрес интерфейса на котором следует
                                  ожидать соединений.  По умолчанию равен
                                  127.0.0.1.
    --port <integer>            Номер TCP-порта на котором следует ожидать.
                                  По умолчанию равен 9000.
    --trace-comdb-calls         Протоколировать все вызовы API common-db.
    --trace-even-noisy-comdb-calls
                                Протоколировать также и шумные вызовы
                                  API common-db.
    --trace-comdb-memory-io     Трассировать обмены с памятью.
    --single-shot               Выйти по окончании первой сессии.")
