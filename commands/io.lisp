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

(defun loadelf (filename &key check csum dump)
  #+help-ru
  "Загрузить содержимое FILENAME в формате ELF в память устройства
и установить точку входа.

Принимаются следующие ключевые аргументы:
  - CHECK ::
    включить проверку целостности записи."
  #-help-ru
  "Load an ELF executable from FILENAME into target device memory, and prepare it for execution."
  (prime-core-executable *core* (loadable:extract-loadable :elf filename) :check check :report t :checksum csum :dump dump))

(defun loadldr (filename &key check csum dump)
  #+help-ru
  "Загрузить содержимое FILENAME в формате LDR-MCS в память устройства
и установить точку входа.

Принимаются следующие ключевые аргументы:
  - CHECK ::
    включить проверку целостности записи."
  #-help-ru
  "Load an LDR executable from FILENAME into target device memory, and prepare it for execution."
  (prime-core-executable *core* (loadable:extract-loadable :ldr filename) :check check :checksum csum :report t :dump dump))

(defun loadeltext (filename &key (entry-point #xbfc00000) check csum dump)
  #+help-ru
  "Загрузить содержимое FILENAME в текстовом формате 'eltext' в память
устройства и установить точку входа.

Принимаются следующие ключевые аргументы:
  - CHECK ::
    включить проверку целостности записи.

  - ENTRY-POINT ::
    установить точку входа."
  #-help-ru
  "Load eltext from FILENAME into target device memory, and prepare it
for execution.  Note that, as ENTRY-POINT isn't provided by the format,
it has to be specified by ENTRY-POINT (which defaults to #xBFC00000)."
  (prime-core-executable *core* (loadable:extract-loadable :eltext filename :entry-point entry-point) :check check :checksum csum :report t :dump dump))

(defun loadbin (filename address &key check)
  #+help-ru
  "Загрузить сырые байты из FILENAME по ADDRESS.

Принимаются следующие ключевые аргументы:
  - CHECK ::
    включить проверку целостности записи."
  #-help-ru
  "Load raw bytes from FILENAME into memory at address ADDRESS."
  (loadable:upload-loadable *core* (loadable:extract-loadable :raw filename :base address) :check check))

(defun make-annotating-disassembly-printer (annotations)
  (lambda (stream addr opcode width insn params)
    (if-let ((name (cdr (assoc addr annotations :test #'eq))))
      (let ((length (length name)))
        (format stream "~8,'0X:    ~/core::print-variable-length-opcode/" addr (cons opcode width))
        (dotimes (i (- 3 length))
          (write-char #\Space stream))
        (write-string name stream)
        (format stream " ~A~1,14T~{~S,~2,6T~}" insn params))
      (default-disassembly-line-printer stream addr opcode width insn params))))

(defun describe-insn-jump-target (stream addr insn insnargs)
  (when-let ((jump-target (branch-insn-target-address insn addr insnargs)))
    (format stream "~8,'0X: ~A" jump-target (addrsym jump-target))))

(defun disasm (&optional address-or-symbol length line-pre-annotate-fn annotations &aux
               (core *core*))
  #+help-ru
  "Дизассемблировать содержимое диапазона ячеек памяти начиная с
ADDRESS-OR-SYMBOL и длиной LENGTH.

Значение параметра ADDRESS-OR-SYMBOL по умолчанию равно содержимому
регистра адреса выборки конвейера уменьшенному на #x10.

Значение параметра LENGTH по умолчанию равно либо #x20, если ADDRESS-OR-SYMBOL представляет собой целое,
либо разнице между адресами символов ADDRESS-OR-SYMBOL и следующего за ним."
  #-help-ru
  "Disassemble LENGTH bytes of target device's memory, starting from ADDRESS-OR-SYMBOL, defaulting to PCfetch.
ADDRESS-OR-SYMBOL must be aligned by 4."
  (let ((address (if address-or-symbol
                     (coerce-to-address address-or-symbol :if-not-found :error)
                     (max 0 (- (moment-fetch (saved-core-moment core)) #x10))))
        (length (or length (if (and address-or-symbol (symbolp address-or-symbol))
                               (symlength address-or-symbol)
                               #x20))))
    (check-address-alignment 4 address)
    (core-disassemble core address length
                      :line-pre-annotate-fn line-pre-annotate-fn
                      :line-fn (if annotations
                                   (make-annotating-disassembly-printer annotations)
                                   #'default-disassembly-line-printer)
                      ;; this just asks for a CLOS GF
                      :line-post-annotate-fn (lambda (stream addr insn insnargs)
                                               (when (typep insn 'branch-insn)
                                                 (describe-insn-jump-target stream addr insn insnargs))))
                      
    (values)))

(defun dump (address-or-symbol &optional (length #x100))
  #+help-ru
  "Напечатать содержимое диапазона ячеек памяти начиная с ADDRESS-OR-SYMBOL
и длиной LENGTH."
  #-help-ru
  "Print LENGTH bytes of target device's memory, starting from ADDRESS-OR-SYMBOL."
  (let* ((ioaddr (coerce-to-address address-or-symbol))
         (iovec (make-array length :element-type '(unsigned-byte 8))))
    (read-block *target* ioaddr iovec)
    (print-u8-sequence *standard-output* iovec :address ioaddr)
    (values)))

(defun peek (address-or-symbol &optional verbose)
  #+help-ru
  "Напечатать содержимое ячейки по адресу ADDRESS-OR-SYMBOL, опционально,
при указанном не-NIL значении параметра VERBOSE, расшифровывая значение
ADDRESS-OR-SYMBOL в контексте активной таблицы символов."
  #-help-ru
  "Read a single memory cell at ADDRESS."
  (when verbose
    (addr address-or-symbol))
  (memory-ref *target* (coerce-to-address address-or-symbol)))

(defun print-memory (address length)
  #+help-ru
  "Напечатать содержимое диапазона ячеек памяти начиная с ADDRESS и
длиной LENGTH, как текст."
  #-help-ru
  "Print a LENGTH bytest long region of TARGET's memory, starting from ADDRESS."
  (let* ((ioaddr (coerce-to-address address))
         (iovec (make-array length :element-type '(unsigned-byte 8))))
    (read-block *target* ioaddr iovec)
    (let* ((end (position 0 iovec))
           (textvec (make-array end :element-type 'character)))
      (map-into textvec #'code-char (subseq iovec 0 end))
      (format t "~&===== Printing ~4,' X bytes:  ~8,'0X..~8,'0X ==========~%" end address (+ address end))
      (write-string textvec)
      (format t "~&=========================================================~%")
      (values))))

(defun saveeltext (filename address length &aux
                   (target *target*))
  #+help-ru
  "Сохранить содержимое диапазона ячеек памяти начиная с ADDRESS и
длиной LENGTH байт в FILENAME, в формате 'eltext'.

ADDRESS и LENGTH должны быть выровнены по 16."
  #-help-ru
  "Save a chunk of memory, LENGTH bytes long, at address ADDRESS, into FILENAME, in eltext format.

ADDRESS and LENGTH must be aligned by 16."
  (check-address-alignment 16 address)
  (check-size-alignment 16 length)
  (with-output-to-file (stream filename)
    (eltext:u8-extent-print-as-eltext (u8-extent target (extent (fixmap-address target address) length))
                                      stream))
  (values))

(defun savebin (filename address length &aux
                (target *target*))
  #+help-ru
  "Сохранить содержимое диапазона ячеек памяти начиная с ADDRESS
и длиной LENGTH байт в FILENAME, в сыром виде."
  #-help-ru
  "Save a chunk of memory, LENGTH bytes long, at address ADDRESS, into FILENAME, in raw format."
  (with-output-to-file (stream filename :element-type '(unsigned-byte 8))
    (write-sequence (extent-data (u8-extent target (extent (fixmap-address target address) length)))
                    stream))
  (values))

(defun clearmem (address length &optional (value 0))
  #+help-ru
  "Установить диапазон ячеек памяти начиная с ADDRESS и длиной LENGTH
байт в значение параметра VALUE (0-255)."
  #-help-ru
  "Clear a chunk of memory, LENGTH bytes long, at ADDRESS."
  (write-block *target* (coerce-to-address address)
               (make-array length :element-type '(unsigned-byte 8) :initial-element value))
  (values))