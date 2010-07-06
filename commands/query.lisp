;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2010, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2007-2010 by
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


(defvar *display-packs*)

(defun get-register (name)
  #+help-ru
  "Найти регистр соответствующий NAME в текущем целевом контексте."
  (register-instance (target-enumpool *target*) name))

(defun coerce-to-register (register-or-name)
  #+help-ru
  "Если REGISTER-OR-NAME представляет собой ключевое слово, найти и вернуть
соответствующий регистр.  Если же он представляет собой регистр, попросту
вернуть его."
  (etypecase register-or-name
    (keyword (get-register register-or-name))
    (register-instance register-or-name)))

(defun register-address (register)
  #+help-ru
  "Найти адрес регистра соответствующего спецификатору регистра REGISTER,
в том случае если он соответствует регистру отображённому в память."
  (when-let* ((ri (coerce-to-register register))
              (device (reginstance-device ri)))
    (if (mapped-device-p device)
        (mapped-device-register-address device (name (reginstance-register ri)))
        (error "~@<The device of type ~A, that register ~A refers to, is not a memory-mapped device.~:@>"
               (type-of device) (name ri)))))

(defun compute-raw-register-value (name bitfield-names bitfield-values &aux
                                   (ri (register-instance (target-enumpool *target*) name))
                                   (device (reginstance-device ri)))
  (values (mapped-device-register-address device (name (reginstance-register ri)))
          (fbits bitfield-names bitfield-values)))

(defun parse-raw-register-value (address raw-value &aux
                                 (target *target*)
                                 (address (fixmap-address target address))
                                 (artifact (target-artifact-by-address target address)))
  (if (typep artifact 'register-instance)
      (let* ((reg (reginstance-register artifact))
             (bitfield-values (format-decode (reg-format reg) raw-value)))
        (values (name reg)
                (mapcar #'car bitfield-values)
                (mapcar #'cdr bitfield-values)))
      (error "~@<Address ~X contains ~S, which is not a device register.~:@>" address artifact)))

(defun get (name-or-address &aux
            (target *target*))
  #+help-ru
  "Получить регистр или ячейку памяти заданный/-ую через спецификатор адреса
NAME-OR-ADDRESS, представляющем собой либо ключевое слово."
  (etypecase name-or-address
    (integer
     (memory-ref target name-or-address))
    (keyword
     (target-reg target name-or-address))))

(defun set (name-or-address value &aux
            (target *target*))
  #+help-ru
  "Установить регистр или ячейку памяти заданный/-ую через спецификатор адреса
NAME-OR-ADDRESS.  Также доступен синтаксис (setf (get NAME-OR-ADDRESS) VALUE)."
  (etypecase name-or-address
    (integer
     (setf (memory-ref target name-or-address) value))
    (keyword
     (setf (target-reg target name-or-address) value))))

(defsetf get set)

(defgeneric show-value-using-value (name-or-address value)
  (:method ((o integer) (value integer)) value)
  (:method ((o symbol) (value integer))  value)
  (:method ((o register-instance) (value integer))
    (let ((format (reg-format (reginstance-register o))))
      (xform format (curry #'format-decode format) value))))

(defun show (name-or-address &aux
             (core *core*)
             (target (backend core)))
  #+help-ru
  "Получить регистр или ячейку памяти заданный/-ую через спецификатор адреса
NAME-OR-ADDRESS.  В том случае если указано имя регистра имеющего структуру,
полученное значение разобирается по полям, и возвращается как второе значение
функции."
  (multiple-value-bind (discriminator value)
      (etypecase name-or-address
        (integer
         (values name-or-address (memory-ref target name-or-address)))
        (keyword
         (let ((reginstance (register-instance (enumerated-pool core) name-or-address)))
           (values reginstance (with-retry-with-state-restart (core :debug)
                                 (reginstance-value reginstance)))))
        (symbol
         (values name-or-address (memory-ref target (symaddr name-or-address))))
        (register-instance
         (values name-or-address (with-retry-with-state-restart (core :debug)
                                   (reginstance-value name-or-address)))))
    (let ((extended-value (show-value-using-value discriminator value)))
      (if (eql extended-value value)
          value
          (values value extended-value)))))

(defun display-code-address-register-value (name value &key (postname-alignment 0) &aux
                                            (core *core*)
                                            (target (backend core)))
  (let* ((allow-invasive (eq (state core) :debug))
         (address-map (when (and (not (= 2 (ash value -30))) ; YYY: mapped-p
                                 allow-invasive
                                 (eq (devbit-decode core :config :mmu-mode) :tlb)
                                 *examine-tlb*)
                        (tlb-address-map core (get-tlb core) #x4000)))
         (address (if address-map
                      (and (addr-in-map-p address-map value)
                           (if *map-to-zeroth-page*
                               (logand #x3fff (virt-to-phys address-map value))
                               (virt-to-phys address-map value)))
                      value))
         (cell (when address (memory-ref target address)))
         (decoded-insn (when address (decode-mips-insn cell)))
         (control (format nil "~~&~~A:~~12T~~8,'0X~~:[~~; ~~:*~~S~~]~
                               ~~~DT~
                               ~~:[~~;@[~~8,'0X~~:[~~*~~;=> ~~8,'0X~~]]: ~~8,'0X   ~~(~~A~~{ ~~S~~}~~)~~]" postname-alignment)))
    (format *log-stream* control
            name value (addrsym value)
            address value (and address-map (addr-in-map-p address-map value)) address
            cell
            (car decoded-insn) (cdr decoded-insn))
    (when (typep (car decoded-insn) 'branch-insn)
      (write-string "  " *log-stream*)
      (describe-insn-jump-target *log-stream* value (car decoded-insn) (cdr decoded-insn)))))

(defun displash (address &optional (splash-size #x10) annotations)
  #+help-ru
  "Дизассемблирование адресов в коридоре вокруг ADDRESS.
Величина коридора задаётся параметром SPLASH-SIZE."
  (disasm (- address splash-size) (* 2 splash-size) nil annotations))

(defun print-tlb (&optional (page-size #x4000) &aux
                  (core *core*))
  #+help-ru
  "Распечатать содержимое TLB.  Размер отображаемый страницей принимается
равным PAGE-SIZE."
  (tlb-address-map core (get-tlb core) page-size))

(defun pipesyms ()
  #+help-ru
  "Вернуть список имён функций соответствующих адресам на конвейере,
используя активную таблицу символов."
  #-help-ru
  "Return a list of symbols referred to by the pipeline."
  (remove-duplicates (mapcar #'addrsym (core-pipeline-addresses *core*))))

(defun print-generic-register-and-value (name value &optional tail)
  (format *log-stream* "~&~A:~12T~8,'0X~:[~; ~:*~A~]~%" name value tail))

(defgeneric display-register-using-core (core name &key &allow-other-keys)
  (:method ((o core) name &rest args)
    (declare (ignore args))
    (multiple-value-bind (value extended-value) (show name)
      (print-generic-register-and-value name value extended-value))))

(defgeneric display-register (name &key &allow-other-keys)
  #+help-ru
  (:documentation
   "Интеллектуально отобразить регистр с именем NAME.")
  (:method (name &rest args &aux
            (*print-right-margin* 120)
            (*print-escape* nil))
    (apply #'display-register-using-core *core* name args)))

(defun display (&rest new-items)
  #+help-ru
  "Печатать значения регистров чьи имена находятся в /месте/
 (DISPLAY-LIST).  Когда команде передаются параметры, они замещают
старое значение (DISPLAY-LIST)."
  #-help-ru
  "Print the values of registers whose names currently are in (DISPLAY-LIST).
When NEW-ITEMS is non-NIL, change (DISPLAY-LIST) before printing."
  (when new-items
    (setf (display-list) new-items))
  (mapc #'display-register (display-list))
  (values))

(defun dispack (&optional (name :pipeline))
  #+help-ru
  "Напечатать значения регистров из заготовленной пачки с именем NAME."
  (if-let ((pack (assoc name *display-packs*)))
    (mapc #'display-register (rest pack))
    (error "~@<No display pack with name ~A.~:@>" name))
  (values))

(defun pipeline ()
  #+help-ru
  "Как DISPLAY, но анализирует лишь адреса на конвейере."
  #-help-ru
  "Pretty-print the pipeline."
  (dispack :pipeline)
  (values))

(defgeneric edisplay-using-core (core)
  (:method-combination most-specific-last)
  (:method ((o mips-core))
    (let* ((epc (show :epc))
           (badvaddr (show :badvaddr)))
      (display)
      (format t "count: ~X~%" (show :count))
      (format t "EPC:   ~8,'0X, ~S~%" epc (addrsym epc))
      (format t "badvaddr: ~8,'0X, ~S~%" badvaddr (addrsym badvaddr))
      (format t "cause:~{ ~A~}~%" (nth-value 1 (show :cause)))
      (format t "code @ PC:~%")
      (when (next-method-p)
        (call-next-method)))))

(defun edisplay ()
  #+help-ru
  "Как DISPLAY, с добавлением типично востребованной информации:
cop0.count, cop0.epc, cop0.badvaddr, cop0.cause и дизассемблированием
адресов вокруг pcfetch и cop0.epc."
  (edisplay-using-core *core*))