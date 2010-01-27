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
     (reginstance-value (register-instance (target-enumpool target) name-or-address)))))

(defun set (name-or-address value &aux
            (target *target*))
  #+help-ru
  "Установить регистр или ячейку памяти заданный/-ую через спецификатор адреса
NAME-OR-ADDRESS.  Также доступен синтаксис (setf (get NAME-OR-ADDRESS) VALUE)."
  (etypecase name-or-address
    (integer
     (setf (memory-ref target name-or-address) value))
    (keyword
     (setf (reginstance-value (register-instance (target-enumpool target) name-or-address)) value))))

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
                      (and address-map (addr-in-map-p address-map value)
                           (if *map-to-zeroth-page*
                               (logand #x3fff (virt-to-phys address-map value))
                               (virt-to-phys address-map value)))
                      value))
         (cell (when address (memory-ref target address)))
         (decoded-insn (when address (decode-mips-insn cell)))
         (control (format nil "~~&~~A:~~12T~~8,'0X~~:[~~; ~~:*~~S~~]~~
                               ~~~DT~~
                               ~~:[~~;~~:*@phys #x~~8,'0X: ~~8,'0X   ~~(~~A~~{ ~~S~~}~~)~~]" postname-alignment)))
    (format *log-stream* control
            name value
            (addrsym value)
            address
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
