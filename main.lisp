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

;;;
;;; Infra
;;;
(defvar *spaces*           '(:interface :platform :core :dsp))

(defvar *default-initargs* '(:reset-p t :stop-cores-p t)
  "Default device context initialization argument list.")

;;;
;;; CLI
;;;
(defvar *explain*            t)
(defvar *examine-tlb*        nil)
(defvar *map-to-zeroth-page* nil)
(defvar *display*            t)
(defvar *watch*              t)
(defvar *watch-fn*           'print-pipeline-terse)
(defvar *log-interface-bus-discovery* nil)

;;;;
;;;; State
;;;;
(defvar *depth*            :debug)

;;;;
;;;; Software
;;;;
(defvar *syms*             nil)

;;;;
;;;; Hooks into *CTX*, *CORE* etc
;;;;
(defmethod coerce-to-trap ((o integer))
  (if-let ((traps (traps *core* o)))
    (first traps)
    (error 'no-core-breakpoint :core *core* :address o)))

;;;;
;;;; Control
;;;;
(defun init ()
  "Prepare the debugger environment.
To be called once, before any use of COMMON-DB."
  (setf *print-circle* t
        *print-base* #x10)
  (init-device-model)
  (set-namespace :interface :platform :core :dsp)
  (values))

;;;;
;;;; Discovery
;;;;
(defmethod bus-add ((o interface-bus) interface)
  (with-maybe-logged-device-io (interface t *log-interface-bus-discovery*)
    (let* ((platform-initargs (or (args) *default-initargs*))
           (target (apply #'make-instance (find-target-class-for-interface interface) :backend interface platform-initargs))
           (core (target-device target '(core:general-purpose-core 0)))
           ;; And one to bind them all..
           (ctx (make-instance 'target-context :interface interface :target target :core core :initargs platform-initargs)))
      (push ctx *target-contexts*)
      (set-context ctx))))

(defmethod bus-remove :after ((o interface-bus) interface)
  (remove-context (find interface *target-contexts* :key #'ctx-interface)))

(defmethod add-target-device :after ((target target) (o mips-core))
  (patch-core-pipeline-reginstances o))

(defun merge-plists (overridee overrider)
  (lret ((result (copy-list overridee)))
    (iter (for (k v) on overrider by #'cddr)
          (setf (getf result k) v))))

(defun scan (&rest initargs &key force-rescan virtual (physical (not virtual)) tapserver-address tapserver-port &allow-other-keys)
  #+help-ru
  "Функция производит следующие операции:

  - поиск адаптеров подключенных к EPP и USB,
  - для каждого подключенного адаптера:
    - анализ OnCD подключенного к адаптеру целевого устройства,
    - определение типа платформы целевого устройства,
    - инициализацию компонентов платформы целевого устройства.

Если функции первым параметром передан не-NIL, то производится повторный анализ
уже найденных к данному моменту адаптеров с подключенными к ним целевыми устройствами.

При нескольких подключенных устройствах, активным становится последнее найденное.
При этом, следует отметить что шина USB сканируется после портов EPP, что, как следствие,
придаёт адаптерам на шине USB определённый приоритет."
  (setf *print-circle* t
        *print-base* #x10)
  (let ((*initargs* (merge-plists *initargs* (remove-from-plist initargs
                                                                :force-rescan :virtual :physical :tapserver-address :tapserver-port))))
    (scan-interface-busses :force-rescan force-rescan
                           :virtual virtual
                           :physical physical
                           :tapserver-address tapserver-address :tapserver-port tapserver-port))
  (values))

(defun reset (&rest platform-args &key (core *core*) (state *depth*) &allow-other-keys)
  #+help-ru
  "Функция сбрасывает и перенастраивает целевое устройство содержащее ядро CORE
 (по умолчанию выбирается являющееся активным в данный момент), с учётом ключевых аргументов PLATFORM-ARGS.
Производятся следующие действия:

  * осуществляется попытка выяснения текущие значения делителей частоты;
  * кристалл сбрасывается в первоначальное состояние;
  * системные компоненты перенастраиваются с использованием либо сохранённых значений
    делителей частоты, либо значений по умолчанию для данной платформы.
  * устанавливается соответствующая текущим настройкам глубина режима отладки (по умолчанию – :DEBUG)."
  #-help-ru
  "Reset the target device."
  (let ((prereset-mult (ignore-errors (core-frequency-multiplier core)))
        (final-platform-args (merge-plists (args) platform-args)))
    (when-let ((multiplier (or (getf final-platform-args :core-multiplier)
                               (when (core-frequency-multiplier-valid-p core prereset-mult)
                                 prereset-mult))))
      (setf (getf final-platform-args :core-multiplier) multiplier))
    (apply #'reset-platform core (remove-from-plist final-platform-args :core :state))
    (setf (state core) state)
    (values)))

(defgeneric default-display-list (platform interface target core))