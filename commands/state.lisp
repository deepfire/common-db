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

(defun dive (&aux
             (core *core*))
  #+help-ru
  "Войти в более глубокий режим отладки, или остановить целевое устройство.
Схема состояний: :FREE -> :STEP -> :DEBUG."
  (setf (state core) (deeper-state (state core)))
  (values))

(defun rise (&aux
             (core *core*))
  #+help-ru
  "Войти в более лёгкий режим отладки, или запустить целевое устройство.
Схема состояний :DEBUG -> :STEP -> :FREE."
  (setf (state core) (shallower-state (state core)))
  (values))

(defun step (&optional (count 1) &key (display *display*) (step-slaves t) &aux
             (core *core*))
  #+help-ru
  "Произвести COUNT шагов."
  (with-temporary-state (core :stop)
    ;; re-implementing the stop-to-free protocol, what's to say...
    (setf (core-moment core) (derive-moment (saved-core-moment core)
                                            (or (moment-fetch (saved-core-moment core)) #xbfc00000)))
    ;; shall we use OTC/TRACE-MODE here, instead (wouldn't that be a lie)?
    (iter (repeat count)
          (while (prog1 (step-core-synchronous core step-slaves)
                   (when step-slaves
                     (dolist (slave (core-slaves core))
                       (when (core-running-p slave)
                         (step-core-debug slave)))))))
    (free-to-stop core))
  (when display (display))    
  (values))

(defun callog (&optional target-symbol &key (core *core*) step-slaves report-normal-returns skiplist)
  #+help-ru
  "Производить исполнение по шагам, регистрируя переходы между функциями,
до попадания в функцию, чьё название задано параметром TARGET-SYMBOL.
Функции, имена которых находятся в списке SKIPLIST исключаются из детального
анализа и пропускаются в ускоренном режиме."
  (declare (optimize debug))
  (flet ((current-sym (&aux (fetch (moment-fetch (core-moment core))))
           (values (addrsym fetch) fetch))
         (frame-addr-match-p (f1 f2)
           (= (cdr f1) (cdr f2)))
         (frame-by-name (symstack name)
           (find name symstack :key #'car))
         (return-address-for-pc (pc skip-p)
           (+ pc (* (if skip-p 3 2) (memory-device-byte-width (backend core)))))
         (report-n-deep (n control-string &rest args)
           (write-string ">>> ")
           (iter (repeat n)
                 (for i from 0)
                 (write-string (if (evenp i) "... " ",,, ")))
           (apply #'format t control-string args)
           (terpri))
         (skip-to-address (addr)
           (with-free-hardware-breakpoints (core) ((b addr))
             (run-core-synchronous core)))
         (do-one-step ()
           (step-core-synchronous core step-slaves)
           (when step-slaves
             (dolist (slave (core-slaves core))
               (when (core-running-p slave)
                 (step-core-debug slave))))))
    (with-temporary-state (core :stop)
      (let ((symstack (list (cons (current-sym) 0)))
            skip-to-addr
            last-reported-call (last-call-repeat-count 0))
        (report-n-deep 0 "~A" (caar symstack))
        (iter (for from-fn-symbol first (caar symstack) then to-fn-symbol)
              (for old-pc first 0 then pc)
              (if skip-to-addr
                  (progn
                    (skip-to-address skip-to-addr)
                    (setf skip-to-addr nil))
                  (do-one-step))
              (for (values to-fn-symbol pc) = (current-sym))
              (until (and to-fn-symbol (eq to-fn-symbol target-symbol)))
              (unless (eq to-fn-symbol from-fn-symbol)
                ;; there is a potentially reportable change in location
                ;; see if this is a return, or a call into a new location
                (if-let ((return-depth (position (cons to-fn-symbol pc) symstack :test #'frame-addr-match-p)))
                  (setf symstack (nthcdr return-depth symstack)
                        (values) (if (= 1 return-depth)
                                     (when report-normal-returns
                                       (report-n-deep (1- (length symstack)) "~A <== ~A" to-fn-symbol from-fn-symbol))
                                     (report-n-deep (1- (length symstack)) "~A <= ~A (NLR ~D frame~:*~P)"
                                                    to-fn-symbol from-fn-symbol return-depth)))
                  (let* ((skip-p (find to-fn-symbol skiplist))
                         (return-addr (return-address-for-pc old-pc skip-p))
                         (report-depth (length symstack)))
                    ;; we might want to execute specific code subtrees at native speed, without tracing, termed "skipping"
                    (cond (skip-p
                           (setf (cdr (frame-by-name symstack (addrsym return-addr))) (+ return-addr (memory-device-byte-width (backend core))))
                           (setf skip-to-addr return-addr))
                          (t
                           (setf (cdr (first symstack)) return-addr)))
                    (push (cons to-fn-symbol 0) symstack)
                    ;; report call, abbreviating repeated call sequences
                    ;; XXX: somewhat misrepresents the self-recursion
                    (if (eq to-fn-symbol last-reported-call)
                        (incf last-call-repeat-count)
                        (let ((compressed-call-p (plusp last-call-repeat-count)))
                          ;; in case the call is compressed, this is the tail part abbreviation,
                          ;; with the jumped-to call to be reported later
                          (report-n-deep report-depth "~:[~*~;~D more call~:*~P to ~]~A~:[~*~; ~8,'0X~]~:[~; (skipped)~]"
                                         (and compressed-call-p (not (= 1 last-call-repeat-count))) last-call-repeat-count
                                         (if compressed-call-p last-reported-call to-fn-symbol)
                                         (and (not compressed-call-p) (null to-fn-symbol)) pc skip-p)
                          (setf last-reported-call to-fn-symbol)
                          (when compressed-call-p
                            ;; now, report the actual call
                            (report-n-deep report-depth "~A~:[~*~; ~8,'0X~]~:[~; (skipped)~]"
                                           to-fn-symbol (null to-fn-symbol) pc skip-p)
                            (setf last-call-repeat-count 0))))))))))))

(defun stepw (&key (display *display*) (step-slaves t) report-normal-returns &aux
              (core *core*))
  #+help-ru
  "Произвести COUNT шагов, пропуская вложенные вызовы."
  (with-temporary-state (core :stop)
    ;; re-implementing the stop-to-free protocol, what's to say...
    (setf (core-moment core) (derive-moment (saved-core-moment core)
                                            (or (moment-fetch (saved-core-moment core)) #xbfc00000)))
    ;; shall we use OTC/TRACE-MODE here, instead (wouldn't that be a lie)?
    (if-let ((start-fn-symbol (addrsym (moment-fetch (saved-core-moment core)))))
            (callog start-fn-symbol :core core :step-slaves step-slaves :report-normal-returns report-normal-returns)
            (write-line "Sorry, but the current PC doesn't resolve -- no function to step within."))
    (free-to-stop core))
  (when display (display))    
  (values))

(defun run-async (&rest params &key address moment-changed (core *core*) &allow-other-keys)
  #+help-ru
  "Вывести целевое устройство из режима отладки и запустить его асинхронно,
не дожидаясь.

Принимаются следующие ключевые аргументы:
  - ADDRESS ::
    заместить содержимое регистра адреса выбираемой инструкции,
    и как следствие, перевести поток исполнения на заданный адрес."
  #-help-ru
  "Start CORE, optionally from ADDRESS."
  (declare (ignore params))
  (run-core-asynchronous core address moment-changed)
  (values))

(defun explain (&optional (core *core*))
  #+help-ru
  "Отобразить причину останова ядра CORE.
По умолчанию используется ядро текущего активного устройства."
  (format *log-stream* "~@<Stop reason of ~S: ~A.~:@>~%" core (core-stop-reason core))
  (addr (moment-fetch (saved-core-moment core))))

(defun run (&rest params &key (core *core*) (explain *explain*) (display *display*) (watch *watch*) (watch-fn *watch-fn*)
            (iteration-period 10000000) (watch-period 100) &allow-other-keys)
  #+help-ru
  "Вывести ядро CORE из режима отладки, запустить его, дождаться его останова
и войти в режим отладки.

Принимаются следующие ключевые аргументы:
  - ADDRESS ::
    заместить содержимое регистра адреса выбираемой инструкции,
    и как следствие, перевести поток исполнения на заданный адрес."
  #-help-ru
  "Start CORE, optionally from ADDRESS, and wait until it stops, periodically
polling it for status and optionally executing WATCH-FN, when WATCH is
non-NIL.  Upon stop, optionally EXPLAIN the stop reason and DISPLAY some data."
  (let* ((prefilter (remove-from-plist params :core :watch :explain :display)) 
         (params (if watch
                     (list* :watch-fn watch-fn :iteration-period iteration-period :watch-period watch-period
                            (remove-from-plist prefilter :watch-fn :iteration-period :watch-period))
                     prefilter)))
    (apply #'run-core-synchronous core :moment-changed nil params)
    (when explain (explain core))
    (when display (display))
    (values)))

(defun run-while-boring (&rest boring-syms)
  #+help-ru
  "Запустить целевое устройство, продолжая затем запускать его, при остановах,
до тех пор пока при очередном останове на конвейере не появятся адреса не покрытые
символами из остаточного списка параметров."
  #-help-ru
  "Keep RUNning while upon stop we have some of the BORING-SYMS
on the pipeline."
  (iter (run :explain nil :display nil)
        (for pipesyms = (pipesyms))
        (while (intersection boring-syms pipesyms)))
  (when *explain*
    (explain))
  (when *display*
    (display))
  (values))

(defun run-while-step-in (address &optional (nsteps 1) verbose &key (explain *explain*) (display *display*) &aux
                          (core *core*))
  #+help-ru
  "Запустить целевое устройство, осуществляя NSTEPS шагов при остановах и
проводя сравнение регистра адреса выбираемой инструкции со значением параметра
ADDRESS, запуская кристалл дальше, в цикле, если сравнение успешно, и выходя
из цикла в обратном случае."
  #-help-ru
  "Keep running, until post-stop NSTEPS make the *CORE*'s continuation
address to be different from ADDRESS."
  (let ((iterations 0)
        (count (get :count)))
    (iter (run :explain nil :display nil)
          (incf iterations)
          (step nsteps :display nil)
          (while (= address (moment-fetch (saved-core-moment core))))
          (when verbose
            (let ((newcount (get :count)))
              (format *log-stream* "~D stops, dcount:~20T~8,' X~%" iterations (- newcount count))
              (setf count newcount))))
    (format *log-stream* "After ~D stops continuation address is ~8,'0X, not ~8,'0X~%"
            iterations (moment-fetch (saved-core-moment core)) address)
    (when explain
      (explain))
    (when display
      (display)))
  (values))

(defun run-until-stopped-by (reason &rest run-params &key (otherwise :report-and-continue) &allow-other-keys)
  #+help-ru
  "Запустить кристалл, дождаться останова, анализируя затем его причину и
возвращая успех при совпадении причины со значением параметра REASON.
Реакция на несовпадение определяется значением ключевого параметра OTHERWISE,
интерпретируемого следующим образом:
  - :RETURN ::
    вернуть ложный статус,
  - :CONTINUE ::
    продолжить цикл,
  - :REPORT-AND-CONTINUE :: 
    продолжить цикл, отобразив причину останова.

Остаточные параметры (за исключением OTHERWISE) передаются команде RUN."
  (let ((downstream-params (remove-from-plist run-params :otherwise)))
    (loop (let ((stop-reason (apply #'run downstream-params)))
            (if (equal stop-reason reason)
                (return t)
                (ecase otherwise
                  (:return (return (values nil stop-reason)))
                  (:continue)
                  (:report-and-continue (format *log-stream* "RUN-UNTIL-STOPPED-BY: was stopped due to ~S, continued.~%"
                                                stop-reason))))))))

(defun trace (count &rest run-params)
  #+help-ru
  "Исполнить COUNT инструкций и остановиться.

Остаточные параметры передаются команде RUN."
  (settrace count)
  (apply #'run run-params)
  (values))

(defun ret (&rest run-params &aux
            (core *core*))
  #+help-ru
  "Установить точку останова в значение регистра R31, предположительно
содержащий актуальный адрес возврата и выполнить команду RUN.

Остаточные параметры передаются команде RUN."
  (with-temporary-state (core :debug)
    (let ((ra (devreg core :ra)))
      (format t "setting a return break to ~8,'0X, ~S~%" ra (addrsym ra))
      (hw-break 0 ra)))
  (apply #'run run-params)
  (values))
