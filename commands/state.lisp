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
    ;; it'd be nice if we could reuse the state machine here...
    (reinstate-saved-moment core)
    ;; shall we use OTC/TRACE-MODE here, instead (wouldn't that be a lie)?
    (iter (repeat count)
          (step-core-synchronous core step-slaves))
    (free-to-stop core))
  (when display (display))    
  (values))

(defun oneline-report (&key print-trail prefix (core *core*))
  #+help-ru
  "Вывести минимальный обзор конвейера: адрес, код и мнемонику декодируемой инструкции."
  (let* ((trail (core-trail core))
         (decode-addr (trail-decode trail))
         (opcode (moment-opcode (core-moment core))))
    (format t "~:[~;~:*~A ~]~8,'0X  ~8,' X:~{ ~A~}~:[~; trail:~{ ~8,'0X~}~]~%"
            prefix decode-addr opcode (decode-insn (core-isa core) opcode)
            print-trail (listify-trail trail))))

(defun callog (&key until (core *core*) step-slaves report-normal-returns skiplist handlers debug qualified-symbols
               verbose-since very-verbose-since stay-for-more)
  #+help-ru
  "Производить исполнение по шагам, регистрируя переходы между функциями,
до попадания в функцию/адрес заданный параметром UNTIL.
Функции, имена которых находятся в списке SKIPLIST исключаются из детального
анализа и пропускаются в ускоренном режиме."
  (declare (optimize debug) (type (or null integer) stay-for-more))
  (let (moment-invaded-p
        (insn-width (memory-device-byte-width (backend core))))
    (labels ((current-sym (&aux (fetch (moment-fetch (current-core-moment core))))
               (values (addrsym fetch) fetch))
             (frame-addr-match-p (pc return-addr)
               ;; XXX: wiggle room, happens to be needed in some cases
               (or (= pc return-addr)
                   (= pc (+ return-addr insn-width))         ; some function calls
                   (= pc (+ return-addr (* 2 insn-width))))) ; some instruction faults (FPU emulation)
             (return-addr-for-pc-change (from to)
               (flet ((user->kernel-p ()
                        "This one is MIPS32-specific."
                        (and (zerop (logand #x80000000 from))
                             (plusp (logand #x80000000 to))))
                      (classify-event ()
                        (case to
                          (#x80000000 :tlb-fault)
                          (#x80000180 :exception))))
                 (let ((event (classify-event)))
                   (values (case event
                             (:tlb-fault (devreg core :epc))
                             (:exception (devreg core :epc))
                             (t          (+ from (* 2 insn-width))))
                           event
                           (when event
                             (devbit-decode core :cause :exccode))
                           (when (eq event :tlb-fault)
                             (devreg core :badvaddr))))))
             (report-n-deep (n control-string &rest args)
               (write-string ">>> ")
               (iter (repeat n)
                     (for i from 0)
                     (write-string (if (evenp i) "... " ",,, ")))
               (apply #'format t control-string args)
               (terpri))
             (report-entry-or-tail (report-depth repeat-count symbol qualify-p kind subkind old-pc pc ret-pc fault-addr skip-p handler-output)
               (report-n-deep report-depth "~:[~;~:*~D more call~:*~P to ~]~
                                            ~A~
                                            ~:[~3*~; ~:[~*~;~8,'0X => ~]~8,'0X~]~
                                            ~:[~; (skipped)~]~
                                            ~:[~3*~;    ******* ~:*~A (~A) *******  return pc: ~8,'0X~:[~;, fault addr: ~:*~8,'0X~]~]~
                                            ~:[~; ~:*~A~]"
                              repeat-count
                              symbol
                              qualify-p kind old-pc pc
                              skip-p
                              kind subkind ret-pc fault-addr
                              handler-output))
             (exit-quick-step ()
               (free-to-stop core)
               (setf moment-invaded-p t))
             (skip-to-address (addr)
               ;; XXX: this looks like it needs yet lighter state...
               (exit-quick-step)
               (with-free-hardware-breakpoints (core) ((b addr))
                 (run-core-synchronous core)))
             (qualify-symbol-p (x)
               (or (null x) (member x qualified-symbols)))
             (quick-step ()
               (when moment-invaded-p
                 ;; restore peace in the tubes...
                 (reinstate-saved-moment core)
                 (setf moment-invaded-p nil))
               (step-core-synchronous core step-slaves)))
      (with-temporary-state (core :stop)
        (let ((symstack (list (cons (current-sym) 0)))
              stay-decay-p
              skip-to-addr
              pc-memory
              verbose-steps very-verbose-steps
              last-reported-call (last-call-repeat-count 0))
          (report-n-deep 0 "~A" (caar symstack))
          (unwind-protect
               (iter (for from-fn-symbol first (caar symstack) then to-fn-symbol)
                     (for old-pc first 0 then pc)
                     (if skip-to-addr
                         (progn
                           (skip-to-address skip-to-addr)
                           (setf skip-to-addr nil))
                         (quick-step))
                     (for (values to-fn-symbol pc) = (current-sym))
                     (cond ((and verbose-since (= pc verbose-since))           (setf verbose-steps t))
                           ((and very-verbose-since (= pc very-verbose-since)) (setf very-verbose-steps t)))
                     (cond (verbose-steps      (push pc pc-memory))
                           (very-verbose-steps (oneline-report :core core)))
                     (until (or (when (typecase until
                                        (null nil)
                                        (integer (= pc until))
                                        (symbol (eq to-fn-symbol until)))
                                  (setf stay-decay-p t)
                                  (not stay-for-more))
                                (and stay-decay-p (minusp (decf stay-for-more)))))
                     (unless (eq to-fn-symbol from-fn-symbol)
                       (when debug
                         (format t "~{~8,'0X ~}| ~8,'0X => ~8,'0X~%" (mapcar #'cdr symstack) old-pc pc))
                       (when verbose-steps
                         (write pc-memory)
                         (terpri)
                         (setf pc-memory nil))
                       ;; there is a potentially reportable change in location
                       ;; see if this is a return, or a call into a new location
                       (if-let ((return-depth (position pc symstack :test #'frame-addr-match-p :key #'cdr)))
                               (setf symstack (prog1 (nthcdr return-depth symstack)
                                                (setf (cdr (first symstack)) 0))
                                     (values) (when debug
                                                (format nil "RET @ stack posn ~D~%" return-depth))
                                     (values) (if (= 1 return-depth)
                                                  (when report-normal-returns
                                                    (report-n-deep (1- (length symstack)) "~A <== ~A" to-fn-symbol from-fn-symbol))
                                                  (report-n-deep (1- (length symstack)) "~A~:[~*~; ~8,'0X~] <= ~A (NLR ~D frame~:*~P)"
                                                                 to-fn-symbol (qualify-symbol-p to-fn-symbol) pc from-fn-symbol return-depth)))
                               (multiple-value-bind (return-addr kind subkind fault-addr) (return-addr-for-pc-change old-pc pc)
                                 (let* ((skip-p (find to-fn-symbol skiplist))
                                        (report-depth (length symstack))
                                        (handler-output (when-let ((handler (cadr (assoc to-fn-symbol handlers))))
                                                          ;; the handler might do anything, so let's obey the state protocol...
                                                          (exit-quick-step)
                                                          (funcall handler to-fn-symbol pc))))
                                   (setf (cdr (first symstack)) return-addr)
                                   (when skip-p
                                     (setf skip-to-addr return-addr))
                                   (push (cons to-fn-symbol 0) symstack)
                                   ;; report call, abbreviating repeated call sequences (unless it's of a non-trivial kind)
                                   ;; XXX: somewhat misrepresents the self-recursion
                                   (if (and (eq to-fn-symbol last-reported-call)
                                            (not kind))
                                       (incf last-call-repeat-count)
                                       (let ((compressed-call-p (plusp last-call-repeat-count)))
                                         ;; in case the call is compressed, this is the tail part abbreviation,
                                         ;; with the jumped-to call to be reported later
                                         (report-entry-or-tail report-depth
                                                               (and compressed-call-p (not (= 1 last-call-repeat-count)) last-call-repeat-count)
                                                               (if compressed-call-p last-reported-call to-fn-symbol)
                                                               (and (not compressed-call-p) (qualify-symbol-p to-fn-symbol)) kind subkind old-pc pc return-addr fault-addr
                                                               skip-p handler-output)
                                         (setf last-reported-call to-fn-symbol)
                                         (when compressed-call-p
                                           ;; now, report the actual call
                                           (report-entry-or-tail report-depth
                                                                 nil
                                                                 to-fn-symbol
                                                                 (and (not compressed-call-p) (qualify-symbol-p to-fn-symbol)) kind subkind old-pc pc return-addr fault-addr
                                                                 skip-p handler-output)
                                           (setf last-call-repeat-count 0)))))))))
            (exit-quick-step)
            (when *display*
              (display))))))))

(defun stepw (&key (display *display*) (step-slaves t) report-normal-returns &aux
              (core *core*))
  #+help-ru
  "Произвести COUNT шагов, пропуская вложенные вызовы."
  (with-temporary-state (core :stop)
    ;; re-implementing the stop-to-free protocol, what's to say...
    (reinstate-saved-moment core)
    ;; shall we use OTC/TRACE-MODE here, instead (wouldn't that be a lie)?
    (if-let ((start-fn-symbol (addrsym (moment-fetch (saved-core-moment core)))))
            (callog :until start-fn-symbol :core core :step-slaves step-slaves :report-normal-returns report-normal-returns)
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
      (hbreak 0 ra)))
  (apply #'run run-params)
  (values))

(defun pause ()
  #+help-ru
  "Вывести сообщение, предлагающее нажать клавишу 'Enter' для продолжения."
  (write-line "Press Enter to continue...")
  (read-line))

(defun save (filename &rest args &key (core *core*) (state t) bank &allow-other-keys)
  #-help-ru
  "Save CORE's state into FILENAME, when STATE is non-NIL, and/or to 'FILENAME.bank',
in the bank format, when BANK is non-NIL."
  #+help-ru
  "Сохранить состояние ядра в файл, в виде чистого состояния, либо в указанное имя файла
с расширением '.bank', в виде банка, исполнение которого приводит к восстановлению
заданного состояния."
  (lret ((pathname (etypecase filename
                     (pathname filename)
                     (string (parse-namestring filename))))
         (core-state (apply #'capture-state core (remove-from-plist args :core :state :bank))))
    (when state
      (write-state core-state pathname))
    (when bank
      (write-state-restorer-bank core core-state (make-pathname :defaults pathname :type "bank")))))

(defun state-to-bank (filename &key (core *core*))
  #-help-ru
  "Resave state from FILENAME into a restorer bank form, in a file with 'bank' extension."
  #+help-ru
  "Пересохранить состояние из FILENAME в форме банк-восстановитель в файле с расширением 'bank'."
  (write-state-restorer-bank core (read-state-for-core core filename) (make-pathname :defaults filename :type "bank")))

(defun testate (&rest args &key filename (core *core*) (state t) bank (pause t) &allow-other-keys)
  #-help-ru
  "Save current state, reset CORE and then apply the saved state."
  #+help-ru
  "Сохранить состояние текущего ядра, сбросить состояние системы,
и затем применить сохранённое состояние."
  (lret ((state (apply #'save filename :core core :state state :bank bank
                       (remove-from-plist args :filename :core :state :bank :pause))))
    (when pause
      (pause))
    (reset :core core)
    (apply-state core state)))

(defun restore (filename &key (core *core*))
  #-help-ru
  "Reset CORE and apply state stored in FILENAME."
  #+help-ru
  "Сбросить ядро, затем восстановив состояние из файла."
  (let ((pathname (etypecase filename
                    (pathname filename)
                    (string (parse-namestring filename)))))
    (reset :core core)
    (switch ((pathname-type pathname) :test #'equal)
      ("bank" (apply-bank core filename))
      (t      (apply-state core filename)))))

(defun findcritmem (state1 state2 repro-p-fn &key (core *core*) (iteration-limit 1) (start 0) (length 67108864) &aux
                    (*core* core)
                    (*display* nil)
                    (*explain* nil))
  #-help-ru
  "Find memory region correlating STATE1 and STATE2."
  (labels ((try (start length)
             (reset)
             (restore state1)
             (run :iteration-limit iteration-limit :if-limit-reached :restore-state-and-break)
             (format t ";; clearing #x~X at #x~X~%" length start)
             (clearmem start length)
             (reset)
             (restore state2)
             (run :iteration-limit iteration-limit :if-limit-reached :restore-state-and-break)
             (funcall repro-p-fn core))
           (rec (start length)
             (if (zerop length)
                 (format t ";; full circle, no memory cleared, yet no reproduction~%")
                 (if (try start length)
                     (rec (+ start length) length)
                     (rec start (ash length -1))))))
    (cond ((not (try 0 0))
           (format t ";; no correlation: no reproduction with no memory cleared~%"))
          ((try start length)
           (format t ";; no correlation: reproduction achieved with all memory cleared~%"))
          (t
           (format t ";; reproduction achieved with no memory cleared, proceeding to find the pesky region...~%")
           (rec start (ash length -1))))))