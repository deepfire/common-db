;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2009-2010, ГУП НПЦ "Элвис"
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

(in-package :common-db)


(defvar *orgify* nil)

(defgeneric read-option (string type)
  (:documentation
   "Parameter values can be of the following types:
  - string
  - binary integer
  - hexadecimal integer
  - decimal integer
  - boolean
This function directly deals with all of those, but the last one,
which is handled implicitly, in the non-default case (the default
case is handled elsewhere).")
  (:method ((s string) (type (eql :string)))
    s)
  (:method ((s string) (type (eql :binary)))
    (when (plusp (length s))
      (parse-integer s :radix 2)))
  (:method ((s string) (type (eql :hex)))
    (when (plusp (length s))
      (parse-integer s :radix 16)))
  (:method ((s string) (type (eql :decimal)))
    (when (plusp (length s))
      (parse-integer s :radix 10)))
  (:method ((s string) (type null))
    "Neutral machine state."
    (cond ((and (> (length s) 2)
                (char= #\- (char s 0)) (char= #\- (char s 1)))
           (make-keyword (string-upcase (subseq s 2))))
          (t
           (read-from-string s)))))

(defun read-args-safely (argv keywords flags)
  (let ((keywords (mapcar #'ensure-list keywords))
        (*read-base* #x10)
        skip-next
        option-type)
    (with-safe-reader-context ()
      (lret ((processed-args
              (iter (for (string . rest) on argv)
                    (when (or skip-next (zerop (length string)))
                      (setf skip-next nil)
                      (next-iteration))
                    (for option = (read-option string option-type))
                    (setf option-type nil)
                    (collect option)
                    (when (keywordp option)
                      (let* ((next-string (car rest))
                             (next-option (when next-string
                                            (read-option next-string nil))))
                        (cond ((and next-string
                                    (typep next-option 'boolean))
                               (setf skip-next t)
                               (collect next-option))
                              ((member option flags)
                               ;; no boolean, but one is expected?  default the flag to T
                               (collect t))
                              (t
                               (destructuring-bind (&optional found-option type (default nil default-provided-p)) (find option keywords :key #'car)
                                 (unless found-option
                                   (error "~@<~A is not a valid keyword argument.~:@>" option))
                                 (if (and (or (keywordp next-option)
                                              (not next-string))
                                          default-provided-p)
                                     ;; expecting a value, got a keyword (or nothing), got a default -- recipe for defaulting
                                     (collect default)
                                     (setf option-type type))))))))))
        (when (oddp (length processed-args))
          (format t "Malformed argument list:~{ ~A~}~%" processed-args)
          (display-invocation-help)
          (quit))))))

(defvar *common-db-release-p* (load-time-value (getenv "RELEASE-P")))
(defvar *common-db-version* (load-time-value (getenv "VERSION")))
(defvar *common-db-commit-id* (load-time-value (getenv "COMMIT-ID")))
(defvar *common-db-date* (load-time-value (getenv "DATE")))

(defun version ()
  #+help-ru
  "Вывести версию отладчика.
Команда различает релизные версии и версии в разработке."
  (format t "~:[~*~;~A, ~]commit-id ~A, ~:[development version, built on ~A~;release, made on ~A~], running on ~A ~A~%"
          *common-db-release-p* *common-db-version* *common-db-commit-id* *common-db-release-p* *common-db-date*
          (lisp-implementation-type) (lisp-implementation-version))
  (values))

(defvar *comdb-help-ru*)
(defvar *comdb-help-en*
  "~%Usage: ~A [ARGS...]

  Lisp runtime options:
    --disable-debugger          Disable Lisp debugger.  Quit on errors.
    --print-backtrace-on-errors Print a backtrace while exiting due to an
                                  unhandled condition when --disable-debugger
                                  was enabled.
    --break-on-signals          Break on conditions with handlers.
    --early-break-on-signals    Enable breaking on conditions early.

  Generic options:
    --early-eval <lisp-expr>    Evaluate a Lisp expression before scanning
                                  interfaces.
    --core-multiplier <integer> Set core frequency multiplier.
    --list-platforms            List all known platforms and quit.
    --platform <platform-name>  Specify platform manually, instead of detection.
    --physical                  Look for physical targets.  Defaults to T,
                                  unless --virtual, --tapserver or --rtlserver
                                  are specified.
    --virtual                   Enable the virtual interface/target/core.
                                  Forces --physical to default to NIL.
    --tapserver [<dotted-quad>] Connect to a tapserver at the optionally 
                                  specified address, defaulting to 127.0.0.1
                                  Forces --physical to default to NIL.
    --rtlserver [<dotted-quad>] Connect to an rtlserver at the optionally 
                                  specified address, defaulting to 127.0.0.1
                                  Forces --physical to default to NIL.
    --trace-exchange [<integer>]  
                                Trace protocol exchanges, whenever applicable.
                                  Whenever applicable, a per-message byte
                                  limit might also be provided, which defaults
                                  to 1024.
    --tapserver-port <integer>  Tapserver TCP port number.
                                  Defaults to 9001.
    --rtlserver-port <integer>  Rtlserver TCP port number.
                                  Defaults to 9001.
    --no-parport                Omit looking for EPP-attached targets.
    --no-usb                    Omit looking for USB-attached targets.
    --no-scan                   Don't scan interfaces.
    --no-platform-init          Do not do platform-level initialisation.
    --memory-config <filename>  Read memory configuration from provided file,
                                  queuing it first in the variant list.
    --print-memory-config       Print the finally chosen memory config.
    --no-memory-configuration   Don't try detecting and configuring memory.
                                  Suppresses --memory-config.
    --no-memory-detection       Don't try detecting if the provided memory
                                  configuration works.
                                  Requires --memory-config.
    --memory-configuration-failure-error-p
                                Consider it an error when no working memory
                                  configuration is found.  Defaults to T.
    --keep-target-intact        Try to keep init-time hardware affairs to
                                  the absolute bare minimum.
                                  Works best with --platform and --memory-config
                                  (or --no-memory-configuration) specified.
    --memory-detection-threshold  While detecting type of main memory
                                  use that much memory for I/O correctness
                                  testing.
    --list-contexts             After scanning interfaces, list contexts of 
                                  found target devices and quit.
    --context <context-id>      After scanning interfaces activate target device
                                  context denoted by context-id.
    --no-rc                     Do not load ~~/.comdbrc
    --load <filename>           Execute commands from file.
    --eval <lisp-expr>          Evaluate a Lisp expression
    --run-tests                 Run tests before doing anything.
                                  Abort on errors.
    --ignore-test-failures      Don't abort on test errors.
    --examine-tlb               Use TLB to resolve virtual addresses.
    --quit                      Terminate instead of continuing into debugger.
    --verbose                   During operation print out MORE STATUS.
                                  Implies --print-backtrace-on-errors.
    --version                   Print version and quit.
    --help                      Display this help message and quit.
    --help-ru                   Display a help message in russian and quit.")

(defvar *additional-help-ru* nil)
(defvar *additional-help-en* nil)

(defun display-invocation-help (&optional russianp)
  (when *orgify*
    (write-string "#+BEGIN_EXAMPLE") (terpri))
  (write-string (format nil (if russianp *comdb-help-ru* *comdb-help-en*)
                        (argv0-executable-name))) (terpri)
  (terpri)
  (when-let ((additional-help (if russianp *additional-help-ru* *additional-help-en*)))
    (write-string additional-help) (terpri)
    (terpri))
  (when *orgify*
    (write-string "#+END_EXAMPLE") (terpri)))

(defvar *standard-parameters* '((:load :string) (:core-multiplier :decimal) :early-eval :context :platform :memory-detection-threshold :eval
                                (:tapserver :string "127.0.0.1") (:rtlserver :string "127.0.0.1") (:tapserver-port :decimal) (:rtlserver-port :decimal)
                                (:trace-exchange :decimal 1024)
                                (:memory-config :string)))
(defvar *standard-switches*   '(:no-rc :virtual :physical :no-parport :no-usb :no-scan :no-platform-init
                                :list-contexts :list-platforms :help :help-ru :version
                                :print-memory-config :no-memory-configuration :no-memory-detection :memory-configuration-failure-error-p :keep-target-intact
                                :disable-debugger :print-backtrace-on-errors :early-break-on-signals :break-on-signals
                                :run-tests :ignore-test-failure :quit
                                :examine-tlb
                                ;; not documented
                                :orgify :log-pipeline-crit))
(defvar *panlevel-switches* '(:verbose)
  "Every COMDB-TOPLEVEL-WRAPPER-based application must accept this key.")

(defun comdb-toplevel-wrapper (fn &optional additional-parameters additional-switches &key
                               (help-needed-discriminator (constantly nil))
                               (user-package :comdb)
                               ;; default option customisation
                               no-rc no-platform-init disable-debugger print-backtrace-on-errors print-memory-config
                               no-memory-configuration no-memory-detection memory-detection-threshold verbose)
  (declare (optimize debug))
  (setf opfr:*opfr-repeat-on-return-key* t)
  (portability:set-and-activate-repl-fun
   (lambda ()
     (handler-bind (#+ccl
                    (serious-condition (lambda (c)
                                         "A nice reminder, for those implementations which don't have it."
                                         (syncformat t ">~%> To quit, type (QUIT).~%>~%")
                                         (error c))))
       (in-package :comdb)
       #+ccl ;; XXX-CCL: there' some kind of a glitch there...
       (when *globally-quitting*
         (quit))
       #+windows
       (interface-parport:bind-to-parport-access-library)     
       (with-quit-restart
         (destructuring-bind (&rest args &key (verbose verbose)
                                    (no-rc no-rc) early-eval
                                    core-multiplier tapserver rtlserver (tapserver-port (when tapserver 9001)) (rtlserver-port (when rtlserver 8090))
                                    virtual (physical (not (or virtual tapserver rtlserver))) no-parport no-usb trace-exchange
                                    no-scan (no-platform-init no-platform-init) keep-target-intact memory-config memory-configuration-failure-error-p
                                    load eval run-tests ignore-test-failures quit
                                    examine-tlb log-pipeline-crit
                                    list-contexts context list-platforms platform
                                    early-break-on-signals break-on-signals help help-ru orgify version
                                    ;; customisable
                                    (print-memory-config print-memory-config)
                                    (no-memory-configuration (unless run-tests no-memory-configuration))
                                    (no-memory-detection no-memory-detection)
                                    (memory-detection-threshold memory-detection-threshold)
                                    (disable-debugger disable-debugger)
                                    (print-backtrace-on-errors print-backtrace-on-errors)
                                    &allow-other-keys)
             (read-args-safely (rest (argv))
                               (append *standard-parameters* additional-parameters)
                               (append *panlevel-switches* *standard-switches* additional-switches))
           (when verbose
             (format t "~@<NOTE: ~@;arguments:~{ ~(~S~) ~S~}~:@>~%" args))
           (let* ((*break-on-signals* early-break-on-signals)
                  (other-args (apply #'remove-from-plist args (append (mapcar #'ensure-car *standard-parameters*) *standard-switches*)))
                  (*log-platform-processing* verbose)
                  (*log-system-configuration* verbose)
                  (*verbose-interface-init* verbose)
                  (*trace-exchange* trace-exchange)
                  (discrimination:*discriminate-verbosely* verbose)
                  (*orgify* orgify)
                  (*virtual-target-enabled* virtual)
                  (forced-platform (when platform
                                     (or (find-symbol (string-upcase (string platform)) :platform-definitions)
                                         (error "~@<Unknown platform \"~A\": use --list-platforms.~:@>" platform))))
                  (*examine-tlb* examine-tlb)
                  (*log-core-pipeline-crit* log-pipeline-crit)
                  (*print-base* #x10)
                  (help (or help help-ru (funcall help-needed-discriminator args))))
             (when disable-debugger
               #+sbcl
               (sb-ext:disable-debugger))
             ;;
             ;; Phase 0: non-core short-path functionality
             ;;
             (when-let ((quitp (cond (list-platforms     (list-platforms) t)
                                     (version            (version) t)
                                     ((or list-contexts) nil)
                                     (help               (display-invocation-help help-ru) t))))
               (quit))
             ;; We expect these arguments to be somewhat permanent, which is why er meddle with *INITARGS*,
             ;; instead of passing them to SCAN.
             (appendf (args)
                      (when no-parport                           `(:disable-parport-interfaces t))
                      (when no-usb                               `(:disable-usb-interfaces t))
                      (when keep-target-intact                   `(:keep-target-intact t))
                      (when forced-platform                      `(:forced-platform t))
                      (when memory-config                        `(:memory-config ,memory-config))
                      (when no-platform-init                     `(:skip-platform-init t))
                      (when no-memory-configuration              `(:inhibit-memory-configuration t))
                      (when no-memory-detection                  `(:inhibit-memory-detection t))
                      (when memory-detection-threshold           `(:memory-detection-threshold ,memory-detection-threshold))
                      (when print-backtrace-on-errors            `(:print-backtrace-on-errors t))
                      (when memory-configuration-failure-error-p `(:memory-configuration-failure-error-p t))
                      (when core-multiplier                      `(:core-multiplier ,core-multiplier)))
             ;;
             ;; Phase 1: inteface scanning
             ;;
             (when early-eval
               (eval early-eval))
             (unless no-scan
               (with-retry-restarts ((retry () :report "Retry scanning interface busses."))
                 (scan :physical physical :virtual virtual
                       :server-address (or tapserver rtlserver)
                       :server-port (or tapserver-port rtlserver-port)
                       :server-type (cond (tapserver :tap) (rtlserver :rtl)))
                 (if *current*
                     (when print-memory-config
                       (print-memconfig))
                     (error "~@<No devices were found attached to active busses.~:@>"))))
             ;;
             ;; Phase 1a: context querying
             ;;
             (cond (list-contexts (list-contexts)
                                  (quit)))
             ;;
             ;; Phase 1b: context selection
             ;;
             (when context
               (ctx context))
             (when *current*
               (format t "~&~@<; ~@;~:[No current device context.~;~
                                   Current platform/core: ~A ~A~]~:@>~%"
                       *current* (type-of (target-platform *target*)) *core*))
             ;;
             ;; Phase 2: in-context actions
             ;;
             (let ((*break-on-signals* break-on-signals))
               (setf *package* (find-package user-package))
               ;;
               ;; Phase 2a: run-control
               ;;
               (unless no-rc
                 (load (subfile* (user-homedir-pathname) ".comdbrc")
                       :verbose verbose :print verbose :if-does-not-exist nil))
               (let (successp)
                 ;;
                 ;; Phase 2b: batch, expression and test execution
                 ;;
                 (when load
                   (setf successp (load load)))
                 (when eval
                   (eval eval))
                 (when run-tests
                   (setf successp (run-tests))
                   (unless (or successp ignore-test-failures quit)
                     (error "~@<Some tests failed.~:@>")))
                 (when quit
                   (quit (if successp 0 1))))
               ;;
               ;; Phase 2: main functionality
               ;;
               (when verbose
                 (format t "NOTE: executing application functionality~%"))
               (apply fn other-args)
               (quit)))))))))

(defun opfr:opfr-prompt ()
  (let ((platform-type (when *target* (type-of (target-platform *target*))))
        (core-state (when *target* (state *core*)))
        (fetch (when *target* (moment-fetch (saved-core-moment *core*)))))
    (format t "~&~:[~;~:*~A ~A ~8,'0X~]> " platform-type core-state fetch))
  (force-output))

;;;;
;;;; Boring: completions.
;;;;
(defun write-package-readline-completions (package &optional qualify (stream *standard-output*))
  "Thanks to Edi Weitz. Location: http://weitz.de/completions.html"
  (let ((seen (make-hash-table :size 6000 :test #'equal))
        (package (find-package package)))
    (let ((prefixes (if qualify
                        (mapcar (lambda (prefix)
                                  (concatenate 'string (string-downcase prefix) ":"))
                                (or (package-nicknames package)
                                    (list (package-name package))))
                        (list "")))
          (external-symbols (sort (let (syms)
                                    (do-external-symbols (s package syms)
                                      (when (> (length (symbol-name s)) 2)
                                        (push s syms))))
                                  #'string<)))
      (dolist (symbol external-symbols)
        (dolist (prefix prefixes)
          (let* ((name (symbol-name symbol))
                 (completion (concatenate 'string prefix (string-downcase name))))
            (unless (or (gethash completion seen)
                        (string= "" name :end2 (min 4 (length name))))
              (setf (gethash completion seen) t)
              (princ completion stream)
              (terpri stream))))))))

(defparameter *tui-completable-packages* '(:common-lisp :alexandria :pergamum :executor :common-db :options :generic))

(defun write-completions (filename &optional (packages *tui-completable-packages*) qualified-packages)
  #+help-ru
  "Записать информацию для автодополнений символов из списка пакетов PACKAGES,
в формате GNU Readline в FILENAME.  Имена символов из пакетов в списке
QUALIFIED-PACKAGES дополняются префиксом из короткого имени пакета и
символа двоеточия."
  (with-output-to-file (s filename)
    (dolist (p packages)
      (write-package-readline-completions p nil s))
    (dolist (p qualified-packages)
      (write-package-readline-completions p t s))))

;;;;
;;;; No user-serviceable parts below.
;;;;
#+sbcl (in-package :sb-impl)
#+sbcl 
(defun toplevel-repl (noprint)
  (let ((* nil) (** nil) (*** nil)
        (- nil)
        (+ nil) (++ nil) (+++ nil)
        (/// nil) (// nil) (/ nil))
    (let ((repl-fun (funcall *repl-fun-generator*)))
      ;; Each REPL in a multithreaded world should have bindings of
      ;; most CL specials (most critically *PACKAGE*).
      (with-rebound-io-syntax
          (handler-bind ((step-condition 'invoke-stepper))
            (loop
               ;; CLHS recommends that there should always be an
               ;; ABORT restart; we have this one here, and one per
               ;; debugger level.
               (catch 'toplevel-catcher
                 ;; In the event of a control-stack-exhausted-error, we
                 ;; should have unwound enough stack by the time we get
                 ;; here that this is now possible.
                 #-win32
                 (sb-kernel::reset-control-stack-guard-page)
                 (funcall repl-fun noprint)
                 (critically-unreachable "after REPL"))))))))

#+sbcl (in-package :common-db)
#+sbcl
(progn
  (defmethod print-object ((o sb-sys:interactive-interrupt) stream)
    (format stream "~@<Ctrl+C pressed.~:@>")))

#+sbcl (in-package :sb-debug)
#+sbcl
(defun debugger-disabled-hook (condition me)
  (declare (ignore me))
  (flet ((failure-quit (&key recklessly-p)
           (sb-int:/show0 "in FAILURE-QUIT (in --disable-debugger debugger hook)")
           (sb-ext:quit :unix-status 1 :recklessly-p recklessly-p)))
    (handler-case
        (progn
          (format *error-output*
                  "~&~%~@<unhandled ~S: ~2I~_~A~:>~2%"
                  (type-of condition)
                  condition)
          (finish-output *error-output*)
          (when (options::arg :print-backtrace-on-errors)
            (sb-debug:backtrace 128 *error-output*)
            (finish-output *error-output*))
          (failure-quit))
      (condition ()
        (ignore-errors
          (sb-c::%primitive print
                            "Argh! error within --disable-debugger error handling"))
        (failure-quit :recklessly-p t)))))

(in-package :common-db)