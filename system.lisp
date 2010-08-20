;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MULTICORE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2009, ГУП НПЦ "Элвис"
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

(in-package :sysdev)


(defvar *manual-memory-config* nil)

(defmethod detect-platform-memory-size ((o platform) (base integer) &key (minimum #x8000) (maximum #x80000000) (when-infinite :error))
  (let ((target (platform-target o))
        (size minimum)
        (patzero #xdeadbeef)
        (pattail #x0facade0)
        (patspan #xcafebabe)
        (pattailspan #x0defaced))
    (and (let (test0 test1)
           (setf (memory-ref target base) patzero
                 test0 (memory-ref target base)
                 (memory-ref target base) pattail
                 test1 (memory-ref target base))
           (and (= patzero test0) (= pattail test1)))
         (iter (while (<= size maximum))
               (setf (memory-ref target (+ base #x0)) patzero
                     (memory-ref target (+ base size)) patspan
                     (memory-ref target (+ base size size -4)) pattailspan
                     (memory-ref target (+ base size -4)) pattail)
               (let ((zero (memory-ref target (+ base #x0)))
                     (tail (memory-ref target (+ base size -4)))
                     (span (memory-ref target (+ base size)))
                     (tailspan (memory-ref target (+ base size size -4))))
                 (cond ((and (= patzero zero) (= patspan span) (= pattail tail) (= pattailspan tailspan))
                        (setf size (ash size 1)))
                       (t
                        (return size))))
               (finally (case when-infinite
                          (:return-nil (return nil))
                          (:error (error "~@<Memory size couldn't be detected: seemingly infinite.~:@>"))))))))

(defmethod initialize-instance :after ((o platform) &key target predefined-devices &allow-other-keys)
  ;; only proceed if we're a real platform, not a prototype stub
  (when target
    (setf (target-platform target) o)
    ;; 1. spawn devices
    (mapc (curry #'apply #'create-target-device-from-spec target) predefined-devices)
    ;; 2. ensure essential devices and detect internal memory size, if present
    (let ((core (car (target-devices-by-type target 'general-purpose-core))))
      (unless core 
        (error 'platform-essentials-missing :platform o :target target
               :missing-device-types (unless core (list 'general-purpose-core))))
      (when-let* ((intmem (first (target-devices-by-type target 'internal-memory)))
                  (intmem-extent (memory-region-extent intmem)))
        (setf (cdr intmem-extent) (or (detect-platform-memory-size o (car intmem-extent))
                                      (error "~@<During initialisation of platform ~A: couldn't detect size of internal memory ~8,'0X.~:@>"
                                             (type-of o) (car intmem-extent))))))))

;;;;
;;;; Platform memory configuration
;;;;
(defstruct (memory-config (:constructor make-memory-config (name &rest register-values)))
  name
  register-values)

(defgeneric platform-memory-configurations (platform)
  (:documentation
   "The implementation must return an alist of MEMORY-CONFIGs keyed by name.")
  (:method :around ((o platform))
    (xform *manual-memory-config* (curry #'acons :manual *manual-memory-config*)
           (call-next-method))))

(defgeneric platform-memory-configuration-order (platform)
  (:documentation
   "The implementation must return a list of memory configuration names.")
  (:method :around ((o platform))
    (xform *manual-memory-config* (curry #'cons :manual)
           (call-next-method))))

(defun memory-config (platform name)
  (or (cdr (assoc name (platform-memory-configurations platform)))
      (platform-error "~@<Platform ~A doesn't have a memory config named ~A.~:@>" (type-of platform) name)))

(defgeneric parse-memory-config (form platform)
  (:documentation
   "The implementation must return a MEMORY-CONFIG object constructed
from the information provided in FORM.")
  (:method (f (p platform))
    (lret ((config (handler-case
                       (destructuring-bind (name &rest regspecs) f
                         (declare (ignore name))
                         (apply #'make-memory-config :manual
                                (iter (for (regname bitfields bitfield-values) in regspecs)
                                      (collect (list regname bitfields bitfield-values)))))
                     (error ()
                       (platform-error "~@<In PARSE-MEMORY-CONFIG: bad structure ~S, must be ~
                            (:name (:register-name (bitfield-name...) (bitfield-value...))...), ~
                            with T designating all-1s and NIL designating all-0s.~@:>"
                                       f)))))
      (unless (memory-config-valid-for-platform-p p config)
        (error "~@<In PARSE-MEMORY-CONFIG: memory config ~S is not valid for platform ~S.~:@>" config p)))))

(defgeneric serialise-memory-config (form platform)
  (:documentation
   "The implementation must serialise MEMORY-CONFIG, in such a way
that its PRINT form could be reconstructed by PARSE-MEMORY-CONFIG."))

(defun read-memory-config-file-for-platform (platform config-file)
  (with-safe-reader-context ()
    (with-input-from-file (s config-file)
      (let ((*read-base* #x10))
        (handler-case
            (values (parse-memory-config (read s nil nil) platform)
                    (lret ((size (destructuring-bind (&key size) (read s nil nil)
                                   size)))
                      (unless (or (null size)
                                  (and (integerp size) (plusp size)))
                        (platform-error "~@<Memory size, whenever specified in configuration file, must be a positive integer.~:@>"))))
          (error (c)
            (platform-error "~@<Error reading memory config ~A:~_~A~:@>" config-file c)))))))

(defun memory-config-valid-for-device-classes-p (config classes)
  "See if all of CONFIG's registers refer to the union of registers
defuned by device CLASSES."
  (every (compose (rcurry #'some classes)
                  (curry #'rcurry #'register-name-valid-for-device-class-p))
         (mapcar #'first (memory-config-register-values config))))

(defgeneric memory-config-valid-for-platform-p (platform memory-config)
  (:documentation
   "Every platform must have a method for this one, in order for
DETECT-AND-CONFIGURE-PLATFORM-MEMORY to work.
MEMORY-CONFIG-VALID-FOR-DEVICE-CLASSES-P could be used."))

(defgeneric apply-memory-config (platform config)
  (:method :before ((o platform) (c memory-config))
    (when *log-platform-processing*
      (format *log-stream* "~@<;;; ~@;Applying memory configuration ~A~:@>~%" (memory-config-name c)))))

(defun test-target-memory (target busaddr size &key (delay 0) (if-fails :print-delta))
  (let ((oarr (make-array size :element-type '(unsigned-byte 8)))
        (iarr (make-array size :element-type '(unsigned-byte 8))))
    (map-into oarr (the (function () (unsigned-byte 8)) (curry #'random 256)))
    (write-block target busaddr oarr)
    (sleep delay)
    (read-block target busaddr iarr)
    (or (equalp iarr oarr)
        (ecase if-fails
          (:continue)
          (:print-checksum
           #+disable-ironclad
           (error "~@<COMMON-DB was built without IRONCLAD: checksumming not supported.~:@>")
           #-disable-ironclad
           (format *log-stream* "Checksum mismatch: write ~A, read ~A~%"
                   (digest-as-string (ironclad:digest-sequence :sha1 oarr))
                   (digest-as-string (ironclad:digest-sequence :sha1 iarr))))
          (:print-delta
           (let ((errors (print-u8-sequence-diff *log-stream* oarr iarr :error-report-limit #x200)))
             (format *log-stream* "Total errors: ~2,'0D%, or ~D/~D of total space.~%~
                                  Error count limiting: total maximum of 512 byte errors were shown.~%"
                     (if (zerop errors) 0 (/ errors size 0.01)) errors size)))))))

(define-reported-condition platform-no-usable-memory-detected-error (platform-error)
  ((target :initarg :target))
  (:report (target) "~@<No memory could be assigned for bus address #x00000000 to ~S.~:@>" target))

(define-reported-condition platform-no-usable-memory-detected (platform-condition warning)
  ((target :initarg :target))
  (:report (target) "~@<No memory could be assigned for bus address #x00000000 to ~S.~:@>" target))

(defun detect-and-configure-platform-memory (platform inhibit-detection-p detection-threshold &optional (if-not-found :error))
  "Try every known memory configuration for PLATFORM, applying and returning
the first one which works as the primary value, and the size of the detected
memory extent as the secondary value.

   In the case no configuration was found, IF-NOT-FOUND specifies
the consequent behavior:
   :CONTINUE  - silently return nil;
   :WARN      - raise the PLATFORM-NO-USABLE-MEMORY-DETECTED condition, but
                  don't enter the debugger;
   :ERROR     - raise the PLATFORM-NO-USABLE-MEMORY-DETECTED condition, and
                  enter the debugger."
  (let* ((target (platform-target platform)))
    (let ((*log-stream* (if *log-platform-processing*
                            *log-stream*
                            (make-broadcast-stream))))
      (terpri *log-stream*)
      (pprint-logical-block (*log-stream* nil :per-line-prefix "NOTE: ")
        (format *log-stream* "trying memory configurations for bus address #x00000000:~%")
        (iter (for config in (remove-if-not (curry #'memory-config-valid-for-platform-p platform)
                                            (mapcar (curry #'memory-config platform)
                                                    (platform-memory-configuration-order platform))))
              (finish-output *log-stream*)
              (apply-memory-config platform config)
              (when inhibit-detection-p
                (return-from detect-and-configure-platform-memory config))
              (for size = (detect-platform-memory-size platform #x00000000))
              (when (and size (let ((*log-platform-processing* nil))
                                (test-target-memory target #x0 detection-threshold :if-fails :continue)))
                (pprint-newline :mandatory *log-stream*)
                (format *log-stream* "found ~DK of ~A-type memory at bus address #x00000000"
                        (ash size -10) (memory-config-name config))
                (terpri *log-stream*)
                (finish-output *log-stream*)
                (return-from detect-and-configure-platform-memory (values config size)))
              (when (eq :manual (memory-config-name config))
                (syncformat *terminal-io* "WARNING: manually provided memory configuration rejected!~%")))
        (terpri *log-stream*)
        (finish-output *log-stream*)
        (ecase if-not-found
          (:continue)
          (:warn (warn 'platform-no-usable-memory-detected :target target))
          (:error (error 'platform-no-usable-memory-detected-error :target target)))))))

(defmethod configure-platform-memory ((o platform) detection-mode config-file detection-threshold detection-failure-error-p)
  (declare (type (member :force :allow :inhibit) detection-mode)
           (type (or null string pathname stream) config-file)) ;; XXX: RTP: STREAM-DESIGNATOR
  (let ((target (platform-target o))
        (*log-stream* (if *log-platform-processing*
                          *log-stream*
                          (make-broadcast-stream)))
        (inhibit-detection (eq :inhibit detection-mode))
        (preexisting-config (platform-memory-configuration o))
        manually-forced-size)
    (when (and inhibit-detection
               (not preexisting-config)
               (not config-file))
      (platform-error "~@<There is no validated memory configuration for ~S, ~
                          no memory configuration file was provided, ~
                          and automatic memory detection was disabled. ~
                          Yet memory configuration wasn't explicitly disabled. ~
                          In these circumstances proceeding is impossible.~:@>"
                      o))
    (cond ((and preexisting-config
                (not inhibit-detection))
           (apply-memory-config o preexisting-config))
          (t
           (when config-file
             (multiple-value-bind (config size) (read-memory-config-file-for-platform o config-file)
               (setf *manual-memory-config* config
                     manually-forced-size (or size
                                              (when inhibit-detection
                                                (platform-error "~@<Memory config file must contain memory size, when automatic detection is disabled.~:@>"))))))
           (when inhibit-detection
             (syncformat *log-stream* "WARNING: memory detection disabled!~%"))
           (multiple-value-bind (config size) (detect-and-configure-platform-memory o inhibit-detection detection-threshold detection-failure-error-p)
             (make-target-device target 'ram
                                 :extent (extent 0 (if inhibit-detection
                                                       manually-forced-size
                                                       size))
                                 :master (target-device target '(general-purpose-core 0)))
             (setf (platform-memory-configuration o) config))))))
