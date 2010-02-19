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


(defparameter *memory-detection-threshold* #x1000)


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
   "The implementation must return an alist of MEMORY-CONFIGs keyed by name."))
(defgeneric platform-memory-configuration-order (platform)
  (:documentation
   "The implementation must return a list of memory configuration names."))

(defun memory-config (platform name)
  (or (cdr (assoc name (platform-memory-configurations platform)))
      (platform-error "~@<Platform ~A doesn't have a memory config named ~A.~:@>" (type-of platform) name)))

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

(defun test-target-memory (target busaddr size &key (if-fails :print-delta))
  (let ((oarr (make-array size :element-type '(unsigned-byte 8)))
        (iarr (make-array size :element-type '(unsigned-byte 8))))
    (map-into oarr (the (function () (unsigned-byte 8)) (curry #'random 256)))
    (write-block target busaddr oarr)
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

(defun detect-and-configure-platform-memory (platform &optional (if-not-found :error))
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
        (multiple-value-bind (config size)
            (iter (for config-name in (platform-memory-configuration-order platform))
                  (for config = (memory-config platform config-name))
                  (unless (memory-config-valid-for-platform-p platform config)
                    (next-iteration))
                  (finish-output *log-stream*)
                  (apply-memory-config platform config)
                  (for size = (detect-platform-memory-size platform #x00000000))
                  (when (and size (let ((*log-platform-processing* nil))
                                    (test-target-memory target #x0 *memory-detection-threshold* :if-fails :continue)))
                    (pprint-newline :mandatory *log-stream*)
                    (format *log-stream* "found ~DK of ~A-type memory at bus address #x00000000"
                            (ash size -10) (memory-config-name config))
                    (terpri *log-stream*)
                    (finish-output *log-stream*)
                    (return-from detect-and-configure-platform-memory (values config size))))
          (declare (ignore size))
          (terpri *log-stream*)
          (finish-output *log-stream*)
          (unless config
            (ecase if-not-found
              (:continue)
              (:warn (warn 'platform-no-usable-memory-detected :target target))
              (:error (error 'platform-no-usable-memory-detected-error :target target)))))))))

(defmethod configure-platform-memory ((o platform) &optional force-detection (if-detection-fails :error))
  (let ((target (platform-target o))
        (*log-stream* (if *log-platform-processing*
                          *log-stream*
                          (make-broadcast-stream))))
    (if-let ((configuration (platform-memory-configuration o))
             (no-forced-detection (null force-detection)))
      (apply-memory-config o configuration)
      (multiple-value-bind (config size) (detect-and-configure-platform-memory o if-detection-fails)
        (make-target-device target 'ram :extent (extent 0 size) :master (target-device target '(general-purpose-core 0)))
        (setf (platform-memory-configuration o) config)))))
