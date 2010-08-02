;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOADABLE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :loadable)


(defvar *log-loadable-processing* nil)

(define-condition loadable-condition ()
  ((loadable :accessor condition-loadable :initarg :loadable)))

(define-condition loadable-error (loadable-condition) ())
(define-simple-error loadable-error)

(defclass loadable ()
  ((filename :accessor loadable-filename :initarg :filename)
   (entry-point :accessor loadable-entry-point :type (unsigned-byte 32) :initarg :entry-point)
   (sections :accessor loadable-sections :initform nil :type list :initarg :sections)
   (load-size :accessor loadable-load-size :type (unsigned-byte 32) :initarg :load-size)))

(defmethod print-object ((o loadable) stream)
  (format stream "~@<#<LOADABLE~; source: ~S,~:_ entry-point #x~8,'0X~_ load-size: #x~X~_ sections: ~S>~:@>" (loadable-filename o) (loadable-entry-point o) (loadable-load-size o) (loadable-sections o)))

(defun make-loadable (extents &key (filename "") (entry-point 0))
  "Given a list of EXTENTS, produce a loadable, with optionally specified
custom FILENAME and ENTRY-POINT."
  (let ((sections (sort extents #'< :key #'base)))
    (dolist (section sections)
      (unless (typep section 'section)
        (let ((best-type (etypecase section
                           (extent 'standard-section)
                           (baseless-extent 'simple-section))))
          (change-class section best-type :name "" :executable-p nil :file-offset 0))))
    (make-instance 'loadable :filename filename :entry-point entry-point :load-size (reduce #'+ sections :key #'size :initial-value 0) :sections sections)))

(defgeneric extract-loadable (type filename &key &allow-other-keys)
  (:method ((type (eql :elf)) filename &key &allow-other-keys)
    (let ((ehdr (with-condition-restart-binding ((displacement-out-of-range trim))
                  (parse 'ehdr (file-as-vector filename)))))
      (make-loadable (mapcar (rcurry #'ncoerce-extent '(simple-array (unsigned-byte 8) 1)) (ehdr-sections ehdr #'shdr-loadable-p))
                     :filename filename :entry-point (ehdr-entry (ehdr-body ehdr)))))
  (:method ((type (eql :raw)) filename &key (raw-section-name :raw-file-data) entry-point base)
    "Given a FILENAME, produce a loadable with BASE and ENTRY-POINT."
    (unless (and entry-point base)
      (error "ENTRY-POINT and BASE must be provided."))
    (make-loadable (list (make-instance 'elf:standard-section :name raw-section-name :executable-p t :file-offset 0
                                        :base base :data (file-as-vector filename)))
                   :filename filename :entry-point entry-point)))

(defun report-section (stream section)
  "Print an one-line summary of SECTION into STREAM."
  (format stream "~A~26,1T~8,'.X at ~8,'0X~%"
          (elf:section-name section) (size section) (base section)))

(defun report-checksummed-section (stream section)
  "Print an one-line summary of SECTION into STREAM."
  (format stream "~A~26,1T~8,'.X at ~8,'0X~:[~;, MD5: ~:*~{~2,'0X~}~]~%"
          (elf:section-name section) (size section) (base section)
          #-disable-ironclad (coerce (ironclad:digest-sequence :md5 (extent-data section)) 'list)
          #+disable-ironclad (error "~@<Checksumming disabled at build time.~:@>")))

(defun dump-section (stream section)
  "Print an one-line summary of SECTION and its dump into STREAM."
  (report-section stream section)
  (print-u8-sequence stream (extent-data section) :address (base section)))

(defun upload-loadable (bioable loadable &rest write-u8-extents-args &key section-before-fn address-remap-fn check &allow-other-keys)
  "Upload LOADABLE into BIOABLE, with section base addresses optionally 
remapped by ADDRESS-REMAP-FN, and optionally performed integrity checking,
which is governed by the CHECK keyword.
SECTION-BEFORE-FN, when non-NIL, is called before each section upload
with two arguments: the output stream and section."
  (let ((final-sections (xform address-remap-fn (curry #'nrebase address-remap-fn) (loadable-sections loadable))))
    (with-measured-time-lapse (seconds)
        (restart-case (apply #'write-u8-extents bioable (loadable-sections loadable) :before-fn section-before-fn
                             (remove-from-plist write-u8-extents-args :section-before-fn :address-remap-fn :check))
          (abort (&rest foo)
            :test (lambda (c) (typep c #+sbcl 'sb-sys:interactive-interrupt #-sbcl 'error))
            :report (lambda (stream) (format stream "~@<Abort upload ~A into ~A.~:@>" loadable bioable))
            (declare (ignore foo))
            (format *log-stream* "~%~@<Aborted upload of ~A into ~A due to user request.~:@>~2%" loadable bioable)
            (finish-output)
            (return-from upload-loadable)))
      (when *log-loadable-processing*
        (format *log-stream* "~D bytes moved in ~D seconds, effective I/O rate ~D kb/sec~%"
                (loadable-load-size loadable) seconds
                (coerce (round (loadable-load-size loadable) (* (max 0.1 seconds) 1024)) 'integer)))
      (when-let ((test-section (find-if (rcurry #'> #x100) final-sections :key #'size)))
        (let* ((evec (subseq (extent-data test-section) 0 #x100))
               (ivec (make-array #x100 :element-type '(unsigned-byte 8))))
          (read-block bioable (base test-section) ivec 0 #x100)
          (unless (equalp evec ivec)
            (print-u8-sequence-diff *log-stream* evec ivec :base (base test-section))
            (loadable-error "~@<Write spot-check failed at address #x~8,'0X.~:@>" (base test-section)))))
      (when check
        (when *log-loadable-processing*
          (format *log-stream* "Write check requested, performing...~%"))
        (when-let ((differences (iter (for original in final-sections)
                                      (for written in (u8-extents bioable final-sections))
                                      (unless (extent-data-equalp original written)
                                        (collect (cons original written))))))
          (iter (for (orig . writ) in differences)
                (format *log-stream* "Found corruptions during write of ~S:~%" orig)
                (print-u8-sequence-diff *log-stream* (extent-data orig) (extent-data writ) :base (base orig)))
          (loadable-error "~@<Write check failed.~:@>"))
        (when *log-loadable-processing*
          (format *log-stream* "Write check passed.~%")))
      t)))