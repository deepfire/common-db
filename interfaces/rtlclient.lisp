;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: INTERFACE; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2010, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2010 by
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

(in-package :interface-rtlclient)


(set-namespace :interface)

(defvar *trace-exchange* nil
  "Whether to report wire exchange to *STANDARD-OUTPUT*.")

(defconstant +oncd-ir-length+   4)
(defconstant +oncd-ird-length+  8)
(defconstant +idcode-length+    32)

;; Delay values, in nanoseconds.
(defconstant +reset-delay+ 1000000)
(defconstant +generic-delay+ 10000)
(defconstant +memory-delay+ 1000000)

(define-device-class client-bus :empty (root-bus enumerating-bus interface-bus)
  ((address :reader   server-address :initarg :address)
   (port    :reader   server-port    :initarg :port)
   (stream  :accessor server-stream)))

(defun sendpkt (stream type &rest args)
  (let ((pkt (list* type args))
        (*print-base* #x10))
    (print pkt stream)
    (finish-output stream)
    (when *trace-exchange*
      (format *trace-output* "<REQ ~S~%" pkt))))

(defun call-with-response (interface stream expectation eof-error-p fn)
  (with-safe-reader-context ()
    (let* ((*read-base* #x10)
           (response (read stream nil '(:eof))))
      (when *trace-exchange*
        (format *trace-output* "> ~S~%" response))
      (case (first response)
        (:eof   (when eof-error-p
                  (interface-error interface "~@<EOF on server stream ~S~:@>" stream)))
        (:error (error "~@<Remote signalled error: ~@<~S~:@>~:@>" (second response)))
        (t
         (unless (eq expectation (first response))
           (error "~@<Expected response type ~S, got ~S.~:@>" expectation (first response)))
         (funcall fn response))))))

(defmacro with-response (interface lambda-list stream eof-error-p &body body)
  (with-gensyms (response)
    `(call-with-response ,interface ,stream ,(first lambda-list) ,eof-error-p
                         (lambda (,response)
                           (destructuring-bind ,(rest lambda-list) (rest ,response)
                             ,@body)))))

(defun dotted-quad/port->integer (dotted-quad port)
  (declare (type string dotted-quad) (type (unsigned-byte 16) port))
  (logior (ash (u8-vector-word32le
                (map '(vector (unsigned-byte 8)) #'identity (usocket:dotted-quad-to-vector-quad dotted-quad))
                0)
               16)
          port))

(define-constant +rtlserver-banner-prefix-1+ "TCLAPI: connected addr=" :test 'equal)
(define-constant +rtlserver-banner-prefix-2+ "TCLAPI: protocol "       :test 'equal)

(defmethod bus-occupied-addresses ((o client-bus))
  (with-slots (address port stream) o
    (when *verbose-interface-init*
      (syncformat *trace-output* "; trying to connect to server at ~A:~D~%" address port))
    ;; XXX: handle errors here
    (cond ((setf stream (ignore-errors (socket-stream (socket-connect address port))))
           (when-let ((line1 (read-line stream nil nil))
                      (line2 (read-line stream nil nil)))
             (cond ((and (starts-with-subseq +rtlserver-banner-prefix-1+ line1)
                         (starts-with-subseq +rtlserver-banner-prefix-2+ line2))
                    (syncformat *trace-output* "; remote server protocol version: ~S~%" (subseq line2 (length +rtlserver-banner-prefix-2+)))
                    (list (dotted-quad/port->integer address port)))
                   (t
                    (syncformat *trace-output* "; unknown service at address ~A:~D~%" address port)))))
          (t
           (syncformat *trace-output* "; failed to connect to server at ~A:~D~%" address port)))))

(defun (setf client-tap-ird) (val iface null &aux
                              (stream (stream-of iface)))
  (declare (type (unsigned-byte 32) val) (ignore null))
  (sendpkt stream :set-ird val)
  (with-response iface (:ird value) stream t
    value))

(defun client-tap-idcode (iface null &aux
                          (stream (stream-of iface)))
  (declare (ignore null))
  (sendpkt stream :idcode)
  (with-response iface (:idcode idcode) stream t
    idcode))

(defun client-tap-dr (iface reg &aux
                      (stream (stream-of iface)))
  (sendpkt stream :get-dr reg)
  (with-response iface (:dr value) stream t
    value))

(defun (setf client-tap-dr) (val iface reg &aux
                             (stream (stream-of iface)))
  (sendpkt stream :set-dr reg val)
  (with-response iface (:ok) stream t)
  val)

(defmethod bus-populate-address ((o client-bus) address)
  (lret ((iface (make-instance 'client-interface :bus o :address address :stream (stream-of o))))
    (setf (iface-idcode iface) (decode-bitfield :oncd-version (interface-reset iface)))))

(define-device-class client-interface :interface (interface)
  ((stream :reader stream-of :initarg :stream))
  (:layouts (:tap-ird nil (setf client-tap-ird))             ;; wronly, not enforced
            (:tap-idcode client-tap-idcode nil)              ;; XXX: this story is yet to be heard...
            (:tap-dr client-tap-dr (setf client-tap-dr))) ;; want pass
  (:extended-layouts :tap-dr))

(defmethod interface-reset ((o client-interface) &aux
                            (stream (stream-of o)))
  (with-condition-recourses interface-error
      (progn
        (sendpkt stream :reset-interface)
        (with-response o (:idcode idcode) stream t
          idcode))))

(defmethod interface-stop-target ((o client-interface) &aux
                                  (stream (stream-of o)))
  (sendpkt stream :stop-target)
  (with-response o (:ok) stream t
    t))

(defmethod interface-attach-target ((o client-interface) &aux
                                    (stream (stream-of o)))
  (sendpkt stream :attach-target)
  (with-response o (:ird ird) stream t
    ird))

(defmethod interface-reset-target ((o client-interface) stop-cores-p &aux
                                   (stream (stream-of o)))
  (declare (ignore stop-cores-p))
  (sendpkt stream :reset-target)
  (with-response o (:ok) stream t)
  (interface-stop-target o)
  (interface-attach-target o))

(defmethod interface-bus-word ((o client-interface) address &aux
                               (stream (stream-of o)))
  "Read 32 bits from a given bus address."
  (declare (type (unsigned-byte 32) address))
  (sendpkt stream :read-word address)
  (with-response o (:word value) stream t
    value))

(defmethod (setf interface-bus-word) (val (o client-interface) address &aux
                                      (stream (stream-of o)))
  "Write 32 bits into a given bus address."
  (declare (type (unsigned-byte 32) val address))
  (sendpkt stream :write-word address val)
  (with-response o (:ok) stream t
    val))

(defmethod interface-bus-io ((o client-interface) buffer address size direction &optional (offset 0) &aux
                             (stream (stream-of o)))
  
  (multiple-value-call #'sendpkt stream (ecase direction
                                          (:read  (values :read-sequence  address size))
                                          (:write (values :write-sequence address (if (and (zerop offset)
                                                                                           (= size (length buffer)))
                                                                                      buffer
                                                                                      (subseq buffer offset (+ offset size)))))))
  (with-response o (:data/ok &optional data) stream t
    (when (eq direction :read)
      (setf (subseq buffer offset) data))))

(defmethod interface-close ((o client-interface) &aux
                            (stream (stream-of o)))
  (when (open-stream-p stream)
    (unwind-protect
         (progn (ignore-errors (sendpkt stream :bye))
                (with-response o (:bye) nil stream))
      (close (stream-of o)))))

(defmethod bus-remove ((bus client-bus) (o client-interface))
  (interface-close o))
