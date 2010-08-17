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

(in-package :interface-tapclient)


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

(define-device-class tapclient-bus :empty (root-bus enumerating-bus interface-bus)
  ((address :reader   tapserver-address :initarg :address)
   (port    :reader   tapserver-port    :initarg :port)
   (stream  :accessor tapserver-stream)))

(defun sendpkt (stream type &rest args)
  (let ((pkt (list* type args))
        (*print-base* #x10))
    (print pkt stream)
    (finish-output stream)
    (when *trace-exchange*
      (format *trace-output* "<REQ ~S~%" pkt))))

(defun call-with-response (stream expectation fn)
  (with-safe-reader-context ()
    (let* ((*read-base* #x10)
           (response (read stream)))
      (when *trace-exchange*
        (format *trace-output* "> ~S~%" response))
      (when (eq :error (first response))
        (error "~@<Remote signalled error: ~@<~S~:@>~:@>" (second response)))
      (unless (eq expectation (first response))
        (error "~@<Expected response type ~S, got ~S.~:@>" expectation (first response)))
      (funcall fn response))))

(defmacro with-response (lambda-list stream &body body)
  (with-gensyms (response)
    `(call-with-response ,stream ,(first lambda-list)
                         (lambda (,response)
                           (destructuring-bind ,(rest lambda-list) (rest ,response)
                             ,@body)))))

(defmethod bus-occupied-addresses ((o tapclient-bus))
  (with-slots (address port stream) o
    (when *verbose-interface-init*
      (syncformat *trace-output* "; trying to connect to tapserver at ~A:~D~%" address port))
    ;; XXX: handle errors here
    (if (setf stream (ignore-errors (socket-stream (socket-connect address port))))
        (list (list address port))
        (syncformat *trace-output* "; failed to connect to tapserver at ~A:~D~%" address port))))

(defun (setf tapclient-tap-ird) (val iface null &aux
                                 (stream (tap-stream iface)))
  (declare (type (unsigned-byte 32) val) (ignore null))
  (sendpkt stream :set-ird val)
  (with-response (:ird value) stream
    value))

(defun tapclient-tap-idcode (iface null &aux
                             (stream (tap-stream iface)))
  (declare (ignore null))
  (sendpkt stream :idcode)
  (with-response (:idcode idcode) stream
    idcode))

(defun tapclient-tap-dr (iface reg &aux
                         (stream (tap-stream iface)))
  (sendpkt stream :get-dr reg)
  (with-response (:dr value) stream
    value))

(defun (setf tapclient-tap-dr) (val iface reg &aux
                                (stream (tap-stream iface)))
  (sendpkt stream :set-dr reg val)
  (with-response (:ok) stream)
  val)

(defmethod bus-populate-address ((o tapclient-bus) address)
  (lret ((iface (make-instance 'tapclient-interface :bus o :address address :stream (tapserver-stream o))))
    (setf (iface-idcode iface) (decode-bitfield :oncd-version (interface-reset iface)))))

(define-device-class tapclient-interface :interface (interface)
  ((stream :reader tap-stream :initarg :stream))
  (:layouts (:tap-ird nil (setf tapclient-tap-ird))             ;; wronly, not enforced
            (:tap-idcode tapclient-tap-idcode nil)              ;; XXX: this story is yet to be heard...
            (:tap-dr tapclient-tap-dr (setf tapclient-tap-dr))) ;; want pass
  (:extended-layouts :tap-dr))

(defmethod interface-reset ((o tapclient-interface) &aux
                            (stream (tap-stream o)))
  (with-condition-recourses interface-error
      (progn
        (sendpkt stream :reset-interface)
        (with-response (:idcode idcode) stream
          idcode))))

(defmethod interface-stop-target ((o tapclient-interface) &aux
                            (stream (tap-stream o)))
  (sendpkt stream :stop-target)
  (with-response (:ok) stream
    t))

(defmethod interface-attach-target ((o tapclient-interface) &aux
                                    (stream (tap-stream o)))
  (sendpkt stream :attach-target)
  (with-response (:ird ird) stream
    ird))

(defmethod interface-reset-target ((o tapclient-interface) stop-cores-p &aux
                                   (stream (tap-stream o)))
  (declare (ignore stop-cores-p))
  (sendpkt stream :reset-target)
  (with-response (:ok) stream)
  (interface-stop-target o)
  (interface-attach-target o))

(defmethod interface-bus-word ((o tapclient-interface) address &aux
                               (stream (tap-stream o)))
  "Read 32 bits from a given bus address."
  (declare (type (unsigned-byte 32) address))
  (sendpkt stream :read-word address)
  (with-response (:word value) stream
    value))

(defmethod (setf interface-bus-word) (val (o tapclient-interface) address &aux
                                      (stream (tap-stream o)))
  "Write 32 bits into a given bus address."
  (declare (type (unsigned-byte 32) val address))
  (sendpkt stream :write-word address val)
  (with-response (:ok) stream
    val))

(defmethod interface-bus-io ((o tapclient-interface) buffer address size direction &optional (offset 0) &aux
                             (stream (tap-stream o)))
  
  (multiple-value-call #'sendpkt stream (ecase direction
                                          (:read  (values :read-sequence  address size))
                                          (:write (values :write-sequence address (if (and (zerop offset)
                                                                                           (= size (length buffer)))
                                                                                      buffer
                                                                                      (subseq buffer offset (+ offset size)))))))
  (with-response (:data/ok &optional data) stream
    (when (eq direction :read)
      (setf (subseq buffer offset) data))))

(defmethod interface-close ((o tapclient-interface) &aux
                            (stream (tap-stream o)))
  (when (open-stream-p stream)
    (unwind-protect
         (progn (sendpkt stream :bye)
                (with-response (:bye) stream))
      (close (tap-stream o)))))

(defmethod bus-remove ((bus tapclient-bus) (o tapclient-interface))
  (interface-close o))
