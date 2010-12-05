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
   (type    :reader   server-type    :initarg :type)
   (stream  :accessor server-stream)))

(defun sendpkt (stream type &rest args)
  (let ((pkt (list* type args))
        (*print-base* #x10)
        (sexp (keywordp type)))
    (if sexp
        (print pkt stream)
        (iter (for (elt . rest) on pkt)
              (when (integerp elt)
                (write-string "0x" stream))
              (write elt :stream stream)
              (if rest
                  (write-char #\Space stream)
                  (terpri stream))))
    (finish-output stream)
    (when *trace-exchange*
      (format *trace-output* "<REQ ~S~%" pkt))))

(defun call-with-response (interface stream expectation eof-error-p fn)
  (with-safe-reader-context ()
    (let* ((*read-base* #x10)
           (sexp (keywordp expectation))
           (response (if sexp
                         (read stream nil '(:eof))
                         (string-right-trim '(#\Return) (read-line stream nil nil)))))
      (when *trace-exchange*
        (format *trace-output* "> ~S~%" response))
      (flet ((handle-eof ()
               (when eof-error-p
                 (interface-error interface "~@<EOF on server stream ~S~:@>" stream)))
             (handle-unexpected-response (response)
               (error "~@<Expected response type ~S, got ~S.~:@>" expectation response)))
        (if sexp
            (case (first response)
              (:eof   (handle-eof))
              (:error (error "~@<Remote signalled error: ~@<~S~:@>~:@>" (second response)))
              (t
               (unless (eq expectation (first response))
                 (handle-unexpected-response (first response)))
               (funcall fn response)))
            (if response
                (multiple-value-bind (okayp suffix) (starts-with-subseq "ok" response :return-suffix t)
                  (if okayp
                      (funcall fn (list* nil (when (> (length suffix) 2)
                                               (list (parse-integer suffix :start (if (char= #\x (char suffix 2))
                                                                                      3
                                                                                      1)
                                                                    :junk-allowed t)))))
                      (handle-unexpected-response (subseq response (or (position #\Space response) 0)))))
                (handle-eof)))))))

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
           (when-let ((line1 (string-right-trim '(#\Return) (read-line stream nil nil)))
                      (line2 (string-right-trim '(#\Return) (read-line stream nil nil))))
             (cond ((and (starts-with-subseq +rtlserver-banner-prefix-1+ line1)
                         (starts-with-subseq +rtlserver-banner-prefix-2+ line2))
                    (syncformat *trace-output* "; remote server protocol version: ~S~%" (subseq line2 (length +rtlserver-banner-prefix-2+)))
                    (list (dotted-quad/port->integer address port)))
                   (t
                    (syncformat *trace-output* "; unknown service at address ~A:~D~%" address port)))))
          (t
           (syncformat *trace-output* "; failed to connect to server at ~A:~D~%" address port)))))

(defun id2reg (reg)
 (ecase reg
   (#x0 "OSCR")
   (#x1 "OMBC")
   (#x2 "OLR0")
   (#x3 "OLR1")
   (#x4 "OBCR")
   (#x5 "IRdec")
   (#x6 "OTC")
   (#x7 "PCdec")
   (#x8 "PCexec")
   (#x9 "PCmem")
   (#xa "PCfetch")
   (#xb "OMAR")
   (#xc "OMDR")
   (#xd "EnMEM")
   (#xe "PCwb")
   (#xf "EnGO")
   (#x100 "EnREGF")))

(defun (setf %client-tap-ird) (val interface null)
  (declare (type (integer 0) val) (type interface interface) (ignore null))
  (setf (client-tap-ird interface nil) val))

(defun (setf %client-tap-dr) (val interface reg)
  (declare (type (integer 0) val) (type interface interface))
  (setf (client-tap-dr interface reg) val))

(defmethod bus-populate-address ((o client-bus) address)
  (lret ((iface (make-instance (ecase (server-type o)
                                 (:rtl 'rtl-client-interface)
                                 (:tap 'tap-client-interface))
                               :bus o :address address :stream (server-stream o))))
    (setf (iface-idcode iface) (decode-bitfield :oncd-version (interface-reset iface)))))

(define-device-class client-interface :interface (elvees-interface)
  ((stream :reader stream-of :initarg :stream))
  (:layouts (:tap-ird nil (setf %client-tap-ird))            ;; wronly, not enforced
            (:tap-idcode client-tap-idcode nil)              ;; XXX: this story is yet to be heard...
            (:tap-dr client-tap-dr (setf %client-tap-dr)))   ;; want pass
  (:extended-layouts :tap-dr))

(define-device-class rtl-client-interface :interface (client-interface)
  ()
  (:extended-layouts :tap-dr))

(define-device-class tap-client-interface :interface (client-interface)
  ()
  (:extended-layouts :tap-dr))

(defgeneric (setf client-tap-ird) (val iface null)
  (:method (val (iface tap-client-interface) null &aux (stream (stream-of iface)))
    (sendpkt stream :set-ird val)
    (with-response iface (:ird value) stream t
      value))
  (:method (val (iface rtl-client-interface) null &aux (stream (stream-of iface)))
    (sendpkt stream "wrDR" val)
    (with-response iface ("ok" value) stream t
      value)))

(defgeneric client-tap-idcode (iface null)
  (:method ((iface tap-client-interface) null &aux (stream (stream-of iface)))
    (sendpkt stream :idcode)
    (with-response iface (:idcode idcode) stream t
      idcode))
  (:method ((iface rtl-client-interface) null &aux (stream (stream-of iface)))
    (sendpkt stream "reset")
    (with-response iface ("ok") stream t)
    (sendpkt stream "rdIR")
    (with-response iface ("ok" idcode) stream t
      idcode)))

(defmethod tap-write-dr-register ((o rtl-client-interface) selector val read-only-p)
  (declare (fixnum selector) (type (unsigned-byte 32) val) (boolean read-only-p))
  (with-writing-oncd-register-value (irdval reglen) (o selector read-only-p)
    (sendpkt-stream :set-dr reg (logior irdval (ash val 8)))
    (with-response iface ("ok" ret) stream t
      ret)))

(defgeneric client-tap-dr (iface reg)
  (:method ((iface tap-client-interface) reg &aux (stream (stream-of iface)))
    (sendpkt stream :get-dr reg)
    (with-response iface (:dr value) stream t
      value))
  (:method ((iface rtl-client-interface) reg &aux (stream (stream-of iface)))
    (tap-write-dr-register o reg 0 t)))

(defgeneric (setf client-tap-dr) (val iface reg)
  (:method (val (o tap-client-interface) reg &aux (stream (stream-of o)))
    (sendpkt stream :set-dr reg val)
    (with-response o (:ok) stream t)
    val)
  (:method (val (o rtl-client-interface) reg &aux (stream (stream-of o)))
    (tap-write-dr-register o reg val nil)))

(defmethod interface-reset ((o tap-client-interface) &aux (stream (stream-of o)))
  (with-condition-recourses interface-error
      (progn
        (sendpkt stream :reset-interface)
        (with-response o (:idcode idcode) stream t
          idcode))))

(defmethod interface-reset ((o client-interface) &aux (stream (stream-of o)))
  (with-condition-recourses interface-error
      (progn
        (sendpkt stream "reset")
        (with-response o ("ok") stream t)
        (sendpkt stream "rdIR")
        (with-response o ("ok" value) stream t
          value))))

(defmethod interface-stop-target ((o tap-client-interface) &aux (stream (stream-of o)))
  (sendpkt stream :stop-target)
  (with-response o (:ok) stream t
    t))

(defmethod interface-stop-target ((o rtl-client-interface) &aux (stream (stream-of o)))
  (sendpkt stream "wrIR" (bits :tap-opcode :debug-request))
  (with-response o ("ok" ir) stream t
    (declare (ignore ir))
    t))

(defmethod interface-attach-target ((o tap-client-interface) &aux (stream (stream-of o)))
  (sendpkt stream :attach-target)
  (with-response o (:ird ird) stream t
    ird))

(defmethod interface-attach-target ((o rtl-client-interface) &aux (stream (stream-of o)))
  (iter (sendpkt stream "wrIR" (bits :tap-opcode :debug-enable))
        (until (with-response o ("ok" ird) stream t
                 (= ird (bits :tap-opcode :debug-enable))))))

(defmethod interface-reset-target ((o tap-client-interface) stop-cores-p &aux (stream (stream-of o)))
  (declare (ignore stop-cores-p))
  (sendpkt stream :reset-target)
  (with-response o (:ok) stream t)
  (interface-stop-target o)
  (interface-attach-target o))

(defmethod interface-reset-target ((o rtl-client-interface) stop-cores-p &aux (stream (stream-of o)))
  (declare (ignore stop-cores-p))
  (sendpkt stream "reset")
  (with-response o ("ok") stream t)
  (interface-stop-target o)
  (interface-attach-target o))

(defmethod interface-bus-word ((o tap-client-interface) address &aux (stream (stream-of o)))
  (declare (type (unsigned-byte 32) address))
  (sendpkt stream :read-word address)
  (with-response o (:word value) stream t
    value))

(defmethod (setf interface-bus-word) (val (o tap-client-interface) address &aux (stream (stream-of o)))
  (declare (type (unsigned-byte 32) val address))
  (sendpkt stream :write-word address val)
  (with-response o (:ok) stream t
    val))

(defmethod interface-bus-io ((o tap-client-interface) buffer address size direction &optional (offset 0) &aux (stream (stream-of o)))
  
  (multiple-value-call #'sendpkt stream (ecase direction
                                          (:read  (values :read-sequence  address size))
                                          (:write (values :write-sequence address (if (and (zerop offset)
                                                                                           (= size (length buffer)))
                                                                                      buffer
                                                                                      (subseq buffer offset (+ offset size)))))))
  (with-response o (:data/ok &optional data) stream t
    (when (eq direction :read)
      (setf (subseq buffer offset) data))))

#+nil ;; let the ELVEES-INTERFACE method kick in
(defmethod interface-bus-io ((o rtl-client-interface) buffer address size direction &optional (offset 0) &aux (stream (stream-of o)))
  (multiple-value-call #'sendpkt stream (ecase direction
                                          (:read  (values :read-sequence  address size))
                                          (:write (values :write-sequence address (if (and (zerop offset)
                                                                                           (= size (length buffer)))
                                                                                      buffer
                                                                                      (subseq buffer offset (+ offset size)))))))
  (with-response o (:data/ok &optional data) stream t
    (when (eq direction :read)
      (setf (subseq buffer offset) data))))

(defmethod interface-close ((o tap-client-interface) &aux (stream (stream-of o)))
  (when (open-stream-p stream)
    (unwind-protect
         (progn (ignore-errors (sendpkt stream :bye))
                (with-response o (:bye) nil stream))
      (close (stream-of o)))))

(defmethod interface-close ((o rtl-client-interface) &aux (stream (stream-of o)))
  (when (open-stream-p stream)
    (close (stream-of o))))

(defmethod bus-remove ((bus client-bus) (o client-interface))
  (interface-close o))
