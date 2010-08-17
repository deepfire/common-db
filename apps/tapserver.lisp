;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TAPSERVER; Base: 10; indent-tabs-mode: nil -*-
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

(defpackage #:tapserver
  (:use :common-lisp :cffi :usocket :pergamum :iterate :setc :bitmop :device-model :isa :assem
        :portability :options :spaces :generic :interface :common-db)
  (:shadowing-import-from :isa #:disassemble)
  (:shadowing-import-from :bitmop #:space #:*space*)
  (:shadowing-import-from :common-db #:catch #:step #:get #:set #:trace)
  (:export
   #:tap-server
   #:serve
   #:tapserver-toplevel
   #:*trace-exchange*
   #:*log-stream*))

(in-package :tapserver)


(defvar *trace-exchange* nil
  "Whether to report wire exchange to *STANDARD-OUTPUT*.")

(defvar *log-stream* *trace-output*)

(defclass tap-server (target-context)
  ((stream :reader stream-of)))

(defun sendpkt (stream type &rest args)
  (print (list* type args) stream)
  (finish-output stream)
  (when *trace-exchange*
    (format *log-stream* "<reply ~S~%" (list* type args))
    (force-output *log-stream*)))

(defun log-request (control-string &rest args)
  (apply #'format *log-stream* (concatenate 'string "~&TAPCLIENT> " control-string "~%") args)
  (force-output *log-stream*))

(defun call-ensuring (stream check error-message fn)
  (if check
      (funcall fn)
      (sendpkt stream :error error-message)))

(defmacro with-check ((check stream) error-message &body body)
  `(call-ensuring ,stream ,check ,error-message (lambda () ,@body)))

(defun call-ensuring-nonnegative-arg (stream arg fn)
  (if (and (integerp arg) (not (minusp arg)))
      (funcall fn)
      (sendpkt stream :error "Single mandatory argument not a non-negative integer.")))

(defmacro with-nonnegative-arg ((arg stream) &body body)
  `(call-ensuring-nonnegative-arg ,stream ,arg (lambda () ,@body)))

(defun call-ensuring-two-nonnegative-args (stream arg1 arg2 fn)
  (if (and (integerp arg1) (not (minusp arg1))
           (integerp arg2) (not (minusp arg2)))
      (funcall fn)
      (sendpkt stream :error "The two mandatory arguments are not both non-negative integers.")))

(defmacro with-two-nonnegative-args ((arg1 arg2 stream) &body body)
  `(call-ensuring-two-nonnegative-args ,stream ,arg1 ,arg2 (lambda () ,@body)))


(defun do-the-dance (server &aux
                     (stream (stream-of server))
                     (iface (ctx-interface server)))
  (with-safe-reader-context ()
    (let ((*print-base* #x10)
          (*read-base*  #x10))
      (iter (for request = (read stream nil (list :eof)))
            (when *trace-exchange*
              (log-request "~S" request))
            (if (atom request)
                (sendpkt stream :error (format nil "Bad request ~S: must be a list." request))
                (destructuring-bind (op &optional first second) request
                  (ecase op
                    (:set-ird
                     (with-nonnegative-arg (first stream)
                       (sendpkt stream :ird (setc (devreg iface :ird) first))))
                    (:get-dr
                     (with-nonnegative-arg (first stream)
                       (sendpkt stream :dr (reginstance-value (device-layout-register-instance-by-selector
                                                               iface (layout (space :interface) :tap-dr) first)))))
                    (:set-dr
                     (with-two-nonnegative-args (first second stream)
                       (set-reginstance-value (device-layout-register-instance-by-selector
                                               iface (layout (space :interface) :tap-dr) first)
                                              second)
                       (sendpkt stream :ok)))
                    (:reset-interface
                     (sendpkt stream :idcode (interface-reset iface)))
                    (:stop-target
                     (interface-stop-target iface)
                     (sendpkt stream :ok))
                    (:attach-target
                     (sendpkt stream :ird (interface-attach-target iface)))
                    (:reset-target
                     (interface-reset-target iface first)
                     (sendpkt stream :ok))
                    (:read-word
                     (with-nonnegative-arg (first stream)
                       (sendpkt stream :word (interface-bus-word iface first))))
                    (:write-word
                     (with-two-nonnegative-args (first second stream)
                       (setf (interface-bus-word iface first) second)
                       (sendpkt stream :ok)))
                    (:read-sequence
                     (with-two-nonnegative-args (first second stream)
                       (let ((iovec (make-array second :element-type '(unsigned-byte 8))))
                         (interface-bus-io iface iovec first second :read)
                         (sendpkt stream :data/ok iovec))))
                    (:write-sequence
                     (with-nonnegative-arg (first stream)
                       (with-check ((vectorp second) stream) "Second mandatory argument not a vector."
                         (let ((iovec (make-array (length second) :element-type '(unsigned-byte 8))))
                           (map-into iovec #'identity second)
                           (interface-bus-io iface iovec first (length iovec) :write)
                           (sendpkt stream :data/ok)))))
                    (:bye
                     (sendpkt stream :bye)
                     (return))
                    (:eof
                     (when *trace-exchange*
                       (format *log-stream* "; EOF within client stream, tearing down connection~%")
                       (force-output *log-stream*))
                     (return)))))))))

(defgeneric accept-client-connection (server port &optional listen-address)
  (:documentation "Wait for a single connection to `server' on `port'")
  (:method ((server tap-server) port &optional (listen-address "127.0.0.1"))
    (let ((server-socket (socket-listen listen-address port :reuse-address t :backlog 1)))
      (unwind-protect (socket-accept server-socket)
        (socket-close server-socket)))))

(defgeneric serve-client-connection (server connection &optional trace-exchange)
  (:method ((server tap-server) client &optional (trace-exchange *trace-exchange*))
    (let ((*trace-exchange* trace-exchange)
          peer-address)
      (unwind-protect
           (let ((stream (socket-stream client)))
             (setf peer-address (get-peer-address client))
             (handler-case (unwind-protect 
                                (progn
                                  (setf (slot-value server 'stream) stream)
                                  (format *trace-output* "~@<; ~@;Connection with ~A established.~:@>~%" peer-address)
                                  (do-the-dance server)
                                  (format *trace-output* "~@<; ~@;Connection with ~A terminated normally.~:@>~%" peer-address))
                             (close stream))
               #+(or sbcl ccl)
               (#+sbcl sb-int:simple-stream-error
                 #+ccl ccl:socket-error
                 (c)
                 ;; Deal with -EPIPE
                 (format *error-output* "~@<; ~@;ERROR: ~A~:@>~%" c))))))))

(defvar *tapserver-help-en*
  "  Tapserver options:
    --address <dotted-quad>     Address of the interface to accept connections.
                                  Defaults to 127.0.0.1.
    --port <integer>            Number of the TCP port to accept connections on.
                                  Defaults to 9001.
    --trace-exchange            Enable wire protocol exchange reporting.
    --single-shot               Exit after the first connection terminates.")

(defun tapserver-toplevel ()
  (setf common-db::*additional-help-en* *tapserver-help-en*)
  (comdb::comdb-toplevel-wrapper #'serve
                                 '((:address :string) (:port :decimal))
                                 '(:trace-exchange :single-shot)))

(defun serve (&key verbose (address "127.0.0.1") (port 9001)
              trace-exchange single-shot &aux
              (ctx (or *current*
                       (error "~@<No active target context: cannot proceed with tapserver!~:@>")))
              (*trace-exchange* trace-exchange))
  (declare (ignore verbose))
  (change-class ctx 'tap-server)
  (iter (syncformat t "; Accepting connections on ~A:~D~:[~;, tracing exchanges~]~%" address port trace-exchange)
        (for client = (accept-client-connection ctx port address))
        (serve-client-connection ctx client trace-exchange)
        (until single-shot)))