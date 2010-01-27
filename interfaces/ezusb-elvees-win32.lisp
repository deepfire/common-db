;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: INTERFACE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :interface-ezusb-elvees)


(set-namespace :interface)

#+sbcl
(cffi:define-foreign-library kernel32
  (t "kernel32.dll"))

#+sbcl
(cffi:use-foreign-library kernel32)

(cffi:defcfun ("GetLastError" get-last-error) :int)

(cffi:defcfun ("DeviceIoControl" device-io-control) :int
  (handle :uint) (ioctl :uint)
  (cmdaddr :pointer) (cmdsize :uint) (dataaddr :pointer) (datasize :uint) (retptr :pointer) (ignore :uint))

(cffi:defcfun ("CreateFileA" create-file) :int
  (name :string) (access :int) (share-mode :int) (sec-attr :pointer) (disposition :int) (flags :int) (template :pointer))

(cffi:defcfun ("CloseHandle" close-handle) :boolean
  (handle :int))

(define-reported-condition win32-error (error)
  ((function-name :accessor function-name :initarg :function-name)
   (args :accessor args :initarg :args)
   (return-value :accessor return-value :initarg :return-value)
   (secondary-return-value :accessor secondary-return-value :initarg :secondary-return-value)
   (error-code :accessor error-code :initarg :error-code)
   (decoded :accessor decoded :initarg :decoded)
   (addendum :accessor addendum :initarg :addendum))
  (:report (function-name args return-value secondary-return-value error-code decoded addendum)
           "~@<Win32 error while calling ~A with parameters ~S: return value ~D/~D, error code ~D, decoded: ~A.~A~:@>" function-name args return-value secondary-return-value error-code (or decoded "Unknown.") addendum)
  (:default-initargs
   :secondary-return-value nil
    :addendum ""))

(define-reported-condition ioctl-error (win32-error interface-error)
  ((handle :accessor handle :initarg :handle)
   (ioctl :accessor ioctl :initarg :ioctl)
   (ioctl-status :accessor ioctl-status :initarg :ioctl-status)
   (ioctl-error-code :accessor ioctl-error-code :initarg :ioctl-error-code))
  (:report (ioctl handle ioctl-status ioctl-error-code error-code decoded return-value secondary-return-value addendum)
           "~@<Ioctl #x~X on handle ~D returned error ~D, with status ~S. Windows error code: ~D/~S, routine/ioctl return values: ~D/~D.~A~:@>" ioctl handle ioctl-error-code ioctl-status error-code decoded return-value secondary-return-value addendum))

(define-reported-condition decoded-ioctl-error (ioctl-error)
  ((ioctl-decoded-status :accessor ioctl-decoded-status :initarg :ioctl-decoded-status)
   (ioctl-decoded-error :accessor ioctl-decoded-error :initarg :ioctl-decoded-error))
  (:report (ioctl handle ioctl-status ioctl-decoded-status ioctl-error-code ioctl-decoded-error addendum)
           "~@<Ioctl #x~X on handle ~D returned status: ~D/~S, error: ~D/~S.~A~:@>" ioctl handle ioctl-status ioctl-decoded-status ioctl-error-code ioctl-decoded-error addendum))

(eval-when (:compile-toplevel :load-toplevel)
  (defun ctl-code-any-buffered (fn)
    (dpb (+ fn #x800) (byte 12 2) #x220000))
  (defun ctl-code-any-in-direct (fn)
    (dpb (+ fn #x800) (byte 12 2) #x220001))
  (defun ctl-code-any-out-direct (fn)
    (dpb (+ fn #x800) (byte 12 2) #x220002)))

(defconstant +ezusb-ioctl-reset+ (ctl-code-any-in-direct 12))
(defconstant +ezusb-ioctl-bulk-read+ (ctl-code-any-out-direct 19))
(defconstant +ezusb-ioctl-bulk-write+ (ctl-code-any-in-direct 20))
(defconstant +ezusb-ioctl-get-frame-number+ (ctl-code-any-buffered 21))
(defconstant +ezusb-ioctl-get-last-error+ (ctl-code-any-buffered 23))

(defun bus-address-filename (addr)
  (concatenate 'string "\\\\.\\EZUSB-" (write-to-string addr)))

(defun ezusb-do-open (addr &aux (name (bus-address-filename addr)))
  (let ((ret (create-file name
                          #x40000000    ; GENERIC_WRITE
                          #x2           ; FILE_SHARE_WRITE
                          (cffi:null-pointer)
                          #x3           ; OPEN_EXISTING
                          #x0
                          (cffi:null-pointer))))
    (when (plusp ret)
      ret)))

(defun ezusb-open (addr)
  (lret ((name (bus-address-filename addr))
         (ret (ezusb-do-open addr)))
    (syncformat t "NOTE: opening an EZUSB device on ~A~%" name)
    (unless ret
      (let ((ler (get-last-error)))
        (error 'win32-error :function-name 'create-file :args (list name)
               :return-value ret :secondary-return-value nil :addendum ""
               :error-code ler :decoded (case ler
                                          (2 "The system cannot find the file specified")))))))

(defun ezusb-ioctl (handle ioctl inptr insize outptr outsize outvec-offset &optional override)
  (declare (special *in-nested-ezusb-ioctl*))
  (cffi:with-foreign-objects ((retptr :int))
    (let ((ret (device-io-control handle ioctl inptr insize(cffi:inc-pointer outptr outvec-offset) outsize
                                  retptr (cffi:pointer-address (cffi:null-pointer)))))
      (unless (plusp ret)
        (let ((ler (get-last-error)))
          (when override
            (error "~&Error during EZUSB-IOCTL #x~X: GetLastError returned ~D~%" ioctl ler))
          (when (boundp '*in-nested-ezusb-ioctl*)
            (error "~@<Nested error in ~A: windows error code ~D, ioctl return value ~D.~:@>"
                   *in-nested-ezusb-ioctl* ler (cffi:mem-ref retptr :int)))
          (let* ((usbd-ioctl-error (ezusb-get-last-usbd-error handle))
                 (usbd-ioctl-error-code (ldb (byte 30 0) usbd-ioctl-error))
                 (usbd-ioctl-error-status (ldb (byte 2 30) usbd-ioctl-error)))
            (error 'decoded-ioctl-error :ioctl ioctl :handle handle
                   :ioctl-status usbd-ioctl-error-status :ioctl-decoded-status (case usbd-ioctl-error-status
                                                                                 (0 "SUCCESS") (2 "PENDING") (t "ERROR"))
                   :ioctl-error-code usbd-ioctl-error-code :ioctl-decoded-error (case usbd-ioctl-error-code
                                                                                  (0 "Success") (1 "CRC error") (2 "BTS error") (3 "Data toggle mismatch") (4 "The device returned a stall packet identifier") (5 "The device is not responding") (6 "The device returned a packet identifier check failure") (7 "The device returned an unexpected packet identifier error") (8 "The device returned a data overrun error") (9 "The device returned a data underrun error") (12 "The device returned a buffer overrun error") (13 "The device returned a buffer underrun error") (15 "The USB stack could not access the device") (16 "The device returned a FIFO error") (17 "The device returned a transaction error") (18 "The device returned a babble detected error") (19 "Data buffer error") (t (format nil "Unknown error ~D" usbd-ioctl-error-code)))
                   :return-value ret :secondary-return-value (cffi:mem-ref retptr :int) :addendum ""
                   :error-code ler :decoded (case ler
                                              (31 "A device attached to the system is not functioning")
                                              (87 "Invalid parameters")
                                              (998 "Invalid access to memory location")
                                              (1167 "The device is not connected")))))))
    (cffi:mem-ref retptr :int)))


(defun ezusb-get-last-usbd-error (handle &aux (*in-nested-ezusb-ioctl* 'get-last-usbd-error))
  (declare (special *in-nested-ezusb-ioctl*))
  (cffi:with-foreign-objects ((last-usbd-error :int))
    (ezusb-ioctl handle +ezusb-ioctl-get-last-error+
                 (cffi:null-pointer) 0 last-usbd-error 4 0)))

(defun ezusb-get-frame-number (handle &aux (*in-nested-ezusb-ioctl* 'get-frame-number))
  (declare (special *in-nested-ezusb-ioctl*))
  (cffi:with-foreign-objects ((frame-number :int))
    (ezusb-ioctl handle +ezusb-ioctl-get-frame-number+
                 (cffi:null-pointer) 0 frame-number 4 0)))

(defun ezusb-reset-ezusb (handle)
  (ezusb-ioctl handle +ezusb-ioctl-reset+ (cffi:null-pointer) 0 (cffi:null-pointer) 0 0))

(defun ezusb-bulk-io (handle io-type datavec datasize &optional (datavec-offset 0))
  #+nil (when (= datasize 3) (break))
  (let ((ep (ecase io-type (:write 0) (:command 1) (:read 2))))
    (cffi:with-pointer-to-vector-data (dataptr datavec)
      (cffi:with-foreign-objects ((pipe-selector :int))
        (setf (cffi:mem-ref pipe-selector :int) ep)
        #+nil
        (syncformat t "~&BULK ~X ~:[READ  ~;WR/CMD: ~S~%~]"
                    datasize (member io-type '(:write :command)) (subseq datavec datavec-offset (+ datavec-offset datasize))
                    #+nil
                    (case io-type
                      (:write (decode :ezusb-command-header (aref datavec 0)))
                      (:command (decode :ezusb-cmd (aref datavec 0)))))
        (prog1 (ezusb-ioctl handle (ecase io-type (:read +ezusb-ioctl-bulk-read+) ((:write :command) +ezusb-ioctl-bulk-write+))
                            pipe-selector 4
                            dataptr datasize
                            datavec-offset
                            t)
          #+nil
          (when (eq io-type :read)
            (syncformat t ": ~S~%" (subseq datavec datavec-offset (+ datavec-offset datasize)))))))))

(defun ezusb-close (interface)
  "Close the EZUSB interface."
  (unless (close-handle (ezusb-interface-handle interface))
    (let ((ler (get-last-error)))
      (error 'win32-error :function-name 'close-handle :args (list (ezusb-interface-handle interface))
             :return-value 0 :error-code ler :decoded (case ler)))))

(define-device-class ezusb-bus :empty (root-bus probe-discovery-bus interface-bus)
  ()
  (:default-initargs :probe-address-set '(0 1 2 3 4 5 6 7)))

(defmethod bus-probe-address ((o ezusb-bus) address)
  (when-let ((handle (ezusb-do-open address)))
    (format t "opening ~A/~A => ~S~%" address (bus-address-filename address) handle)
    (close-handle handle)
    t))
