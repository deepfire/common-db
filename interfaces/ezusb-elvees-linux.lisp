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

(defconstant +multicore-ezusb-vendor-id+ #x0547)
(defconstant +multicore-ezusb-device-id+ #x1002)
(defconstant +multicore-ezusb-interface-nr+ #x0)

(define-device-class ezusb-bus :empty (root-bus enumerating-bus interface-bus)
  ())

(defmethod bus-occupied-addresses ((o ezusb-bus))
  (when *verbose-interface-init*
    (syncformat t "NOTE: scanning USB busses for EZUSB devices~%"))
  (iter (for (bus dev) in (lh-usb:find-usb-devices-by-id +multicore-ezusb-vendor-id+ +multicore-ezusb-device-id+))
        (collect (dpb bus (byte 16 16) dev))))

(defconstant +get-status+ 0)
(defconstant +clear-feature+ 1)
(defconstant +set-feature+ 3)
(defconstant +set-address+ 5)
(defconstant +get-descriptor+ 6)
(defconstant +set-descriptor+ 7)
(defconstant +get-configuration+ 8)
(defconstant +set-configuration+ 9)
(defconstant +get-interface+ 10)
(defconstant +set-interface+ 11)
(defconstant +synch-frame+ 12)

(defun (setf usb-device-active-config-id) (id handle)
  (let ((data (make-array 0 :element-type '(unsigned-byte 8))))
    (lh-usb:usb-control handle #x0 +set-configuration+ id 0 data)))

(defun ezusb-open (address)
  (let ((bus (ldb (byte 16 16) address))
        (dev (ldb (byte 16 0) address)))
    (syncformat t "NOTE: opening an EZUSB device on ~3,'0D:~3,'0D~%" bus dev)
    (lret ((handle (lh-usb:open-usb-device bus dev)))
      (handler-case (let ((*break-on-signals* nil))
                      (with-condition-recourses lh-usb:usb-error
                          (lh-usb:usb-claim-interface handle 0)
                        (try-disconnect ()
                          (lh-usb:usb-disconnect handle +multicore-ezusb-interface-nr+)))
                      (setf (usb-device-active-config-id handle) 1))
        (lh-usb:usb-error (c)
          (syncformat t "ERROR: disconnecting & closing down ~S: open failure%" handle)
          (lh-usb:usb-disconnect handle +multicore-ezusb-interface-nr+)
          (close handle)
          (error c))))))

(defun ezusb-close (interface)
  (let ((handle (ezusb-interface-handle interface)))
    (lh-usb:usb-disconnect handle +multicore-ezusb-interface-nr+)
    (syncformat t "NOTE: closing USB interface ~S~%" handle)
    (close handle)))

(defun ezusb-bulk-io (handle io-type datavec datasize &optional (datavec-offset 0)
                      &aux (ep (ecase io-type
                                 (:write 2)
                                 (:command 4)
                                 (:read #x86))))
  (lh-usb:usb-bulk handle ep datavec datavec-offset datasize))

;; (defbintype usb-descriptor ()
;;   (:documentation "USB descriptor")
;;   (:type :structure)
;;   (:prefix desc-)
;;   (:fields
;;    (value length (unsigned-byte 8))
;;    (match type   (unsigned-byte 8)
;;           ((#x1 'device)
;;            (#x6 'device-qualifier)
;;            (#x2 'configuration)
;;            (#x7 'other-speed-configuration)
;;            (#x4 'interface)
;;            (#x5 'endpoint)
;;            (#x3 'string)
;;            (#x8 'interface-power)))
;;    (value body (typecase (path-value *self* 'type)
;;                  (device usb-device-descriptor)
;;                  (device-qualifier usb-device-qualifier-descriptor)
;;                  ((configuration other-speed-configuration) usb-configuration-descriptor)
;;                  (interface usb-interface-setting-descriptor)
;;                  (endpoint usb-endpoint-descriptor)
;;                  (string usb-string-descriptor)))))

;; (defbintype usb-device-descriptor ()
;;   (:documentation "USB device descriptor")
;;   (:type :structure)
;;   (:prefix dev-desc-)
;;   (:fields
;;    (value  %sizecheck      (pure t (let ((len (path-value *self* :parent 'length)))
;;                                      (unless (= len 18)
;;                                        (bintype-error "~@<USB device descriptor length wrong: ~S~:@>" len))))
;;            :ignore t)
;;    (value  usb-version     (bcd 16))
;;    (value  class           (unsigned-byte 8))
;;    (value  subclass        (unsigned-byte 8))
;;    (value  protocol        (unsigned-byte 8))
;;    (value  max-packet-size (unsigned-byte 8))
;;    (value  vendor-id       (unsigned-byte 16))
;;    (value  product-id      (unsigned-byte 16))
;;    (value  device-version  (bcd 16))
;;    (value  mfgr-desc       (unsigned-byte 8))
;;    (value  product-desc    (unsigned-byte 8))
;;    (value  serial-desc     (unsigned-byte 8))
;;    (value  num-configs     (unsigned-byte 8))))

;; (defbintype usb-device-qualifier-descriptor ()
;;   (:documentation "USB device qualifier descriptor")
;;   (:type :structure)
;;   (:prefix devq-desc-)
;;   (:fields
;;    (value  %sizecheck      (pure t (let ((len (path-value *self* :parent 'length)))
;;                                      (unless (= len 10)
;;                                        (bintype-error "~@<USB device qualifier descriptor length wrong: ~S~:@>" len))))
;;            :ignore t)
;;    (value  usb-version     (bcd 16))
;;    (value  class           (unsigned-byte 8))
;;    (value  subclass        (unsigned-byte 8))
;;    (value  protocol        (unsigned-byte 8))
;;    (value  max-packet-size (unsigned-byte 8))
;;    (value  num-configs     (unsigned-byte 8))
;;    (match  reserved        (unsigned-byte 8) ((0 t)) :ignore t)))

;; (defbintype usb-endpoint-descriptor ()
;;   (:documentation "USB endpoint descriptor")
;;   (:type :structure)
;;   (:prefix ep-desc-)
;;   (:fields
;;    (value  %sizecheck      (pure t (let ((len (path-value *self* :parent 'length)))
;;                                      (unless (= len 7)
;;                                        (bintype-error "~@<USB endpoint descriptor length wrong: ~S~:@>" len))))
;;            :ignore t)
;;    (value index           (unsigned-byte 4))
;;    (value reserved0       (unsigned-byte 3) :ignore t)
;;    (flag read-p)
;;    (match type            (unsigned-byte 2)
;;           ((0 :control) (1 :isochronous) (2 :bulk) (3 :interrupt)))
;;    (match sync-type       (unsigned-byte 2)
;;           ((0 :unsychronised) (1 :asynchronous) (2 :adaptive) (3 :synchronous)))
;;    (match usage-type      (unsigned-byte 2)
;;           ((0 :data) (1 :feedback) (2 :implicit-feedback-data) (3 :reserved)))
;;    (value reserved1       (unsigned-byte 2) :ignore t)
;;    (value max-packet-size (unsigned-byte 11))
;;    (match opportuninies   (unsigned-byte 2)
;;           ((0 :1txn/uframe) (1 :2txn/uframe) (2 :3txn/uframe) (3 :reserved)))
;;    (value reserved2       (unsigned-byte 3) :ignore t)
;;    (value poll-interval   (unsigned-byte 8))))

;; (defbintype usb-interface-setting-descriptor ()
;;   (:documentation "USB interface setting descriptor")
;;   (:type :structure)
;;   (:prefix ifset-desc-)
;;   (:fields
;;    (value  %sizecheck      (pure t (let ((len (path-value *self* :parent 'length)))
;;                                      (unless (= len 9)
;;                                        (bintype-error "~@<USB interface setting descriptor length wrong: ~S~:@>" len))))
;;            :ignore t)
;;    (value index          (unsigned-byte 8))
;;    (value setting-index  (unsigned-byte 8))
;;    (value num-endpoints  (unsigned-byte 8))
;;    (value class          (unsigned-byte 8))
;;    (value subclass       (unsigned-byte 8))
;;    (value protocol       (unsigned-byte 8))
;;    (value desc           (unsigned-byte 8))
;;    (value endpoints      (sequence (path-value *self* 'num-endpoints) :element-type usb-descriptor :stride 56 :format :list))))

;; (defbintype usb-configuration-descriptor ()
;;   (:documentation "USB configuration descriptor")
;;   (:type :structure)
;;   (:prefix cfg-desc-)
;;   (:fields
;;    (value  %sizecheck      (pure t (let ((len (path-value *self* :parent 'length)))
;;                                      (unless (= len 9)
;;                                        (bintype-error "~@<USB config descriptor length wrong: ~S~:@>" len))))
;;            :ignore t)
;;    (value total-length     (unsigned-byte 16))
;;    (value num-interfaces   (unsigned-byte 8))
;;    (value id               (unsigned-byte 8))
;;    (value desc             (unsigned-byte 8))
;;    (value reserved-1       (unsigned-byte 1) :ignore t)
;;    (flag  self-powered-p)
;;    (flag  remote-wakeup-p)
;;    (value reserved-0       (unsigned-byte 5) :ignore t)
;;    (expr  maxconsumption   (unsigned-byte 8) integer (* _ 2))
;;    (value interface0       (usb-descriptor))))

;; (defbintype usb-string-descriptor ()
;;   (:documentation "USB descriptor")
;;   (:type :structure)
;;   (:prefix string-desc-)
;;   (:fields
;;    (value data (sequence (path-value *self* :parent 'length) :element-type (unsigned-byte 8) :format :vector))))

;; (defun usb-get-descriptor (handle type descno)
;;   (flet ((get-descriptor (iovec)
;;            (lh-usb:usb-control handle #x80 +get-descriptor+ (dpb (ecase type
;;                                                                    (:device 1)
;;                                                                    (:configuration 2)
;;                                                                    (:string 3))
;;                                                                  (byte 8 8)
;;                                                                  descno)
;;                                0 iovec)))
;;     (let ((predata (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer t)))
;;       (get-descriptor predata)
;;       (let* ((total-length (if (eq type :configuration)
;;                                (u8-vector-word16le predata 2)
;;                                (aref predata 0)))
;;              (data (make-array total-length :element-type '(unsigned-byte 8) :fill-pointer t)))
;;         (get-descriptor data)
;;         (parse 'usb-descriptor data :endianness :little-endian)))))

;; (defun usb-get-status (handle type index)
;;   (let ((data (make-array 2 :element-type '(unsigned-byte 8))))
;;     (lh-usb:usb-control handle (ecase type
;;                                  (:device #x80)
;;                                  (:interface #x81)
;;                                  (:endpoint #x82)) +get-status+ 0 index data)
;;     data))

;; (defun usb-device-active-config-id (handle)
;;   (let ((data (make-array 1 :element-type '(unsigned-byte 8))))
;;     (lh-usb:usb-control handle #x80 +get-configuration+ 0 0 data)
;;     (aref data 0)))

;; (defun usb-device-interface-altsetting (handle ifno)
;;   (let ((data (make-array 1 :element-type '(unsigned-byte 8))))
;;     (lh-usb:usb-control handle #x81 +get-interface+ 0 ifno data)
;;     (aref data 0)))
