;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HOST-PCI; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :host-pci)


(define-foreign-library libpciaccess
  (:unix (:or  "/usr/lib/libpciaccess.so.0"))
  (t (:default "libpciaccess")))

#+(or)
(handler-case
    (progn (use-foreign-library libpciaccess)
           (defcfun ("pci_device_map_region" pci-device-map-region)                   :int (dev pci-device) (region :unsigned-int) (write-enable :int))
           (defcfun ("pci_device_unmap_region" pci-device-unmap-region)               :int (dev pci-device) (region :unsigned-int))
           (defcfun ("pci_device_probe" pci-device-probe)                             :int (dev pci-device))
           (defcfun ("pci_system_init" pci-system-init)                               :int)
           (defcfun ("pci_system_cleanup" pci-system-cleanup)                         :int)
           (defcfun ("pci_id_match_iterator_create" pci-id-match-iterator-create)     pci-device-iterator (match pci-id-match))
           (defcfun ("pci_slot_match_iterator_create" pci-slot-match-iterator-create) pci-device-iterator (match pci-slot-match))
           (defcfun ("pci_iterator_destroy" pci-iterator-destroy)                     :void (iter pci-device-iterator))
           (defcfun ("pci_device_next" pci-device-next)                               pci-device (iter pci-device-iterator))
           (defcfun ("pci_device_find_by_slot" pci-device-find-by-slot)               pci-device (domain :unsigned-int) (bus :unsigned-int) (dev :unsigned-int) (func :unsigned-int))
           (defcfun ("pci_device_cfg_read_u32" pci-device-cfg-read-32)                :int (device pci-device) (data :pointer) (offset :unsigned-int))
           (defcfun ("pci_device_cfg_write_u32" pci-device-cfg-write-32)              :int (device pci-device) (data :unsigned-int) (offset :unsigned-int)))
  (error ()
    (format t "~@<Failed to load libpciaccess. HOSTPCI functionality will not be available.~:@>")))

(defcstruct (pci-mem-region :size 32)
  (memory :pointer)
  (bus-addr :unsigned-int)
  (base-addr :unsigned-int)
  (size :unsigned-int))

(defcstruct (pci-device :size 256)
  (domain :uint16)
  (bus :uint8)
  (dev :uint8)
  (func :uint8)
  (vendor-id :uint16)
  (device-id :uint16)
  (subvendor-id :uint16)
  (subdevice-id :uint16)
  (device-class :uint32)
  (revision :uint8)
  (regions pci-mem-region :count 6 :offset 28)
  (rom-size :uint32)
  (irq :int)
  (user-data :int))

(defctype pci-device-iterator :pointer)

(defconstant +pci-match-any+ #xffffffff)

(defcstruct pci-id-match
  (vendor-id :unsigned-int)
  (device-id :unsigned-int)
  (subvendor-id :unsigned-int)
  (subdevice-id :unsigned-int)
  (device-class :unsigned-int)
  (device-class-mask :unsigned-int)
  (match-data :unsigned-int))

(defcstruct pci-slot-match
  (domain :unsigned-int)
  (bus :unsigned-int)
  (dev :unsigned-int)
  (func :unsigned-int)
  (match-data :unsigned-int))

(defun pci-device-config-32bit (device offset)
  (with-foreign-object (val :uint32)
    (let ((val (pci-device-cfg-read-32 device val offset)))
    (unless (= 0 val)
      (error "There was an error reading the config register of the host PCI device ~X ~D: ~D ~D: ~D."
	     (sb-kernel:get-lisp-obj-address device) offset
	     (foreign-slot-value device 'pci-device 'dev)
	     (foreign-slot-value device 'pci-device 'func)
	     val
	     )))
    (mem-aref val :uint32)))

(defun (setf pci-device-config-32bit) (val device offset)
  (unless (= 0 (pci-device-cfg-write-32 device val offset))
    (error "There was an error writing the config register of the host PCI device ~D ~D."
	   (foreign-slot-value device 'pci-device 'dev)
	   (foreign-slot-value device 'pci-device 'func))))

(defmacro with-libpciaccess (&body body)
  `(unwind-protect
	(progn 
	  (pci-system-init)
	   ,@body)
     (pci-system-cleanup)))

(defun print-pci-region (reg)
  (format t "  region: bus-addr: ~X  base-addr: ~X  size: ~X~%"
	  (foreign-slot-value reg 'pci-mem-region 'bus-addr)
	  (foreign-slot-value reg 'pci-mem-region 'base-addr)
	  (foreign-slot-value reg 'pci-mem-region 'size)))

(defun print-pci-device (dev)
  (format t "device: ~X.~X.~X.~X vendor ID: ~X device ID: ~X cfg0: ~X~%"
	  (foreign-slot-value dev 'pci-device 'domain)
	  (foreign-slot-value dev 'pci-device 'bus)
	  (foreign-slot-value dev 'pci-device 'dev)
	  (foreign-slot-value dev 'pci-device 'func)
	  (foreign-slot-value dev 'pci-device 'vendor-id)
	  (foreign-slot-value dev 'pci-device 'device-id)
	  (pci-device-config-32bit dev 0)
	  )
  (dotimes (i 6)
    (print-pci-region (mem-aref (foreign-slot-value dev 'pci-device 'regions) 'pci-mem-region i))))

(defun list-all ()
  (loop :with iterator = (pci-slot-match-iterator-create (null-pointer))
	:for device = (pci-device-next iterator)
	:until (null-pointer-p device)
     :do (progn
	   (pci-device-probe device)
	   (print-pci-device device))
     :finally (pci-iterator-destroy iterator)))