;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: VIRTCORE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :virtcore)

;;;
;;; The generic base for all known virt platforms.  Everything should subplatform this.
;;;
(define-platform generic-virtual-platform (virtual-platform)
  (:instantiate-p t)
  (:predefined-devices
   ;;                                                         And why exactly don't we have memory detected there?
   (virtcore      :slaves `((internal-memory :extent ,(extent #x0 #x10000))))
   (virtport      :base #x18001000
                  :slave '(virtport-dma-channel :base #x18001080))
   (virtport      :base #x18001100
                  :slave '(virtport-dma-channel :base #x18001180))
   (virtsys       :base #x18000000)))

(define-platform basic-virtual-platform (generic-virtual-platform)
  (:predefined-devices
   (virtcore      :slave '(internal-memory :slave (memory-dma-engine :base #x18000800)))
   (virtimer      :base #x18000100)
   (virtmport     :base #x18000200)))

(define-platform virtual-platform-d02 (generic-virtual-platform)
  (:predefined-devices
   (virtcore      :slave '(internal-memory :slave (memory-dma-engine :base #x18000800)))
   (virtimer      :base #x18000100)
   (virtmport-02  :base #x18000200)))
