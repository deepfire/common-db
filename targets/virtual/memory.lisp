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


(define-protocol-device-class mport :platform (virtual-mapped-device)
  ()
  (:default-initargs :enumeration-class 'mport))

(define-device-class virtmport :platform (mport)
 ()
  (:layouts (:vmport mapped-ref (setf mapped-ref))))

(define-device-class virtmport-02 :platform (mport)
  ()
  (:layouts (:vmport-02 mapped-ref (setf mapped-ref))))

(setf *memory-configurations*
      (alist-hash-table
       (list
        `(:plain .  ,(make-memory-config
                      :plain
                      '(:vcscon0      (:type        :enable      :wait-states  :busaddr  :addrmask)
                                      (:sync+dyn    t            0             0         #xfc))
                      '(:vcscon1      (:type        :enable      :wait-states  :busaddr  :addrmask)
                                      (:sync+dyn    t            0             4         #xfc))
                      '(:vmemcon      (:init        :cas-latency :refresh      :page)
                                      (t            :cl-3        #x70          :|512|))))
        `(:d02-1 .  ,(make-memory-config
                      :d02-1
                      '(:vcscon0      (:type        :enable      :wait-states  :busaddr  :addrmask)
                                      (:sync+dyn    t            0             0         #xfc))
                      '(:vcscon1      (:type        :enable      :wait-states  :busaddr  :addrmask)
                                      (:async+wait  t            4             8         #xfc))
                      '(:vmemcon-d02  (:init        :cas-latency :page         :prefetch)
                                      (t            :cl-3        :|512|        16))))
        `(:d02-2 . ,(make-memory-config
                     :d02-2
                     '(:vcscon0      (:type         :enable      :wait-states  :busaddr  :addrmask)
                                     (:sync+dyn     t            0             0         #xfc))
                     '(:vcscon1      (:type         :enable      :wait-states  :busaddr  :addrmask)
                                     (:async+wait   t            4             8         #xfc))
                     '(:vmemcon-d02  (:init         :cas-latency :page         :prefetch)
                                     (t             :cl-2        :|512|        32)))))))

(setf *memory-configuration-order*
      '(:plain :d02-2 :d02-1))

(defmethod memory-config-valid-for-platform-p ((o virtual-platform) (c memory-config))
  (memory-config-valid-for-device-classes-p
   c (list (class-of (target-device (platform-target o) '(mport 0))))))

(defgeneric apply-memory-config-using-mport (mport config)
  (:method :around ((o mport) (c memory-config))
    (with-maybe-logged-device-io (o *log-stream* *log-system-configuration*)
      (iter (for (reg nil val) in (memory-config-register-values c))
            (case reg
              (:vcscon0
               (setc (devbits o :vcscon0 (:type :enable :wait-states :busaddr :addrmask) :write-only t) val))
              (:vcscon1
               (setc (devbits o :vcscon1 (:type :enable :wait-states :busaddr :addrmask) :write-only t) val))))
      (call-next-method)))
  (:method ((o virtmport) (c memory-config))
    (iter (for (reg nil val) in (memory-config-register-values c))
          (case reg
            (:vmemcon
             (setc (devbits o :vmemcon (:init :cas-latency :refresh :page) :write-only t) val)))))
  (:method ((o virtmport-02) (c memory-config))
    (iter (for (reg nil val) in (memory-config-register-values c))
          (case reg
            (:vmemcon-d02
             (setc (devbits o :vmemcon-d02 (:init :cas-latency :page :prefetch) :write-only t) val))))))

(defmethod apply-memory-config ((o virtual-platform) config)
  (let ((mport (target-device (platform-target o) '(mport 0))))
    (unless (memory-config-valid-for-device-classes-p config (list (class-of-device mport)))
      (error "~@<Memory configuration ~A is invalid for mport device class ~S.~:@>"
             (memory-config-name config) (class-name (class-of-device mport))))
    (apply-memory-config-using-mport mport config)))
