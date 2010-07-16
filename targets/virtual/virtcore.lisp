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


(set-namespace :core :target :interface)

(defstruct pipe
  (pcf #xbfc00000)
  (pcd 0) (dinsn 0)
  (pce 0) (einsn 0)
  (pcm 0)
  (pcw 0))

(defun pipeline-reg (core sel &aux
                     (pipe (virtcore-pipe core)))
  (if (eq (state core) :free)
      (ecase sel
        (#x0 (pipe-pcf pipe))
        (#x1 (pipe-pcd pipe))
        (#x2 (pipe-pce pipe))
        (#x3 (pipe-pcm pipe))
        (#x4 (pipe-pcw pipe))
        (#x5 (pipe-dinsn pipe)))
      (ecase sel
        (#x0 (moment-fetch (saved-core-moment core)))
        (#x1 (trail-decode (saved-core-trail core)))
        (#x2 (trail-execute (saved-core-trail core)))
        (#x3 (trail-mem (saved-core-trail core)))
        (#x4 (trail-wb (saved-core-trail core)))
        (#x5 (moment-opcode (saved-core-moment core))))))

(defun (setf pipeline-reg) (value core sel)
  (ecase sel
    (#x0 (setf (moment-fetch (saved-core-moment core)) value))
    (#x5 (setf (moment-opcode (saved-core-moment core)) value)))
  value)

(defun virt-gpr (core sel) (aref (virtcore-gpr core) sel))
(defun virt-fpr (core sel) (aref (virtcore-fpr core) sel))
(defun virt-cop0 (core sel) (aref (virtcore-cop0 core) sel))
(defun virt-cop1control (core sel) (aref (virtcore-cop1control core) sel))
(defun virt-hilo (core sel) (aref (virtcore-hilo core) sel))
(defun (setf virt-gpr) (val core sel) (setf (aref (virtcore-gpr core) sel) val))
(defun (setf virt-fpr) (val core sel) (setf (aref (virtcore-fpr core) sel) val))
(defun (setf virt-cop0) (val core sel) (setf (aref (virtcore-cop0 core) sel) val))
(defun (setf virt-cop1control) (val core sel) (setf (aref (virtcore-cop1control core) sel) val))
(defun (setf virt-hilo) (val core sel) (setf (aref (virtcore-hilo core) sel) val))

(define-device-class virtcore :core (mipsel torn-pipeline-mips-core)
  ((pipe :accessor virtcore-pipe :initarg :pipe)
   (state :accessor virtcore-state :type (member :running :stopped :stopped-kernel-mode) :initarg :state)
   (gpr :reader virtcore-gpr :initarg :gpr)
   (fpr :reader virtcore-fpr :initarg :fpr)
   (cop0 :reader virtcore-cop0 :initarg :cop0)
   (cop1control :reader virtcore-cop1control :initarg :cop1)
   (hilo :reader virtcore-hilo :initarg :hilo))
  (:default-initargs
   :pipe (make-pipe)
   :insn-instruction-limit 0
   :state :running
   :gpr (make-array 32 :element-type '(unsigned-byte 32))
   :fpr (make-array 32 :element-type '(unsigned-byte 32))
   :cop0 (make-array 32 :element-type '(unsigned-byte 32))
   :cop1control (make-array 32 :element-type '(unsigned-byte 32))
   :hilo (make-array 2 :element-type '(unsigned-byte 32))
   :default-sw-breakpoint-type 'virtcore-mips-software-breakpoint
   :traps          (alist-hash-table
                    `((#xbfc00000 ,(make-instance 'virtcore-vector-trap :address #xbfc00000))
                      (#x80000000 ,(make-instance 'virtcore-vector-trap :address #x80000000))
                      (#x80000180 ,(make-instance 'virtcore-vector-trap :address #x80000180))
                      (#x80000200 ,(make-instance 'virtcore-vector-trap :address #x80000200))
                      (#xbfc00200 ,(make-instance 'virtcore-vector-trap :address #xbfc00200))
                      (#xbfc00380 ,(make-instance 'virtcore-vector-trap :address #xbfc00380))
                      (#xbfc00400 ,(make-instance 'virtcore-vector-trap :address #xbfc00400))))
   :hw-breakpoints (alist-hash-table
                    `((0 . ,(make-instance 'virtcore-mips-hardware-breakpoint :id 0 :address 0
                                           :memory-p nil :bound-p nil :read t :write nil :condition :equal))
                      (1 . ,(make-instance 'virtcore-mips-hardware-breakpoint :id 1 :address 4
                                           :memory-p nil :bound-p nil :read t :write nil :condition :equal)))))
  (:layouts (:vpipeline pipeline-reg (setf pipeline-reg))
            (:gpr virt-gpr (setf virt-gpr))
            (:fpr virt-fpr (setf virt-fpr))
            (:cop0 virt-cop0 (setf virt-cop0))
            (:cop1control virt-cop1control (setf virt-cop1control))
            (:hilo virt-hilo (setf virt-hilo))))

(defclass virtcore-trail (trail)
  ((dec :accessor trail-decode :initarg :dec)
   (exec :accessor trail-execute :initarg :exec)
   (mem :accessor trail-mem :initarg :mem)
   (wb :accessor trail-wb :initarg :wb)))

(define-print-object-method ((o virtcore-trail) dec exec mem wb)
    "PCdec ~8,'0X, PCexec ~8,'0X, PCmem ~8,'0X, PCwb ~8,'0X" dec exec mem wb)

(defmethod initialize-instance :after ((o virtcore) &key &allow-other-keys)
  (save-core-moment o)
  (save-core-trail o))

;;;;
;;;; Simulation machinery basics
;;;;
(defun execute-insn (core insn address)
  (declare (ignore core insn address)))

(defun step-pipeline (core &optional address &aux
                      (target (backend core))
                      (pipe (virtcore-pipe core)))
  (let ((nupipe (make-pipe :pcf (or address (+ (pipe-pcf pipe) (memory-device-byte-width target)))
                           :pcd (pipe-pcf pipe)
                           :pce (pipe-pcd pipe)
                           :pcm (pipe-pce pipe)
                           :pcw (pipe-pcm pipe)
                           :dinsn (memory-ref target (pipe-pcf pipe))
                           :einsn (pipe-einsn pipe))))
    ;; Let's hope that assignments are atomic...
    (setf (virtcore-pipe core) nupipe)
    ;;
    ;; The stuff below isn't C-c -safe..
    ;;
    ;; these checks should be partially deferred to ANALYSE-CORE
    (when (zerop (decf (core-insn-execution-limit core)))
      (setf (core-stop-reason core) (make-instance 'instruction-count-trap :core core)
            (state core) :stopped))
    (when (= (pipe-einsn pipe) (encode-mips-insn :break 0))
      (setf (core-stop-reason core) (or (find-if (of-type 'software-breakpoint)
                                                 (append (traps core (pipe-pcd pipe))
                                                         (traps core (pipe-pce pipe))))
                                        ;; XXX: missing information -- guessing SWbreak's address
                                        (recognise-sw-breakpoint core (pipe-pce pipe)))
            (state core) :stopped))))

;;;;
;;;; Moment/trail API
;;;;
(defmethod current-core-moment ((o virtcore) &aux
                                (pipe (virtcore-pipe o)))
  (make-moment 'moment (pipe-pcf pipe) (pipe-dinsn pipe)))

(defmethod set-current-core-moment ((o virtcore) (m moment) &aux
                                    (pipe (virtcore-pipe o)))
  (setc (pipe-pcf pipe) (moment-fetch m)
        (pipe-dinsn pipe) (moment-opcode m)))

(defmethod make-neutral-moment ((o virtcore) address)
  (make-moment 'moment address 0))

(defmethod current-core-trail ((o virtcore) &aux
                               (pipe (virtcore-pipe o)))
  (make-instance 'virtcore-trail
                 :dec (pipe-pcd pipe)
                 :exec (pipe-pce pipe)
                 :mem (pipe-pcm pipe)
                 :wb (pipe-pcw pipe)))

(defmethod set-current-core-trail ((o virtcore) trail &aux
                                   (pipe (virtcore-pipe o)))
  (setf (pipe-pcd pipe) (trail-decode trail)
        (pipe-pce pipe) (trail-execute trail)
        (pipe-pcm pipe) (trail-mem trail)
        (pipe-pcw pipe) (trail-wb trail)))

(defmethod make-neutral-trail ((o virtcore))
  (make-instance 'virtcore-trail :dec 0 :exec 0 :mem 0 :wb 0))

(defmethod core-pipeline-addresses ((o virtcore) &optional cached &aux
                                    (pipe (virtcore-pipe o)))
  (declare (ignore cached))
  (list (pipe-pcf pipe) (pipe-pcd pipe) (pipe-pce pipe) (pipe-pcm pipe) (pipe-pcw pipe)))

(defmethod (setf pc) (value (o virtcore))
  (setf (pipe-pcf (virtcore-pipe o)) value))

(defmethod print-pipeline ((o virtcore) stream &aux
                           (pipe (virtcore-pipe o)))
  (format stream "PIPELINE: ~8,'0X ~8,'0X ~8,'0X ~8,'0X ~8,'0X, ~8,'0X ~S~%"
          (pipe-pcf pipe)
          (pipe-pcd pipe)
          (pipe-pce pipe)
          (pipe-pcm pipe)
          (pipe-pcw pipe)
          (pipe-dinsn pipe)
          (decode-mips-insn (pipe-dinsn o)))
  (with-slots (dec exec mem wb) (saved-core-trail o)
    (format stream "SHADOW  : ~8,'0X ~8,'0X ~8,'0X ~8,'0X ~8,'0X, ~8,'0X ~S~%"
            (moment-fetch (saved-core-moment o)) dec exec mem wb (moment-opcode (saved-core-moment o))
            (decode-mips-insn (moment-opcode (saved-core-moment o))))))

;;;;
;;;; Target API
;;;;
(defmethod reset-target-using-interface ((o virtual-target) interface)
  (declare (ignore interface))
  (when-let ((core (target-device o '(general-purpose-core 0) :continue)))
    (setf (virtcore-pipe core) (make-pipe))))

(defmethod stop-target-using-interface ((o virtual-target) interface)
  (declare (ignore interface))
  (when-let ((core (target-device o '(general-purpose-core 0) :continue)))
    (setf (virtcore-state core)
          (if (or *virtual-interface-stop-during-reset*
                  (not (core-running-p core)))
              :stopped
              :running))))

;;;;
;;;; Core API
;;;;
(defmethod gpr ((o virtcore) gpr)
  (declare (ignore gpr)))

(defmethod set-gpr ((o virtcore) gpr val)
  (declare (ignore gpr val)))

(defmethod fpr ((o virtcore) fpr)
  (declare (ignore fpr)))

(defmethod set-fpr ((o virtcore) fpr val)
  (declare (ignore fpr val)))

(defmethod flush-core-instruction-cache ((o virtcore)))
(defmethod flush-core-data-cache ((o virtcore)))

(defmethod finish-core-pipeline ((o virtcore)))

(defmethod core-running-p ((o virtcore))
  (eq (virtcore-state o) :running))

(defmethod analyse-core ((core virtcore)))

(defmethod (setf core-running-p) ((run-p (eql t)) (o virtcore))
  (setf (virtcore-state o) :running)
  (with-sigint-trap t
    (sleep 0.01)
    (step-pipeline o)))

(defmethod (setf core-running-p) ((run-p (eql nil)) (core virtcore) &aux (iface (backend (backend core))))
  (interface-attach-target iface)
  (interface-stop-target iface)
  (freeze-core-slaves core)
  (save-core-moment core)
  (save-core-trail core)
  (setf (core-trail-important-p core) nil)
  nil)

(defmethod step-core-asynchronous ((core virtcore) &optional step-slaves)
  (declare (ignore step-slaves))
  (freeze-core-slaves core)
  (thaw-core-slaves core))

;;;;
;;;; State machinery
;;;;
(defmethod free-to-stop :around ((core virtcore) &key &allow-other-keys)
  (setf (core-running-p core) nil))

(defmethod stop-to-debug :around ((core virtcore) &key &allow-other-keys)
  (setf (virtcore-state core) :stopped-kernel-mode)
  (when (next-method-p)
    (call-next-method))                 ; Insanity lurks nearby...
  (capture-instruction-counters core)) ; We can do register IO on all platforms by now.

(defmethod debug-to-stop :around ((core virtcore) &key &allow-other-keys)
  (restore-instruction-counters core) ; Last chance to fixup instruction counters without irreversible register damage.
  (when (next-method-p)
    (call-next-method))                 ; Insanity lurks nearby...
  (thaw-core-slaves core)
  (setf (virtcore-state core) :stopped))

;;;;
;;;; Breakpoints & traps
;;;;
(defmethod set-core-insn-execution-limit ((o virtcore) ninsns))

(defclass virtcore-vector-trap (vector-trap) ())
(defclass virtcore-mips-software-breakpoint (mips-software-breakpoint) ())
(defclass virtcore-mips-hardware-breakpoint (enumerated-trap skippable-trap mips-hardware-breakpoint)
  ((memory-p :accessor breakpoint-memory-p :initform nil :initarg :memory-p :type boolean)
   (bound-p :accessor breakpoint-bound-p :initform nil :initarg :bound-p :type boolean)
   (read :accessor breakpoint-read :initform nil :initarg :read :type boolean)
   (write :accessor breakpoint-write :initform nil :initarg :write :type boolean)
   (condition :accessor breakpoint-condition :initform t :initarg :condition)))

(define-print-object-method ((b virtcore-mips-hardware-breakpoint) core::address id memory-p read write condition)
    "~@<#<~;~A ~8,'0X id: ~D mem: ~S rw: ~B~B condition: ~S~;>~:@>" (type-of b) core::address id memory-p read write condition)

(defmethod setup-hw-trap ((b virtcore-mips-hardware-breakpoint) address skipcount &key (read t) (write nil) bound memory)
  (declare (type (or null (unsigned-byte 32)) address))
  (setf (trap-skipcount b) skipcount
        (breakpoint-memory-p b) memory
        (breakpoint-read b) (and address read)
        (breakpoint-write b) (and address write)
        (breakpoint-bound-p b) bound)
  b)

(defmethod add-cell-watchpoint ((core virtcore) address &optional (skipcount 0))
  (setup-hw-trap (allocate-hardware-breakpoint core) address skipcount :read t :write t :memory t))

(defmethod enable-trap ((b virtcore-mips-software-breakpoint))
  (let ((target (backend (trap-core b)))
        (address (trap-address b)))
    (setf (software-breakpoint-saved-insn b) (memory-ref target address)
          (memory-ref target address) (encode-mips-insn :break 0))))

(defmethod disable-trap ((b virtcore-mips-software-breakpoint))
  (let ((target (backend (trap-core b)))
        (address (trap-address b)))
    (setf (memory-ref target address) (software-breakpoint-saved-insn b))))

(defmethod enable-trap ((o virtcore-mips-hardware-breakpoint)))
(defmethod disable-trap ((o virtcore-mips-hardware-breakpoint)))
(defmethod enable-trap ((o virtcore-vector-trap)))
(defmethod disable-trap ((o virtcore-vector-trap)))

(defmethod deduce-stop-reason ((core virtcore)))
