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

(defconstant +oncd-ir-length+   4)
(defconstant +oncd-ird-length+  8)
(defconstant +idcode-length+    32)

;; Delay values, in nanoseconds.
(defconstant +reset-delay+ 1000000)
(defconstant +generic-delay+ 10000)
(defconstant +memory-delay+ 1000000)

(defmacro build-pkt (type size irdr ndebug nrst ntrst)
  `(bits (:ez-hdr-type :sys-nrst :sys-ntrst :size :ndebug-oper :irdrsel) ,type ,nrst ,ntrst ,size ,ndebug ,irdr))

(defmacro build-vec-pkt (vec offt type trst rst size debug irdr second-byte)
  `(setf (aref ,vec (+ ,offt 0)) (build-pkt ,type ,size ,irdr ,debug ,rst ,trst)
         (aref ,vec (+ ,offt 1)) ,second-byte))

(defmacro show-vec-pkt (vec offt type trst rst size debug irdr second-byte)
  (declare (ignore vec offt))
  `(list (build-pkt ,type ,size ,irdr ,debug ,rst ,trst) ,second-byte))

(declaim (inline build-vec-block-pkt))
(defun build-vec-block-pkt (vec offt type data)
  (build-vec-pkt vec offt (ecase type
                            ((:read-blkend :write-blkend) :idcode/endblk)
                            ((:read-addr :read-data) :block-read)
                            ((:write-addr :write-data) :block-write))
                 t t :size-32 t :dr
                 (ecase type
                   ((:read-addr :write-addr) (bits :regsel :omar))
                   ((:write-blkend :write-data) (bits :regsel :omdr))
                   ((:read-blkend :read-data) (bits (:regsel :rdonly/push-irdec) :omdr t))))
  (setf (u8-vector-word32le vec (+ offt 2)) data))

(declaim (inline build-vec-fastword-pkt))
(defun build-vec-fastword-pkt (vec offt type data)
  (build-vec-pkt vec offt (ecase type
                            ((:read-blkend :write-blkend) :idcode/endblk)
                            ((:read-addr :read-data) :block-read)
                            ((:write-addr :write-data) :block-write))
                 nil t :size-32 t :dr
                 (ecase type
                   ((:read-addr :write-addr) (bits :regsel :omar))
                   ((:write-blkend :write-data) (bits :regsel :omdr))
                   ((:read-blkend :read-data) (bits (:regsel :rdonly/push-irdec) :omdr t))))
  (setf (u8-vector-word32le vec (+ offt 2)) data))

(defmacro build-vec-dr-io-pkt ((vec bulksize) offt name &optional val)
  (let* ((if-class (find-class 'interface))
         (selector (register-selector (space :interface) :tap-dr name))
         (bit-length (aref (aref (device-class-extensions if-class) selector) 1)))
    (multiple-value-bind (header size)
        (case bit-length
          (16 (values (bits (:ez-hdr-type :sys-nrst :sys-ntrst :size :ndebug-oper :irdrsel) :plain t t :size-16 t :dr) 4))
          (12 (values (bits (:ez-hdr-type :sys-nrst :sys-ntrst :size :ndebug-oper :irdrsel) :plain t t :size-12 t :dr) 4))
          (t (values (bits (:ez-hdr-type :sys-nrst :sys-ntrst :size :ndebug-oper :irdrsel) :plain t t :size-32 t :dr) 6)))
      (once-only (offt vec)
        `(setf (aref ,vec (+ ,offt 0)) ,header
               (aref ,vec (+ ,offt 1)) ,(bits (:regsel :rdonly/push-irdec) selector (null val))
               (u8-vector-word32le ,vec (+ ,offt 2)) ,(or val 0)
               ,bulksize (+ ,bulksize ,size))))))

(let ((tap-iovec           (make-array 6 :element-type '(unsigned-byte 8)))
      (reset-iovec         (make-array 2 :element-type '(unsigned-byte 8)))
      (reset-oncd-iovec    (make-array 2 :element-type '(unsigned-byte 8)))
      (idcode-iovec        (make-array 6 :element-type '(unsigned-byte 8)))
      (version-iovec       (make-array 2 :element-type '(unsigned-byte 8)))
      (debug-request-iovec (make-array 2 :element-type '(unsigned-byte 8)))
      (debug-enable-iovec  (make-array 2 :element-type '(unsigned-byte 8)))
      (blkread-addr        (make-array 6 :element-type '(unsigned-byte 8)))
      (blkread-data        (make-array 6 :element-type '(unsigned-byte 8)))
      (blkread-end         (make-array 6 :element-type '(unsigned-byte 8)))
      (blkwrite-addr       (make-array 6 :element-type '(unsigned-byte 8)))
      (blkwrite-data       (make-array 6 :element-type '(unsigned-byte 8)))
      (blkwrite-end        (make-array 6 :element-type '(unsigned-byte 8)))
      (input-iovec         (make-array 32 :element-type '(unsigned-byte 8))))
  (build-vec-pkt reset-iovec         0 :plain         t   nil :size-8  nil :dr (bits (:regsel :rdonly/push-irdec) :enmem t))
  (build-vec-pkt reset-oncd-iovec    0 :plain         nil t   :size-8  nil :dr (bits (:regsel :rdonly/push-irdec) :enmem t))
  (build-vec-pkt idcode-iovec        0 :idcode/endblk t   t   :size-32 t   :dr #x03)
  (build-vec-pkt version-iovec       0 :plain         nil nil :size-8  nil :ir #xff)
  (build-vec-pkt debug-request-iovec 0 :plain         t   t   :size-8  nil :ir #x44)
  (build-vec-pkt debug-enable-iovec  0 :plain         t   t   :size-8  nil :ir #x55)
  (build-vec-pkt blkread-addr        0 :block-read    t   t   :size-32 t   :dr (bits :regsel :omar))
  (build-vec-pkt blkread-data        0 :block-read    t   t   :size-32 t   :dr (bits :regsel :omdr))
  (build-vec-pkt blkread-end         0 :idcode/endblk t   t   :size-32 t   :dr (bits :regsel :omdr))
  (build-vec-pkt blkwrite-addr       0 :block-write   t   t   :size-32 t   :dr (bits :regsel :omar))
  (build-vec-pkt blkwrite-data       0 :block-write   t   t   :size-32 t   :dr (bits (:regsel :rdonly/push-irdec) :omdr t))
  (build-vec-pkt blkwrite-end        0 :idcode/endblk t   t   :size-32 t   :dr (bits (:regsel :rdonly/push-irdec) :omdr t))
  (defun (setf ezusb-tap-ird) (val iface null &aux (handle (ezusb-interface-handle iface)))
    (declare (type (unsigned-byte 32) val) (ignore null))
    (let* ((bitlen (max +oncd-ird-length+ (integer-length val)))
           (bytelen (cond ((> bitlen 16) 5)
                          (t 3))))
      (without-gcing-and-interrupts
        (setf (aref tap-iovec 0) (bits (:ez-hdr-type :sys-nrst :sys-ntrst :size :ndebug-oper :irdrsel)
                                       :plain t t (cond ((> bitlen 16) :size-32)
                                                        (t :size-16))
                                       t :dr)
              (u8-vector-wordle tap-iovec 1) val)
        (ezusb-bulk-io handle :write tap-iovec (1+ bytelen))
        (when (test-bits val (:nstep :rdonly/push-irdec :modstep) nil t nil)
          (ezusb-bulk-io handle :read tap-iovec bytelen)))
      (u8-vector-wordle tap-iovec 0 bytelen)))

  (defun ezusb-tap-dr-io (o selector val &aux (handle (ezusb-interface-handle o)))
    (let ((bit-length (aref (aref (device-extensions o) selector) 1)))
      (multiple-value-bind (header size)
          (case bit-length
            (16 (values (bits (:ez-hdr-type :sys-nrst :sys-ntrst :size :ndebug-oper :irdrsel) :plain t t :size-16 t :dr) 4))
            (12 (values (bits (:ez-hdr-type :sys-nrst :sys-ntrst :size :ndebug-oper :irdrsel) :plain t t :size-12 t :dr) 4))
            (t (values (bits (:ez-hdr-type :sys-nrst :sys-ntrst :size :ndebug-oper :irdrsel) :plain t t :size-32 t :dr) 6)))
        (without-gcing-and-interrupts
          (setf (aref tap-iovec 0) header
                (aref tap-iovec 1) (logior selector (if val 0 #x40))
                (u8-vector-word32le tap-iovec 2) (or val 0))
          (ezusb-bulk-io handle :write tap-iovec size)
          (unless val
            (ezusb-bulk-io handle :read tap-iovec (if val 8 4))))
        (u8-vector-word32le tap-iovec 0))))

  (defun (setf ezusb-cmd) (val iface null)
    (declare (ignore null))
    (setf (aref tap-iovec 0) val)
    (ezusb-bulk-io (ezusb-interface-handle iface) :command tap-iovec 3))

  (defun ezusb-target-reset (iface &aux (handle (ezusb-interface-handle iface)))
    (without-gcing-and-interrupts
      (setf (aref input-iovec 0) 0)
      (ezusb-bulk-io handle :write reset-iovec 2)
      (ezusb-bulk-io handle :read input-iovec 8)))

  (defun ezusb-oncd-reset (iface &aux (handle (ezusb-interface-handle iface)))
    (without-gcing-and-interrupts
      (setf (aref input-iovec 0) 0)
      (ezusb-bulk-io handle :write reset-oncd-iovec 2)
      (ezusb-bulk-io handle :read input-iovec 8)))
    
  (defun ezusb-tap-idcode (iface null &aux (handle (ezusb-interface-handle iface)))
    (declare (ignore null))
    (without-gcing-and-interrupts
      (setf (aref input-iovec 0) 0)
      (ezusb-bulk-io handle :write idcode-iovec 6)
      (let ((retval (ezusb-bulk-io handle :read input-iovec 4)))
        (unless (= 4 retval)
          (interface-error iface "~@<Failed to get IDCODE of ~S: retval ~S~:@>" iface retval))))
    (u8-vector-word32le input-iovec 0))
    
  (defun ezusb-firmware-version (iface &aux (handle (ezusb-interface-handle iface)))
    (without-gcing-and-interrupts
      (setf (aref input-iovec 0) 0)
      (ezusb-bulk-io handle :write version-iovec 2)
      (let ((retval (ezusb-bulk-io handle :read input-iovec 2)))
        (unless (= 2 retval)
          (interface-error iface "~@<Failed to get VERSION of ~S: retval ~S~:@>" iface retval))))
    (u8-vector-word16be input-iovec 0))
  
  (defun ezusb-debug-request (iface &aux (handle (ezusb-interface-handle iface)))
    (without-gcing-and-interrupts
      (setf (aref input-iovec 0) 0)
      (ezusb-bulk-io handle :write debug-request-iovec 2)
      (let ((retval (ezusb-bulk-io handle :read input-iovec 2)))
        (unless (and (= 2 retval)
                     (= #x40 (logand #xf0 (aref input-iovec 0))))
          (interface-error iface "~@<Failed to issue DEBUG_REQUEST on ~S~:@>" iface))))
    t)
    
  (defun ezusb-debug-enable (iface &aux (handle (ezusb-interface-handle iface)))
    (without-gcing-and-interrupts
      (setf (aref input-iovec 0) 0)
      (ezusb-bulk-io handle :write debug-enable-iovec 2)
      (unless (and (= 2 (ezusb-bulk-io handle :read input-iovec 2))
                   (= #x50 (logand #xf0 (aref input-iovec 0))))
        (interface-error iface "~@<Failed to issue DEBUG_ENABLE on ~S~:@>" iface)))
    (u8-vector-word16le input-iovec 0)))

(defun ezusb-tap-dr (iface reg)
  (ezusb-tap-dr-io iface reg nil))

(defun (setf ezusb-tap-dr) (val iface reg)
  (ezusb-tap-dr-io iface reg val)
  val)

(defmethod bus-populate-address ((o ezusb-bus) address)
  (lret ((iface (make-instance 'ezusb-interface :bus o :address address :handle (ezusb-open address))))
    (setf (iface-idcode iface) (decode-bitfield :oncd-version (interface-reset iface)))))

(define-device-class ezusb-interface :interface (interface)
  ((handle :accessor ezusb-interface-handle :type (or null #-windows stream #+windows integer) :initarg :handle))
  (:layouts (:tap-ird nil (setf ezusb-tap-ird))        ;; wronly, not enforced
            (:tap-idcode ezusb-tap-idcode nil)
            (:tap-dr ezusb-tap-dr (setf ezusb-tap-dr)) ;; want pass
            (:ezusb-cmd nil (setf ezusb-cmd)))         ;; wronly, not enforced
  (:extended-layouts :tap-dr)
  (:default-initargs
   :handle nil))

(defmethod interface-reset ((o ezusb-interface))
  (with-condition-recourses interface-error
      (progn-1
        (setc (devbit o :ezusb-cmd :ezusb-opcode :write-only t) :program-pll-to-48mhz
              (devbit o :ezusb-cmd :ezusb-opcode :write-only t) :begin-reset
              (devbit o :ezusb-cmd :ezusb-opcode :write-only t) :end-reset)
        (ezusb-oncd-reset o)
        (devbit-value o :idcode :oncd-version)
        (setf (iface-version o) (ezusb-firmware-version o)))
    #+windows
    (:common (c)
      (call-next-recourse-and-retry)
      (error 'persistent-interface-error :interface o :error c))
    #+(or)
    (reset-ezusb ()
      (warn "~@<Warning: interface error during firmware reset, resetting the interface itself.~:@>")
      (ezusb-reset-ezusb (ezusb-interface-handle o)))))

(defmethod interface-stop-target ((o ezusb-interface))
  (ezusb-debug-request o))

(defmethod interface-attach-target ((o ezusb-interface))
  (ezusb-debug-enable o))

(defmethod interface-reset-target ((o ezusb-interface) stop-cores-p)
  (declare (ignore stop-cores-p))
  (ezusb-target-reset o)
  (interface-stop-target o)
  (interface-attach-target o))

(defmethod interface-bus-word ((o ezusb-interface) address)
  "Read 32 bits from a given bus address."
  (declare (type (unsigned-byte 32) address))
  (lret (result)
    (setc (devbits o :oscr (:slctmem :ro)) (t t)
          (devreg o :omar) address
          (devreg o :mem) 0)
    (busywait (test-devbits o :oscr :rdym)
              ((error 'interface-memory-timeout :interface o))
              :iteration-period +memory-delay+ :timeout 300)
    (setc result (devreg o :omdr)
          (devbits o :oscr (:slctmem :ro)) (nil nil))))

(defmethod (setf interface-bus-word) (val (o ezusb-interface) address)
  "Write 32 bits into a given bus address."
  (declare (type (unsigned-byte 32) val address))
  (setc (devbit o :oscr :slctmem) t
        (devreg o :omar) address
        (devreg o :omdr) val
        (devreg o :mem) 0)
  (busywait (test-devbits o :oscr :rdym)
            ((error "RDYM!"))
            :iteration-period +memory-delay+ :timeout 300)
  (setc (devbit o :oscr :slctmem) nil)
  val)

(defparameter *iovec* (make-array 4000 :element-type '(unsigned-byte 8)))

(defmethod interface-bus-io ((o ezusb-interface) buffer address size direction &optional (offset 0))
  (let ((handle (ezusb-interface-handle o))
        (chunk-word-size 40)
        (bulk-chunk-size 9)
        (words (ash size -2))
        (inbase offset)
        (bufofft offset))
    (with-pinned-objects (buffer)
      (multiple-value-bind (chunks tail-chunk-words) (floor words chunk-word-size)
        (multiple-value-bind (bulks tail-bulk-chunks) (floor chunks bulk-chunk-size)
          (labels ((build-chunk (bulkoutbase words &aux (intrachunk 0))
                     (funcall (if (iface-fastio o) #'build-vec-block-pkt #'build-vec-fastword-pkt)
                              *iovec* bulkoutbase (if (eq direction :read) :read-addr :write-addr) address)
                     (incf bulkoutbase 6)
                     ;; (syncformat t "~&bulk formation, io start #x~X, buffer ~X~%" address bufofft)
                     (iter (for i from 0 below (1- words))
                           (funcall (if (iface-fastio o) #'build-vec-block-pkt #'build-vec-fastword-pkt)
                                    *iovec* (+ bulkoutbase intrachunk)
                                    (if (eq direction :read) :read-data :write-data)
                                    (if (eq direction :read) 0 (u8-vector-word32le buffer bufofft)))
                           (incf bufofft 4)
                           (incf intrachunk 6))
                     (funcall (if (iface-fastio o) #'build-vec-block-pkt #'build-vec-fastword-pkt)
                              *iovec* (+ bulkoutbase intrachunk)
                              (if (eq direction :read) :read-blkend :write-blkend)
                              (if (eq direction :read) 0 (u8-vector-word32le buffer bufofft)))
                     
                     (incf bufofft 4)
                     (incf address (* words 4)))
                   (perform-bulk (inbase chunks chunk-words &aux (chunk-pktbytes (* (1+ chunk-words) 6)) (chunk-databytes (ash chunk-words 2)))
                     ;; (when (eq direction :read)
                     ;;   (syncformat t "bulk read, inbase #x~X~%" inbase))
                     (iter (repeat chunks)
                           (for outbase upfrom 0 by chunk-pktbytes)
                           (build-chunk outbase chunk-words))
                     ;; (syncformat t "~&bulk write, #x~X bytes ~X chunks ~X pktbytes~%" (* chunks chunk-pktbytes) chunks chunk-pktbytes)
                     (without-gcing-and-interrupts
                       (ezusb-bulk-io handle :write *iovec* (* chunks chunk-pktbytes))
                       (when (eq direction :read)
                         (iter (repeat chunks)
                               (for inbase-sub upfrom inbase by chunk-databytes)
                               ;; (syncformat t "bulk read, #x~X bytes, offt #x~X~%" chunk-databytes inbase-sub)
                               (ezusb-bulk-io handle :read buffer chunk-databytes inbase-sub))))))
            ;; (syncformat t "~&parameters: bulks: ~X, bulk size: ~X, chunk size: ~X, tail bulk: ~X, tail chunk: ~X~%"
            ;;             bulks
            ;;             (* 4 bulk-chunk-size chunk-word-size) (* 4 chunk-word-size)
            ;;             (* 4 tail-bulk-chunks chunk-word-size) (* 4 tail-chunk-words))
            (let ((saved-oscr (devreg o :oscr)))
              (iter (repeat bulks)
                    (perform-bulk inbase bulk-chunk-size chunk-word-size)
                    (incf inbase (* 4 bulk-chunk-size chunk-word-size)))
              ;; (syncformat t "past main phase: ~X~%" inbase)
              (when (plusp tail-bulk-chunks)
                (perform-bulk inbase tail-bulk-chunks chunk-word-size)
                (incf inbase (* tail-bulk-chunks chunk-word-size 4)))
              ;; (syncformat t "past tail bulk phase: ~X~%" inbase)
              (cond ((= 1 tail-chunk-words)
                     (case direction
                       ;; inbase and bufofft grow at the same rate, but in different rhythms
                       (:read (setf (u8-vector-word32le buffer inbase) (interface-bus-word o address)))
                       (:write (setf (interface-bus-word o address) (u8-vector-word32le buffer bufofft)))))
                    ((plusp tail-chunk-words)
                     (perform-bulk inbase 1 tail-chunk-words)))
              (setc (devreg o :oscr) saved-oscr))))))))

(defmethod interface-close ((o ezusb-interface))
  (when (null (ezusb-interface-handle o))
    (interface-error o "~@<Attempt to shut down interface an already deactivated interface ~A.~:@>" o))
  (ezusb-close o)
  (setf (ezusb-interface-handle o) nil))

(defmethod bus-remove ((bus interface-bus) (o ezusb-interface))
  (interface-close o))
