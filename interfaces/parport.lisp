;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: INTERFACE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :interface-parport)


(define-namespace :epp
  (:documentation "EPP-mode parallel port namespace")
  (:register-formats
   (:ecp-ecr ""
    (:ecr-mode          3 5 "ECR mode"
      ((0       :spp    "SPP")
       (1       :ps2    "PS/2")
       (2       :ppf    "PPF")
       (3       :ecp    "ECP")
       (4       :epp    "EPP")
       (5       :vnd    "VND")
       (6       :tst    "TST")
       (7       :cfg    "CFG"))))
   (:spp-control ""
    (:strobe            1 0 "pin 1  - Strobe, hw inv")
    (:autolinefeed      1 1 "pin 14 - Auto LineFeed, hw inv")
    (:reset             1 2 "pin 16 - Initialize Printer")
    (:select            1 3 "pin 17 - Select Printer, hw inv")
    (:irqenable         1 4 "IRQ Enable")
    (:bidirectional     1 5 "Enable Bi-directional mode"))
   (:spp-status ""
    (:epp-tmout         1 0 "EPP TimeOut, only for EPP mode")
    (:nirq              1 2 "0 - IRQ occured")
    (:nerror            1 3 "pin 15 - Error")
    (:nselect           1 4 "pin 13 - Select In")
    (:pout              1 5 "pin 12 - Out of Paper")
    (:nack              1 6 "pin 10 - Ack")
    (:busy              1 7 "pin 11 - Busy, hw inv")))
  (:layouts
   ((:ioport-address    "PC IO ports")
    (:spp-data          0       :doc "SPP data")
    (:spp-status        1       :format :spp-status :doc "SPP status")
    (:spp-control       2       :format :spp-control :doc "SPP control")
    (:epp-addr          3       :doc "EPP address")
    (:epp-data          4       :doc "EPP data")
    (:epp-ecr           5       :doc "EPP extended control register")
    (:ecp-config-a      #x400   :doc "ECP config A")
    (:ecp-config-b      #x401   :doc "ECP config B")
    (:ecp-ecr           #x402   :format :ecp-ecr :doc "ECP Extended Control Register"))))

(set-namespace :epp :interface)

(defconstant lpt1-base #x378)
(defconstant lpt2-base #x278)
(defconstant lpt3-base #xbc0)
(defconstant lpt4-base #xb80)

;; Delay values, in nanoseconds.
(defconstant +reset-delay+ 1000000)
(defconstant +generic-delay+ 10000)

(defparameter *epp-reset-delay* 500000)
(defvar *log-graft-access* nil)

(define-condition parport-error (error)
  ((port :initarg :port)))

(define-condition insufficient-privileges-condition (error) ())

(define-reported-condition parport-insufficient-privileges-error (parport-error insufficient-privileges-condition) ()
  (:report () "Insufficient privileges to claim access to parallel port."))

(define-reported-condition parport-reset-error (parport-error) ()
  (:report (port) "~S reset failed: SPP BUSY never came low." port))

(define-reported-condition parport-busy-error (parport-error) ()
  (:report (port) "~S busy." port))

(define-reported-condition parport-operation-timeout-error (parport-error) ()
  (:report (port) "~S operation timeout." port))

;;;;
;;;; Port permissions & IO
;;;;

#+ecl
(defun inb (port)
  (ffi:c-inline (port) (:unsigned-short) (values :unsigned-byte)
    #+(or linux mingw32) "{
    unsigned short port = #0;
    unsigned char val;
    asm(\"inb %w1, %b0\": \"=a\"(val): \"d\"(port));
    @(return 0)= val;
    }"
    #+msvc "{
    unsigned short port = #0;
    unsigned char val;
    __asm {
            mov dx, port
            in al, dx
            mov val, al
    }
    @(return 0)= val;
    }"
   :side-effects t))

#+ecl
(defun outb (val port)
  (ffi:c-inline (val port) (:unsigned-byte :unsigned-short) (values)
    #+(or linux mingw32) "{
    unsigned char val = #0;
    unsigned short port = #1;
    asm(\"outb %b0,%w1\":: \"a\"(val), \"d\"(port));
    }"
    #+msvc "{
    unsigned char val = #0;
    unsigned short port = #1;
    __asm {
            mov dx, port
            mov al, val
            out dx, al
    }
    }"
   :side-effects t))

#+sbcl (declaim (ftype (function ((unsigned-byte 16)) (values (unsigned-byte 8))) inb) (inline inb)) #+sbcl
(defun inb (addr)
  (declare (optimize speed (safety 0) (debug 0)))
  (sb-vm::port-byte addr))

#+sbcl (declaim (ftype (function ((unsigned-byte 8) (unsigned-byte 16)) (values (unsigned-byte 8))) outb) (inline outb)) #+sbcl
(defun outb (val addr)
  (declare (optimize speed (safety 0) (debug 0)))
  (sb-vm::%set-port-byte addr val))

#-(or ecl sbcl (and ccl windows))
(defun outb (val addr)
  (declare (ignore val addr))
  (error 'not-implemented :name 'outb))

#-(or ecl sbcl (and ccl windows))
(defun inb (addr)
  (declare (ignore addr))
  (error 'not-implemented :name 'inb))

#+(and (not windows) (or ecl sbcl ccl))
(cffi:defcfun iopl :int (uid :int))

#+windows
(progn
  (defun bind-to-parport-access-library ()
    #+ecl
    (cffi:use-foreign-library model3-comdb-gate)
    #+ccl
    (cffi:use-foreign-library libportaccess)
    #+(or ecl)
    (cffi:defcfun "get_winnt_lpt_access" :int)
    #+(or)
    (progn
      (cffi:defcfun "inb" :unsigned-char (port :unsigned-short))
      (cffi:defcfun "outb" :void (value :unsigned-char) (port :unsigned-short))))
  #+ecl
  (progn
    (cffi:define-foreign-library model3-comdb-gate
      (t (:or "MCDevice/model3-comdb-gate.dll" "model3-comdb-gate.dll")))
    (cffi:defcfun "get_winnt_lpt_access" :int))
  #+sbcl
  (progn
    (cffi:define-foreign-library libportaccess
      (t (:default "libportaccess")))
    (cffi:use-foreign-library libportaccess)
    (cffi:defcfun "get_winnt_lpt_access" :int))
  #+ccl
  (progn
    (cffi:define-foreign-library libportaccess
      (t (:default "libportaccess")))
    (cffi:use-foreign-library libportaccess)
    (cffi:defcfun "get_winnt_lpt_access" :int)
    (cffi:defcfun "inb" :unsigned-char (port :unsigned-short))
    (cffi:defcfun "outb" :void (value :unsigned-char) (port :unsigned-short))))


;;; XXX: should pass *verbose-interface-init* to G-W-L-A
(defun set-epp-port-perm ()
  (let ((status (case #+windows (get-winnt-lpt-access)
                      #-windows (iopl 3)
                      (0 nil)
                      (-1 "EACCESS")
                      (-2 "EBADSVCNAME")
                      (-3 "ESVCINSTALL")
                      (-4 "ESVCACCESS")
                      (-5 "ESVCPURGEERROR")
                      (-6 "EUNKNOWN")
                      (-7 "ESVCSTARTINVALIDFN")
                      (-8 "ESVCSTARTNOSVCFILE")
                      (t "EREALLYUNKNOWN"))))
    (when status
      (format t "SET-EPP-PORT-PERM: ~A() returned ~A.~%" #+windows "get_winnt_lpt_access" #-windows "iopl" status))
    (null status)))

;;;;
;;;; Bit-banging
;;;;
(defmacro with-graft-io-capture (&body body)
  `(with-iolog-capture *log-stream*
     (let ((*log-graft-access* t))
       ,@body)))

(defmacro port (base register)
  `(%port ,base ,(register-selector (space :epp) :ioport-address register)))

(defmacro port-bit (base register bit &rest rest)
  `(place-bit (port ,base ,register) ,register ,bit ,@rest))

(define-setc-expander port-bit (value base register bit &rest rest)
  (decode-context (space-name) nil `(,bit)
    `(setc (place-bit (port ,base ,register) ,register ,bit ,@rest) ,value)))

(defmacro port-bits (base register (&rest bits) &rest rest)
  `(place-bits (port ,base ,register) ,register (,@bits) ,@rest))

(define-setc-expander port-bits (values base register (&rest bits) &rest rest)
  (decode-context (space-name) nil bits
    `(setc (place-bits (port ,base ,register) ,register ,bits ,@rest) ,values)))

(declaim (ftype (function ((unsigned-byte 16) (unsigned-byte 16)) (values (unsigned-byte 8))) %port)
         (inline %port))
(defun %port (base addr)
  (declare (type (unsigned-byte 16) base addr) (optimize speed (safety 0) (debug 0)))
  (inb (+ base addr)))

(declaim #-ecl (ftype (function ((unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 16)) (unsigned-byte 8)) (setf %port))
         (inline (setf %port)))
(defun (setf %port) (val base addr)
  (declare (type (unsigned-byte 8) val) (type (unsigned-byte 16) base addr) (optimize speed (safety 0) (debug 0)))
  (outb val (+ base addr))
  val)

(declaim (inline set-port-forward) (ftype (function ((unsigned-byte 16)) (values)) set-port-reverse))
(defun set-port-forward (base)
  (declare (type (unsigned-byte 16) base) (optimize speed (safety 0) (debug 0)))
  (setc (port-bit base :spp-control :bidirectional) nil)
  (values))

(declaim (inline set-port-reverse) (ftype (function ((unsigned-byte 16)) (values)) set-port-reverse))
(defun set-port-reverse (base)
  (declare (type (unsigned-byte 16) base) (optimize speed (safety 0) (debug 0)))
  (setc (port-bit base :spp-control :bidirectional) t)
  (values))

(defun clear-epp-timeout (base)
  (declare (type (unsigned-byte 16) base))
  (or (not (port-bit base :spp-status :epp-tmout))
      (progn
        (port base :spp-status)
        (port base :spp-status) ; To clear tmout some chips require dbl read
        (setc (port-bit base :spp-status :epp-tmout :write-only t) t ; Some reset by writing 1
              (port-bit base :spp-status :epp-tmout :write-only t) nil) ; Some by writing 0
        (not (port-bit base :spp-status :epp-tmout)))))

;;;;
;;;; Higher-level bit-banging
;;;;
(defun port-reset (base)
  (declare (type (unsigned-byte 16) base))
  (setc (values) (port base :spp-control)
        (port-bit base :spp-control :reset :write-only t) t
        (port-bit base :spp-control :reset :write-only t) nil)
  (nanosleep *epp-reset-delay*)
  (setc (port-bits base :spp-control (:reset :bidirectional) :write-only t) (t t))
  (unless (clear-epp-timeout base)
    (error 'parport-operation-timeout-error :port base))
  (loop
     (tagbody
      :retry
      (restart-bind ((retry (lambda () (go :retry))
                      :report-function (lambda (s) (format s "~@<Retry resetting EPP.~:@>"))))
        (unless (port-bit base :spp-status :busy)
          (error 'parport-reset-error :port base))
        (return))))
  (set-port-forward base)
  t)

;; XXX: cached operation? (we have redundant reads here)
(defun port-status (base)
  (declare (type (unsigned-byte 16) base))
  (set-port-reverse base)
  (lret ((status (port base :spp-status)))
    (cond ((port-bit base :spp-status :epp-tmout)
           (error 'parport-operation-timeout-error :port base))
          #+nil (setc (devreg port :spp-status) status) ;; clear timeout
          ((not (port-bit base :spp-status :busy))
           (error 'parport-busy-error :port base))
          #+nil ((test-bits-set (spp-status nirq) status)
                 (error "EPP: nIRQ~%")))
    (set-port-forward base)))

(declaim (inline read-octets))
(defun read-octets (array count base &key (offset 0))
  (declare (type (simple-array (unsigned-byte 8)) array) (type (unsigned-byte 8) count)
           (type (unsigned-byte 16) base) (type (unsigned-byte 27) offset) (optimize speed (safety 0) (debug 0)))
  ;;   (format t "EPP: ~:[writ~;read~]ing ~D octets from/to ~S~%" t count array)
  (set-port-reverse base)
  (dotimes (i count)
    (declare (type (unsigned-byte 16) i))
    (setf (aref array (+ i offset)) (port base :epp-data)))
  (set-port-forward base)
  (values))

(declaim (inline write-octets))
(defun write-octets (array count base &key (offset 0))
  (declare (type (simple-array (unsigned-byte 8)) array) (type (unsigned-byte 8) count)
           (type (unsigned-byte 16) base) (type (unsigned-byte 27) offset) (optimize speed (safety 0) (debug 0)))
  ;;   (format t "EPP: ~:[writ~;read~]ing ~D octets from/to ~S~%" t count array)
  (dotimes (i count)
    (declare (type (unsigned-byte 16) i))
    (setf (port base :epp-data) (aref array (+ (the fixnum i) offset))))
  (values))

;;;
;;; Dongle-specific
;;;
(declaim (inline get-status) (ftype (function ((unsigned-byte 16)) (values (unsigned-byte 8))) get-status))
(defun get-status (base)
  (declare (type (unsigned-byte 16) base) (optimize speed (safety 0) (debug 0)))
  (set-port-reverse base)
  (prog1 (port base :epp-addr)
    (set-port-forward base)))

(defmacro status-bit (base bit)
  `(bit-value (get-status ,base) ,bit))

(defmacro send-command (base command)
  `(setc (place-bit (port ,base :epp-addr) :parport-cmd :parport-opcode :write-only t) ,command))

(defmacro with-reset-bit-twitch (reset-bit port-base &body body)
  "FWD, ,@BODY, FWD"
  (once-only (port-base)
    (let ((bitno (ecase reset-bit
                   (:trst 1)
                   (:sysrst 0))))
      `(unwind-protect (progn (send-command ,port-base :reset-preread)
                              (setc (port ,port-base :epp-data) (logxor #xff (ash 1 ,bitno)))
                              ,@body)
         (send-command ,port-base :reset-preread)
         (setc (port ,port-base :epp-data) #xff)))))

(declaim (inline move-bits))
(defun move-bits (array port-base bitcount dir lie)
  "write (1+ (ash bitcount -3)) bytes into epp-data, prepending them with the bitcount"
  (declare (type (simple-array (unsigned-byte 8)) array) (type (unsigned-byte 16) port-base) (type (unsigned-byte 6) bitcount)
           (type (member :forward :reverse) dir) (type (unsigned-byte 1) lie) 
           (optimize speed (safety 0) (debug 0)))
  (case dir
    (:forward (setc (port port-base :epp-data) (+ bitcount lie))
              (dotimes (i (ceiling bitcount 8))
                (setc (port port-base :epp-data) (aref array i))))
    (:reverse (set-port-reverse port-base)
              (dotimes (i (ceiling bitcount 8))
                (setc (aref array i) (port port-base :epp-data)))
              (set-port-forward port-base)))
  (values))
 
 (define-setc-expander iface-bits (dir array base bitcount &optional lie)
  `(move-bits ,array ,base ,bitcount ,dir ,(or lie 0)))

;;;;
;;;; TAP IR/DR I/O
;;;;
(define-setc-expander state-transition ((command pollstate) port-base interface)
  `(progn
     (send-command ,port-base ,command)
     (busywait (test-bits (get-status ,port-base) :tap-state ,pollstate)
               ((error 'interface-state-transition-timeout
                       :interface ,interface :command ,command :destination-state ,pollstate))
               :iteration-period +generic-delay+)))

(defun tap-shift-dr-path (array iface bits lie &aux (port-base (parport-interface-epp-base iface)))
  (declare (type (simple-array (unsigned-byte 8)) array) (type (unsigned-byte 32) bits)
           (type (unsigned-byte 16) port-base) (type (unsigned-byte 1) lie) 
           (optimize (debug 0) (safety 0)))
  (setc (state-transition port-base iface) (:rti-capturedr-pausedr :pausedr)
        (values) (send-command port-base :prewrite)
        (iface-bits array port-base bits lie) :forward
        (state-transition port-base iface) (:pausedr-shiftdr-pausedr :pausedr)
        (iface-bits array port-base bits) :reverse
        (state-transition port-base iface) (:pausedr-updatedr-rti :rti))
  (values))

(defun tap-write-dr-vector (array port-base bits)
  (declare (type (simple-array (unsigned-byte 8) (5)) array) (type (unsigned-byte 16) port-base) (type (unsigned-byte 32) bits))
  (setc (values) (send-command port-base :prewrite)
        (iface-bits array port-base bits 1) :forward
        (state-transition port-base nil) (:writedr :rti)
        (values) (send-command port-base :reset-preread)
        (iface-bits array port-base bits) :reverse)
  (values))

(defun tap-write-ir-vector (array port-base bits)
  (declare (type (simple-array (unsigned-byte 8)) array) (type (unsigned-byte 16) port-base) (type (unsigned-byte 32) bits))
  (setc (values) (send-command port-base :prewrite)
        (iface-bits array port-base bits 0) :forward
        (state-transition port-base nil) (:writeir :rti)
        (values) (send-command port-base :reset-preread)
        (iface-bits array port-base bits) :reverse)
  (values))

(defun (setf tap-ir) (insn iface null &aux
                      (port-base (parport-interface-epp-base iface)))
  (declare (type unsigned-byte insn) (ignore null) (type (unsigned-byte 16) port-base))
  (let ((ioarr (make-array 1 :element-type '(unsigned-byte 8) :initial-element insn)))
    (tap-write-ir-vector ioarr port-base +oncd-ir-length+)
    (setc (iface-ircache iface) insn)
    (aref ioarr 0)))

(let ((ioarr (make-array 5 :element-type '(unsigned-byte 8))))
  (defmethod tap-write-dr-register ((o parport-interface) selector val read-only-p)
    (declare (fixnum selector) (type (unsigned-byte 32) val) (boolean read-only-p))
    (with-writing-oncd-register-value (irdval reglen) (o selector read-only-p)
      (setf (aref ioarr 0) irdval
            (u8-vector-word32le ioarr 1) value)
      (tap-write-dr-vector vector (parport-interface-epp-base o) reglen)
      (u8-vector-word32le ioarr 1))))

(let ((ioarr (make-array 5 :element-type '(unsigned-byte 8))))
  (defun (setf parport-tap-ird) (val iface null &aux
                                 (port-base (parport-interface-epp-base iface)))
    "Used in interface-reset, interface-reset-target, (setf core-running-p),
step-core-asynchronous and exec-raw."
    (declare (type (unsigned-byte 32) val) (type (unsigned-byte 16) port-base) (ignore null))
    (let* ((bitlen (max +oncd-ird-length+ (integer-length val)))
           (bytelen (ceiling bitlen 8)))
      (setf (u8-vector-wordle ioarr 0) val)
      (tap-write-dr-vector ioarr port-base bitlen)
      (u8-vector-wordle ioarr 0 bytelen))))

(defun parport-tap-dr (iface reg)
  (tap-write-dr-register iface reg 0 t))

(defun (setf parport-tap-dr) (val iface reg)
  (tap-write-dr-register iface reg val nil)
  val)

;;;;
;;;; IDCODE
;;;;
(defun parport-read-idcode (iface)
  (let ((retarr (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
    (tap-shift-dr-path retarr iface +idcode-length+ 0)
    (u8-vector-word32le retarr 0)))

(defun parport-tap-idcode (interface selector &aux (port-base (parport-interface-epp-base interface)))
  "Get device's ID and lookup device table.
 
   Unfortunately JTAG specification defines
   Identification Register as optional register.
   This makes impossible to write reliable chip
   autodetection.
 
   Identification Register Format:
   u_int32_t    id_1:1,
                id_vendor:11,
                id_partno:16,
                id_ver:4;
 
   I assume:
        IDCODE length = IR length = 4 bit (it can be detected)
        IDCODE value = 0xE      (Valid for ARM)"
  (declare (ignore selector) (type interface interface) (type (unsigned-byte 16) port-base))
  ;;  After exiting TLR state DR path becomes Identification Register,
  ;;  or BYPASS register if IDCODE unimplemented.
  ;;  first bit at TDO: BYPASS 0, IDCODE 1
  (send-command port-base :finish)
  (send-command port-base :start)
  (parport-read-idcode interface))

;;;;
;;;; Interface bus
;;;;
(define-device-class parport-bus :empty (root-bus probe-discovery-bus interface-bus)
    ()
  (:default-initargs :probe-address-set '(0 1 2 3)))

(defmethod initialize-instance :after ((o parport-bus) &key &allow-other-keys)
  (unless (set-epp-port-perm)
    (error 'parport-insufficient-privileges-error)))

(defun bus-address-epp-base (addr)
  (ecase addr
    (0 lpt1-base)
    (1 lpt2-base)
    (2 lpt3-base)
    (3 lpt4-base)))

(defmethod bus-probe-address ((o parport-bus) address)
  (let ((interface-id (status-bit (bus-address-epp-base address) :idmask)))
    (member interface-id '(1 2))))

(defmethod bus-populate-address ((o parport-bus) address &aux (epp-base (bus-address-epp-base address)))
  (syncformat t "NOTE: opening an EPP device on port #x~X~%" epp-base)
  (lret ((iface (make-instance 'parport-interface :bus o :address address :epp-base epp-base :version (status-bit epp-base :idmask))))
    (setf (iface-idcode iface)
          (decode-bitfield :oncd-version
                           (with-condition-recourses interface-state-transition-timeout
                               (interface-reset iface)
                             (:common (c recourse)
                                      (syncformat t "~&WARNING: OFP: ~S, failed to get IDCODE of ~S. Attempting workaround: ~A~%"
                                                  (oncd-functional-p iface) iface recourse)
                                      (call-next-recourse-and-retry)
                                      (error c))
                             (target-reset-and-stop ()
                                                    (interface-reset-target iface t)))))
    (interface-reset iface)))

;;;;
;;;; Interface
;;;;
(define-device-class parport-interface :interface (elvees-interface)
  ((epp-base :accessor parport-interface-epp-base :type (unsigned-byte 32) :initarg :epp-base)
   (ircache :accessor iface-ircache :initform :undefined :type (or (member :undefined) (unsigned-byte 4))))
  (:layouts (:tap-ir nil (setf tap-ir))                     ;; wronly, not enforced
            (:tap-ird nil (setf parport-tap-ird))           ;; wronly, not enforced
            (:tap-idcode parport-tap-idcode nil)
            (:tap-dr parport-tap-dr (setf parport-tap-dr))) ;; want pass)
  (:extended-layouts :tap-dr))

(defmethod interface-reset ((i parport-interface) &aux (port-base (parport-interface-epp-base i)))
  (port-reset port-base)
  (send-command port-base :start)
  (prog1 (place-bit-value (parport-read-idcode i) :idcode :oncd-version)
    (with-reset-bit-twitch :trst port-base
      (nanosleep +generic-delay+))))

(defun oncd-functional-p (interface)
  "Sniff OnCD, so as to avoid excessive resetting."
  (numberp (ignore-errors (= (setc (devreg interface :omar) #xf0add00d)
                             (devreg interface :omar)))))

(defmethod interface-stop-target ((o parport-interface))
  (setc (devbit o :ir :tap-opcode :write-only t) :debug-request))

(defmethod interface-attach-target ((o parport-interface))
  (busywait (= (bits :tap-opcode :debug-enable)
               (setc (devbit o :ir :tap-opcode :write-only t) :debug-enable))
            ((error 'interface-debug-quiescence-timeout :interface o))
            :timeout 10 :iteration-period +generic-delay+))

(defmethod interface-reset-target ((i parport-interface) stop-cores-p &aux (port-base (parport-interface-epp-base i)))
  (with-condition-recourses interface-error
      (with-reset-bit-twitch :sysrst port-base
        (nanosleep +reset-delay+)
        (if stop-cores-p
            (progn (interface-stop-target i)
                   (interface-attach-target i))
            (devreg i :oscr)))
    (:common (c)
             (call-next-recourse-and-retry)
             (error 'persistent-interface-error :interface i :error c))
    (reset-epp ()
               (port-reset port-base)
               (send-command port-base :start)))
  (when stop-cores-p
    (setc (devbits i :oscr (:trace-mode :erase :ime :rdym :slctmem)) (nil nil nil nil nil)))
  t)

;;;
;;; Interface bus I/O
;;;
(defmethod interface-bus-io ((o parport-interface) buffer address size direction &optional (offset 0) &aux (port-base (parport-interface-epp-base o)))
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type (unsigned-byte 32) address)
           (type (unsigned-byte 28) size)
           (type (member :read :write) direction))
  (setc (devbits o :oscr (:slctmem :ro)) (t (eq direction :read)))
  (unwind-protect
       (progn
         (setc (devreg o :omar) address
               (values) (send-command port-base :reset-preread)
               (port port-base :epp-data) (ecase direction (:read #x2f) (:write #x1f)))
         (unwind-protect
              (loop :for size :from size :above 3 :by 4
                 :for buf-offset :upfrom offset :by 4
                 :do (loop :until (plusp (status-bit port-base :blkio)))
                 (if (eq direction :read)
                     (read-octets buffer 4 port-base :offset buf-offset)
                     (write-octets buffer 4 port-base :offset buf-offset))
                 :finally
                 (busywait (plusp (status-bit port-base :blkio))
                           ((error 'interface-status-timeout
                                   :interface o :operation 'tap-block-io-completion
                                   :timeout (* 1000 +generic-delay+))) :iteration-period +generic-delay+))
           (setc (values) (send-command port-base :reset-preread)
                 (port port-base :epp-data) #x3f)))
    (setc (devbits o :oscr (:slctmem :ro)) (nil nil))))

;; int get_ir_len(void)
;; {
;;      u_int32_t pattern = 0x5e2a3b4c, data = 0;
;;      u_int tdo, i;
;;      u_int max = 33;         /* limit for our search of IR length */

;;      change_tap_state(SHIFT_IR);
;;      /* Shift pattern into IR */
;;      for (i = 0; i < NBBY * sizeof(pattern); i++) {
;;              data <<= 1;
;;              data |= get_tdo();
;;              set_tdi(pattern & (1 << (NBBY * sizeof(pattern) - 1 - i)));
;;              do_tick();
;;      }

;;      set_tdi(1);             /* Shift in BYPASS command */

;;      /* Test for pattern */
;;      for (i = 1; i != max; i++) {
;;              tdo = data >> (NBBY * sizeof(data) - 1);
;;              data <<= 1;
;;              data |= get_tdo();
;;              if (data == pattern)
;;                      break;
;;              do_tick();
;;      }
;;      change_tap_state(RTI);

;;      if (i == max)
;;              i = -1;

;;      return (i);
;; }

;; int get_dr_len(void)
;; {
;;      u_int32_t pattern = 0x5e2a3b4c, data = 0;
;;      u_int tdo, i;
;;      u_int max = 1024;       /* limit for our search of DR length */

;;      change_tap_state(SHIFT_DR);
;;      /* Shift pattern into DR */
;;      for (i = 0; i < NBBY * sizeof(pattern); i++) {
;;              data <<= 1;
;;              data |= get_tdo();
;;              set_tdi(pattern & (1 << (NBBY * sizeof(pattern) - 1 - i)));
;;              do_tick();
;;      }

;;      /* Restore shifted data and test for pattern */
;;      for (i = 1; i != max; i++) {
;;              tdo = data >> (NBBY * sizeof(data) - 1);
;;              data <<= 1;
;;              data |= get_tdo();
;;              /* Shift in the same data which was at TDO */
;;              set_tdi(tdo);
;;              if (data == pattern)
;;                      break;
;;              do_tick();
;;      }
;;      change_tap_state(RTI);

;;      if (i == max)
;;              i = -1;

;;      return (i);
;; }

;;   (test:deftest test-bypass (target)
;;     (let ((iface (parport-interface-epp-base target)))
;;       (setf (devbit target :ir :tap-opcode) :bypass)
;;       (let ((result (
