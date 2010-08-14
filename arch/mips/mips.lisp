;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MIPS; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2010, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2007-2010 by
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

(in-package :mips)


(extend-namespace :core
  (:register-formats
   (:entrylo ""
     (:global           1 0 "page global")
     (:valid            1 1 "page valid")
     (:dirty            1 2 "page dirty")
     (:cache-mode       3 3 "cache mode"
       ((#b010  :uncached       "uncached")
        (#b011  :cacheable      "cacheable noncoherent")))
     (:addr             20 6 "address part"))
   (:entryhi ""
     (:asid             8 0 "AS Id")
     (:vpn2            19 13 "Virtual Page Number"))
   (:status ""
     (:ie               1 0 "Interrupts Enabled")
     (:exl              1 1 "Exception Level")
     (:erl              1 2 "Error Level")
     (:um               1 4 "User mode")
     (:ux               1 5 "64bit user virtual address space enable")
     (:sx               1 6 "64bit supervisor virtual address space enable")
     (:kx               1 7 "64bit kernel virtual address space enable")
     (:sw0              1 8 "Software interrupt 0")
     (:sw1              1 9 "Software interrupt 1")
     (:irq0             1 10 "Hardware interrupt 0")
     (:irq1             1 11 "Hardware interrupt 1")
     (:irq2             1 12 "Hardware interrupt 2")
     (:irq3             1 13 "Hardware interrupt 3")
     (:irq4             1 14 "Hardware interrupt 4")
     (:irq5             1 15 "Hardware interrupt 5")
     (:nmi              1 19 "Boot vector reached becaused of an NMI.")
     (:sr               1 20 "Boot vector reached becaused of soft reset.")
     (:ts               1 21 "TLB shutdown")
     (:bev              1 22 "Boot Exception Vectors")
     (:px               1 23 "Enable 64-bit operations")
     (:mx               1 24 "MDMX enable")
     (:re               1 25 "Reverse Endianness")
     (:rp               1 27 "Reduced Power")
     (:cu0              1 28 "Cop0 Usable")
     (:cu1              1 29 "Cop1 Usable"))
   (:cause ""
     (:exccode          5 2 "Exception code"
        ((#x0   :interrupt      "")
         (#x1   :tlb-mod        "TLB modification")
         (#x2   :tlb-load-miss  "")
         (#x3   :tlb-store-miss "")
         (#x4   :adel-load      "Misaligned load")
         (#x5   :adel-store     "Misaligned store")
         (#x6   :insn-bus-error "")
         (#x7   :data-bus-error "")
         (#x8   :syscall        "")
         (#x9   :breakpoint     "")
         (#xa   :reserved-insn  "")
         (#xb   :cop-unusable   "")
         (#xc   :arith-overflow "")
         (#xd   :trap           "")
         (#xe   :vcei           "Virtual coherency error on icache")
         (#xf   :floating-point "")
         (#x10  :cop2-excpt     "")
         (#x17  :watch          "Bus access matched WatchLo/WatchHi")
         (#x1f  :vced           "Virtual coherency error on data")))
     (:p-sw0              1 8 "Pending software interrupt 0")
     (:p-sw1              1 9 "Pending software interrupt 1")
     (:p-irq0             1 10 "Pending hardware interrupt 0")
     (:p-irq1             1 11 "Pending hardware interrupt 1")
     (:p-irq2             1 12 "Pending hardware interrupt 2")
     (:p-irq3             1 13 "Pending hardware interrupt 3")
     (:p-irq4             1 14 "Pending hardware interrupt 4")
     (:p-irq5             1 15 "Pending hardware interrupt 5")
     (:iv                 1 23 "Move interrupt vector by #x80")
     (:epc-dslot-skew     1 31 "EPC skewed one instruction back."))
;;    (:config-r4k ""
;;      (:kseg0-cacheable       3 0 "Kseg0 cacheability")
;;      (:cu            1 3 "MP cache coherency, another")
;;      (:dcacheline    1 4 "Dcache cacheline: 1:256/0:128 bit")
;;      (:icacheline    1 5 "Icache cacheline: 1:256/0:128 bit")
;;      (:dcachesize    3 6 "Dcache size: 2^(12+<this>)")
;;      (:icachesize    3 9 "Icache size: 2^(12+<this>)")
;;      (:eb            1 13 "Must be 0. (cache refills/write backs in sequential order)")
;;      (:em            1 14 "Data checking mode: 1:ECC/0:parity")
;;      (:be            1 15 "Big Endianness")
;;      (:sm            1 16 "MP cache coherency")
;;      (:sc            1 17 "SC enable")
;;      (:ew            2 18 "System bus width: 1:32/0:64")
;;      (:sw            1 20 "SC width: 1:128/0:64")
;;      (:ss            1 21 "Split/Unified SC")
;;      (:sb            2 22 "Secondary cache Block size")
;;      (:ep            4 24 "Bus burst pattern")
;;      (:ec            3 28 "Clock divider")
;;      (:cm            1 31 "Master/Checker"))
   (:config-mc12 ""
     (:kseg0-cacheable  3 0 "Kseg0 cacheability"
       ((#b011  :cached "")
        (#b010  :uncached "")))
     (:mmu-mode         3 7 "MMU Mode"
       ((#b001  :tlb    "TLB")
        (#b010  :bat    "BAT")
        (#b011  :fm     "Fixed mapping")))
     (:version          3 10 "Version")
     (:arch             2 13 ""
       ((#b00   :mips32 "")
        (#b01   :mips64-compat32 "")
        (#b10   :mips64 "")))
     (:big-endian       1 15 "Big Endian mode")
     ;; (mc-specific
     (:burst-mode       1 16 "0: sequential, 1: ???")
     (:merge-mode       2 17 "32bit Collapsing write buffer"
       ((#b00   :no-merging "")))
     (:mult-div-unit    1 20 "Enable the iterative Multiplier/Divisor unit")
     (:kuseg-cacheable  3 25 "useg/kuseg cacheability in non-TLB mode."
       ((#b011  :cached "")
        (#b010  :uncached "")))
     (:kseg23-cacheable 3 28 ""
       ((#b011  :cached "")
        (#b010  :uncached "")))
     ;; )
     (:config1-present  1 31 ""))
   (:config1 ""
     (:fp               1 0 "FPU implemented")
     (:ejtag            1 1 "")
     (:ca               1 2 "Code compression: MIPS16")
     (:watch-reg-p      1 3 "")
     (:perfcount-reg-p  1 4 "")
     (:mdmx             1 5 "MDMX implemented")
     (:c2-p             1 6 "Co-processor 2 implemented")
     (:d$-associativity 3 7 "")
     (:d$-linesize      3 10 "")
     (:d$-sets          3 13 "")
     (:i$-associativity 3 16 "")
     (:i$-linesize      3 19 "")
     (:i$-sets          3 22 "")
     (:tlb-entries      6 25 "")
     (:config2-present  1 31 "Config2 register present")))
  (:layouts
   ((:cop0      "MIPS Cop0 registers")
    (:index     0 :doc "TLB entry selector")
    (:random    1 :doc "Lower bound of the TLB entry extent modified by TLBWR")
    (:entrylo0  2 :format :entrylo)
    (:entrylo1  3 :format :entrylo)
    (:context   4)
    (:pagemask  5)
    (:wired     6)
    (:c0.rsvd0  7)
    (:badvaddr  8)
    (:count     9)
    (:entryhi   10 :format :entryhi)
    (:compare   11)
    (:status    12 :format :status)
    (:cause     13 :format :cause)
    (:epc       14)
    (:prid      15)
    (:config    16 :format :config-mc12)
    (:lladdr    17 :doc "PC of the most recently executed LL insn")
    (:watchlo   18)
    (:watchhi   19)
    (:c0.rsvd1  20)
    (:config1   21 :format :config1)
    (:c0.rsvd3  22)
    (:debug     23)
    (:debugepc  24)
    (:perfcnt   25)
    (:ecc       26)
    (:cacheerr  27)
    (:taglo     28)
    (:taghi     29)
    (:errorepc  30)
    (:desave    31))
   ((:cop1control "MIPS FPU control registers")
    (:fir       0)
    (:fccr      25)
    (:fexr      26)
    (:fenr      28)
    (:fcsr      31))
   ((:gpr               "")
    (:zero      0 :aliases (:r0))
    (:at        1 :aliases (:r1))
    (:v0        2 :aliases (:r2))
    (:v1        3 :aliases (:r3))
    (:a0        4 :aliases (:r4))
    (:a1        5 :aliases (:r5))
    (:a2        6 :aliases (:r6))
    (:a3        7 :aliases (:r7))
    (:t0        8 :aliases (:r8))
    (:t1        9 :aliases (:r9))
    (:t2       10 :aliases (:r10))
    (:t3       11 :aliases (:r11))
    (:t4       12 :aliases (:r12))
    (:t5       13 :aliases (:r13))
    (:t6       14 :aliases (:r14))
    (:t7       15 :aliases (:r15))
    (:s0       16 :aliases (:r16))
    (:s1       17 :aliases (:r17))
    (:s2       18 :aliases (:r18))
    (:s3       19 :aliases (:r19))
    (:s4       20 :aliases (:r20))
    (:s5       21 :aliases (:r21))
    (:s6       22 :aliases (:r22))
    (:s7       23 :aliases (:r23))
    (:t8       24 :aliases (:r24))
    (:t9       25 :aliases (:r25))
    (:kt0      26 :aliases (:r26))
    (:kt1      27 :aliases (:r27))
    (:gp       28 :aliases (:r28))
    (:sp       29 :aliases (:r29))
    (:s8       30 :aliases (:r30))
    (:ra       31 :aliases (:r31)))
   ((:fpr       "MIPS FPR registers (Cop1)")
    (:f0        0 :aliases (:fv0))
    (:f1        1)
    (:f2        2 :aliases (:fv1))
    (:f3        3)
    (:f4        4 :aliases (:ft0))
    (:f5        5)
    (:f6        6 :aliases (:ft1))
    (:f7        7)
    (:f8        8 :aliases (:ft2))
    (:f9        9)
    (:f10       10 :aliases (:ft3))
    (:f11       11)
    (:f12       12 :aliases (:fa0))
    (:f13       13)
    (:f14       14 :aliases (:fa1))
    (:f15       15)
    (:f16       16 :aliases (:ft4))
    (:f17       17)
    (:f18       18 :aliases (:ft5))
    (:f19       19)
    (:f20       20 :aliases (:fs0))
    (:f21       21)
    (:f22       22 :aliases (:fs1))
    (:f23       23)
    (:f24       24 :aliases (:fs2))
    (:f25       25)
    (:f26       26 :aliases (:fs3))
    (:f27       27)
    (:f28       28 :aliases (:fs4))
    (:f29       29)
    (:f30       30 :aliases (:fs5))
    (:f31       31))
   ((:hilo      "MIPS HI/LO registers")
    (:lo        0)
    (:hi        1))))

(set-namespace :core)

(define-protocol-device-class mips-core :core (general-purpose-core 32bit-memory-device)
  ((bank :accessor mips-core-bank :initform (make-mips-bank)))
  (:default-initargs
   :isa *mips-isa*
   :default-state-type 'mips-state
   :continuation-address #xbfc00000
   :stop-address #xbfc00000))

(define-protocol-device-class mipsel :core (little-endian-core mips-core) ())
(define-protocol-device-class mipseb :core (big-endian-core mips-core) ())

(define-protocol-device-class mips-mmu-core :core (mmu-core) ())

(defstruct mips-bank
  (gpr-shadow (make-array 32 :element-type '(unsigned-byte 32)))
  (cop0-shadow (make-array 32 :element-type '(unsigned-byte 32))))

(define-device-class torn-pipeline-mips-core :core (mips-core)
  ((broken-by :accessor torn-pipeline-mips-core-broken-by :initarg :broken-by))
  (:documentation
   "A type of MIPS core which cannot be fully inspected without interventions
into its pipeline.  This includes cores which require TAP-driven execution of
instructions for register access, cores which have unfortunate software
instructions which place them into debug mode one pipeline step too late,
such kind of thing.")
  (:default-initargs :broken-by nil))

;;;;
;;;; Address space
;;;;
(defconstant kuseg #b000)
(defconstant kseg0 #b100)
(defconstant kseg1 #b101)
(defconstant kseg2 #b110)
(defconstant kseg3 #b111)
(defconstant x32kseg0 #b1111111111111111111111111111111111111111111111111111111111111111100)
(defconstant x32kseg1 #b1111111111111111111111111111111111111111111111111111111111111111101)
(defconstant x32kseg2 #b1111111111111111111111111111111111111111111111111111111111111111110)
(defconstant x32kseg3 #b1111111111111111111111111111111111111111111111111111111111111111111)

(defun seg32p (seg x)
  (= seg (ldb (byte 3 29) x)))
(defun x32seg32p (seg x)
  (= seg (ldb (byte 35 29) x)))

(defun ksegp (x) (ldb-test (byte 1 31) x))
(defun kusegp (x) (not (ksegp x)))
(defun kseg0p (x) (seg32p kseg0 x))
(defun kseg1p (x) (seg32p kseg1 x))
(defun kseg2p (x) (seg32p kseg2 x))
(defun kseg3p (x) (seg32p kseg3 x))
(defun x32kseg0p (x) (x32seg32p x32kseg0 x))
(defun x32kseg1p (x) (x32seg32p x32kseg1 x))
(defun x32kseg2p (x) (x32seg32p x32kseg2 x))
(defun x32kseg3p (x) (x32seg32p x32kseg3 x))

(defun remap-to-seg32 (seg x)
  (dpb seg (byte 3 29) x))
(defun remap-to-x32seg32 (seg x)
  (dpb seg (byte 35 29) x))

(defun remap-to-kuseg (x) (remap-to-seg32 kuseg x))
(defun remap-to-kseg0 (x) (remap-to-seg32 kseg0 x))
(defun remap-to-kseg1 (x) (remap-to-seg32 kseg1 x))
(defun remap-to-kseg2 (x) (remap-to-seg32 kseg2 x))
(defun remap-to-kseg3 (x) (remap-to-seg32 kseg3 x))
(defun remap-to-x32kseg0 (x) (remap-to-x32seg32 x32kseg0 x))
(defun remap-to-x32kseg1 (x) (remap-to-x32seg32 x32kseg1 x))
(defun remap-to-x32kseg2 (x) (remap-to-x32seg32 x32kseg2 x))
(defun remap-to-x32kseg3 (x) (remap-to-x32seg32 x32kseg3 x))

(defun extent-to-seg32 (seg extent)
  (etypecase extent
    (cons (cons (remap-to-seg32 seg (car extent)) (cdr extent)))))

(defun extent-to-x32seg32 (seg extent)
  (etypecase extent
    (cons (cons (remap-to-x32seg32 seg (car extent)) (cdr extent)))))

;;;;
;;;; Core API implementation
;;;;
(defmethod default-core-pc ((o mips-core))
  #xbfc00000)

(defmethod capture-instruction-counters ((o mips-core))
  (setf (core-instruction-counter o) (devreg o :count))
  (dolist (slave (core-slaves o))
    (capture-instruction-counters slave)))

(defmethod reset-instruction-counters ((o mips-core))
  (setc (devreg o :count) 0)
  (dolist (slave (core-slaves o))
    (reset-instruction-counters slave)))

(defmethod restore-instruction-counters ((o mips-core)))

(defmethod core-call-stack ((o mips-core))
  (make-array 0 :element-type '(unsigned-byte 32)))

(defmethod reset-core ((o torn-pipeline-mips-core))
  (setf (torn-pipeline-mips-core-broken-by o) nil))

;;;;
;;;; Target assembly execution
;;;;
(defmacro exec (core &body insns)
  (flet ((constant-params-p (params)
           (every (lambda (x) (or (keywordp x) (integerp x))) params)))
    (with-gensyms (target)
      `(let ((,target (backend ,core)))
         ,@(loop :for insn :in insns :collect
              (destructuring-bind (mnemonic . params) insn
                (multiple-value-bind (opcode param-specs) (lookup-insn *mips-isa* mnemonic)
                  `(exec-raw ,target
                             ,(if (constant-params-p params)
                                  (encode-insn *mips-isa* (list* mnemonic params))
                                  `(logior ,opcode
                                           ,@(iter (for param in params)
                                                   (for (byte . type-alist) in param-specs)
                                                   (collect
                                                       (typecase param
                                                         (integer (if (<= (integer-length param) (byte-size byte))
                                                                      (ash param (byte-position byte))
                                                                      (error "~@<Type mismatch in parameter #x~X for insn ~S: doesn fit ~D bits.~:@>"
                                                                             param mnemonic (byte-size byte))))
                                                         (keyword (if-let ((result (cdr (assoc param type-alist))))
                                                                    (ash result (byte-position byte))
                                                                    (error "~@<Type mismatch in parameter ~S for insn ~S: unknown mnemonic.~:@>"
                                                                           param mnemonic)))
                                                         (t (once-only (param)
                                                              `(ash ,param ,(byte-position byte))
                                                              ;; `(typecase ,param
                                                              ;;    (integer (ash ,param ,(byte-position byte)))
                                                              ;;    (keyword (ash (cdr (assoc ,param ',type-alist)) ,(byte-position byte)))
                                                              ;;    (t (error "~@<Runtime assembly error.~:@>")))
                                                              )))))))))))))))

;;;;
;;;; Target runtime
;;;;
(defun core-trans-stack-trace (core stack-top cenv)
  (let ((globals (sort (iter (for (name . tag) in (env:env-alist (env-global-frame (cenv-tagenv cenv))))
                             (collect (cons name (tag-address tag)))) #'< :key #'cdr))
        (target (backend core)))
    (flet ((address-extent-name (addr)
             (iter (for (cur . rest) on globals)
                   (if rest
                       (when (> (cdar rest) addr)
                         (return (car cur)))
                       (return (format *log-stream* "Stack entry ~8,'0X points past last tag ~X.~%" addr cur))))))
      (iter (for addr from stack-top by -4)
            (for stackval = (memory-ref target addr))
            (until (= stackval #xfeeddead))
            (if-let ((address-extent-name (address-extent-name stackval)))
              (collect (list addr stackval address-extent-name))
              (progn
                (collect (list addr stackval :outside-compilation-environment))
                (finish)))))))

(defmethod prepare-trans-args ((o mips-core) stack-top args)
  (destructuring-bind (&optional (arg0-ret 0) (arg1 0) (arg2 0) &rest more-bindings) args
    (iter (for (reg-name . value) in (list* (cons :stack-top stack-top)
                                            (cons :arg0-ret arg0-ret)
                                            (cons :arg1 arg1)
                                            (cons :arg2 arg2)
                                            more-bindings))
          (set-gpr-by-name o (evaluate-mips-gpr reg-name) value))))

(defun disasm-trans-zone (core cenv &optional print-bbnet-instead-of-raw)
  (declare (ignore print-bbnet-instead-of-raw))
  (with-compilation-environment cenv
    (let ((*segment* (first (cenv-segments cenv)))
          (tags (mapcar #'cdr (env:env-mapping (env-global-frame *tag-domain*)))))
      (labels ((tag-by-address (address)
                 (find address tags :key (compose #'remap-to-kuseg #'tag-address))))
        (core-disassemble core #x18000000 (length (segment-active-vector *segment*))
                          :line-pre-annotate-fn 
                          (lambda (stream base-address)
                            (when-let ((tag (tag-by-address base-address)))
                              (format stream "~&~10T~S~%" (envobject-name tag)))))))))

(defmethod trans-funcall ((o mips-core) (cenv compilation-environment) address-space function-name args &key trace disasm &allow-other-keys)
  (with-mips-gpr-environment
    (prepare-trans-args o (base (as-stack address-space)) args)
    (with-traps (#xbfc00380 #xbfc00400)
      (flush-core-instruction-cache o)
      (with-executed-segment (o (tag-address :entry-point)
                                :trace trace
                                :watch-fn (lambda (core stream)
                                            (declare (ignorable core stream))
                                            #+(or) (watch-mips-core core (base (as-stack address-space)) cenv)))
        (emit-nops 8)
        (apply #'emit-long-function-call function-name args)
        (when disasm
          (disasm-trans-zone o cenv nil))))))

(defmethod handle-execution-error ((o mips-core) (type (eql 'unexpected-stop-reason)) args)
  (let ((epc (with-temporary-state (o :debug)
               (devreg o :epc))))
    (core-disassemble o (- (logandc1 #xf epc) #x10) #x30)
    (call-next-method o type (list* :error-address epc args))))

;;;;
;;;; Breakpoints
;;;;
(defclass mips-software-breakpoint (software-breakpoint) ())
(defclass mips-hardware-breakpoint (hardware-trap) ())

;;;;
;;;; TLB
;;;;
(defmethod address-mapped-p ((o mips-core) (address integer))
  (not (or (kseg0p address)
           (kseg1p address))))

(defmethod tlb-active-p ((o mips-core))
  (eq (devbit-decode o :config :mmu-mode) :tlb))

(defmethod current-page-size ((o mips-core))
  (ash 1 (1- (integer-length (logior #x1000 (devreg o :pagemask))))))

(defstruct (mips-tlb-entry (:constructor make-mips-tlb-entry (hi lo0 lo1))
                           (:conc-name mips-tlbent-))
  (hi nil :type (unsigned-byte 32))
  (lo0 nil :type (unsigned-byte 32))
  (lo1 nil :type (unsigned-byte 32)))

(defmethod tlb-entry ((o mips-core) (i integer))
  (setc (devreg o :index) i)
  (exec o
    (:tlbr)
    (:nop))
  (make-mips-tlb-entry (devreg o :entryhi)
                       (devreg o :entrylo0)
                       (devreg o :entrylo1)))

(defmethod set-tlb-entry ((o mips-core) (i integer) (entry mips-tlb-entry))
  (setc (devreg o :index) i
        (devreg o :entryhi) (mips-tlbent-hi entry)
        (devreg o :entrylo0) (mips-tlbent-lo0 entry)
        (devreg o :entrylo1) (mips-tlbent-lo1 entry))
  (exec o
    (:tlbwi)
    (:nop)))

(defmethod decode-tlb-entry ((o mips-tlb-entry))
  (with-slots (hi lo0 lo1) o
    (list (decode :entryhi hi)
          (decode :entrylo lo0)
          (decode :entrylo lo1))))

(defmethod listify-tlb-entry ((o mips-tlb-entry))
  (with-slots (hi lo0 lo1) o
    (list hi lo0 lo1)))

(defmethod parse-tlb-entry ((type (eql :mips-tlb-entry)) list)
  (make-mips-tlb-entry (first list) (second list) (third list)))

(defmethod probe-tlb ((o mips-core) (asid integer) (vpn integer))
  (setc (devbits o :entryhi (:asid :vpn2)) (asid (ash vpn -1)))
  (exec o
    (:nop)
    (:tlbp)
    (:nop))
  (let ((index (devreg o :index)))
    (when (zerop (logandc1 #xf index))
      index)))

(defmethod tlb-address-map ((o mips-core) tlb page-size)
  (lret ((pagemask (1- (ash page-size 1)))
         (map (make-address-map :page-size page-size)))
    (iter (for entry in tlb)
          (with-slots (hi lo0 lo1) entry
            (let ((virt (logandc1 pagemask (dpb (bit-value hi :vpn2) (byte 19 13) 0))))
              (when (test-bits-set (:valid) lo0)
                (address-map-add map virt
                                 (logandc1 (ash pagemask -1) (ash (bit-value lo0 :addr) 12))))
              (when (test-bits-set (:valid) lo1)
                (address-map-add map (+ virt page-size)
                                 (logandc1 (ash pagemask -1) (ash (bit-value lo1 :addr) 12)))))))))

;;;;
;;;; Memory device
;;;;
(defmethod memory-device-8bit-ref ((o mips-core) address)
  (with-mips-assem
    (with-executed-segment (o #x18000000 :iteration-limit 10)
      (emit-load8 :r1 address)
      (emit-nops 3)))
  (ldb (byte 8 0) (devreg o :at)))

(defmethod memory-device-16bit-ref ((o mips-core) address)
  (with-mips-assem
    (with-executed-segment (o #x18000000 :iteration-limit 10)
      (emit-load16 :r1 address)
      (emit-nops 3)))
  (ldb (byte 16 0) (devreg o :at)))

(defmethod memory-device-32bit-ref ((o mips-core) address)
  (with-mips-assem
    (with-executed-segment (o #x18000000 :iteration-limit 10)
      (emit-load32 :r1 address)
      (emit-nops 3)))
  (ldb (byte 32 0) (devreg o :at)))

(defmethod memory-device-8bit-set ((o mips-core) address val)
  (with-mips-assem
    (with-executed-segment (o #x18000000 :iteration-limit 10)
      (emit-store8 val address))))

(defmethod memory-device-16bit-set ((o mips-core) address val)
  (with-mips-assem
    (with-executed-segment (o #x18000000 :iteration-limit 10)
      (emit-store16 val address))))

(defmethod memory-device-32bit-set ((o mips-core) address val)
  (with-mips-assem
    (with-executed-segment (o #x18000000 :iteration-limit 10)
      (emit-store32 val address))))

;;;;
;;;; State
;;;;
(defclass mips-state (state)
  ())

(defmethod emit-state-restorer (core segment (o mips-state))
  (declare (type pinned-segment segment))
  (with-slots (moment gpr fpr regs physical-cells tlb page-size virtual-pages physical-pages) o
    (let* ((pc (moment-fetch moment))
           (userspace-trampoline-p (kusegp pc))
           (nr-total-transit-regions (+ (length virtual-pages) (length physical-pages)))
           table-offset)
      (with-segment-emission (*mips-isa* segment)
        (with-mips-gpr-environment
          ;; Restore physical cells first: this allows us to do dirty platform setup tricks,
          ;; like enabling the TLB mode.
          (emit-tag :__entry)
          (emit-ref :entry (delta) :beq :r0 :r0 delta)
          (emit-tag :transit-table)
          (setf table-offset (- (tag-address :transit-table) (tag-address :__entry)))
          (emit-nops (* 3 nr-total-transit-regions))
          (emit-tag :entry)
          (iter (for (addr val) in physical-cells)
                (emit-store32 val addr))
          (multiple-value-bind (status-tlb-hi-lo cop0-regs) (unzip (lambda (x) (member (car x) '(:status :hi :lo :entryhi :entrylo0 :entrylo1))) regs)
            (when-let ((hi (cadr (assoc :hi status-tlb-hi-lo))))
              (emit-set-hi hi))
            (when-let ((lo (cadr (assoc :lo status-tlb-hi-lo))))
              (emit-set-lo lo))
            (mapc (curry #'apply #'emit-set-cp0) cop0-regs)
            (when-let ((status (cadr (assoc :status status-tlb-hi-lo))))
              (emit-set-cp0 :status (logior status
                                            (bits (:exl) userspace-trampoline-p))))
            (iter (for entry in tlb) (for i from 0)
                  (emit-set-tlb-entry i (listify-tlb-entry entry))
                  (emit-nops 1))
            (mapc (curry #'apply #'emit-set-cp0) (unzip (lambda (x) (member (car x) '(:entryhi :entrylo0 :entrylo1))) status-tlb-hi-lo)))
          (when userspace-trampoline-p
            (emit-set-cp0 :epc pc))
          (when fpr
            (mapc #'emit-set-fpr (mapcar (curry #'format-symbol :keyword "F~D") (iota 32)) fpr))
          (when (plusp nr-total-transit-regions)
            ;;  Copy virtual pages from the transit area into their target location.
            ;; ...........
            ;; r1: current region entry pointer
            ;; r2: remaining region count
            ;; r3: source
            ;; r4: dest
            ;; r5: region copy loop counter
            (emit-ref :transit-table (delta :tag-address table-base)
              :lui :r1 (ash table-base -16))
            (emit-ref :transit-table (delta :tag-address table-base)
              :ori :r1 :r1 (logand #xffff table-base))
            (emit* :ori :r2 :r2 nr-total-transit-regions)
            (emitting-iteration (nr-total-transit-regions :outer-exit :r2)
              (emit* :lw :r3 0 :r1)
              (emit* :lw :r4 4 :r1)
              (emit* :lw :r5 8 :r1)
              (emit* :nop)
              (emitting-iteration (:r5 :inner-exit :r5)
                (emit* :lw :r6 0 :r3)
                (emit* :sw :r6 0 :r4)
                (emit* :addiu :r3 :r3 4)
                (emit* :addiu :r4 :r4 4))
              (emit* :addiu :r1 :r1 #xc)))
          (mapc #'emit-set-gpr (mapcar (curry #'format-symbol :keyword "R~D") (iota 32)) gpr)
          (when userspace-trampoline-p
            (emit* :nop)
            (emit* :nop)
            (emit* :eret)
            (emit* :nop))
          (emit-tag :transit-regions)
          (let ((map (when virtual-pages
                       (tlb-address-map core tlb page-size))))
            (iter (for (virtp . extent) in (append (mapcar (curry #'cons nil) physical-pages)
                                                   (mapcar (curry #'cons t)   virtual-pages)))
                  (let ((phys-base (xform virtp (curry #'virt-to-phys map) (base extent))))
                    (setf (u8-vector-word32le (segment-data *segment*) (+ table-offset 0)) (current-absolute-addr)
                          (u8-vector-word32le (segment-data *segment*) (+ table-offset 4)) (remap-to-kseg0 phys-base)
                          (u8-vector-word32le (segment-data *segment*) (+ table-offset 8)) (size extent)))
                  (assem-emission:emit-vector (extent-data extent))
                  (incf table-offset #xc)))))
      segment)))
