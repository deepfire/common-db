;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SPACES; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :interface)


(extend-namespace :interface
  (:register-formats
   (:ezusb-command-header ""
    (:ez-hdr-type       2 0   ""
      ((#x0  :plain         "")
       (#x1  :block-write   "")
       (#x2  :block-read    "")
       (#x3  :idcode/endblk "")))
    (:sys-nrst          1 2   "")
    (:sys-ntrst         1 3   "")
    (:size              2 4   ""
      ((#x0  :size-32       "")
       (#x1  :size-16       "")
       (#x2  :size-12       "")
       (#x3  :size-8        "")))
    (:ndebug-oper       1 6 "0 when either DEBUG_REQUEST or DEBUG_ENABLE")
    (:irdrsel           1 7 ""
      ((#x0  :dr        "")
       (#x1  :ir        ""))))
   (:ezusb-cmd "command pipe opcodes"
     (:ezusb-opcode             8 0 ""
      ((#x1 :program-pll-to-12mhz "")
       (#x2 :program-pll-to-24mhz "")
       (#x3 :program-pll-to-48mhz "")
       (#x4 :begin-reset "")
       (#x5 :end-reset ""))))
   (:parport-cmd ""
     (:parport-opcode           8 0 "Command opcode"
      ((#x01    :pausedr-updatedr-rti   "Pause -> Exit2 -> Update -> RTI")
       (#x02    :start                  "Change JTAG TAP state from TLR to RTI")
       (#x04    :finish                 "Change JTAG TAP state from RTI to TLR")
       (#x08    :prewrite               "Prepare data for writing")
       (#x10    :reset-preread          "prepare data for reading/reset sequence prologue")
       (#x20    :writedr                "Write data from adapter into TAP DR")
       (#x80    :writeir                "Write data from adapter into TAP IR")
       (#x40    :rti-capturedr-pausedr  "RTI -> Capture -> Exit1 -> Pause")
       (#x41    :pausedr-shiftdr-pausedr "Pause-DR -> Shift-DR -> Pause-DR"))))
   (:parport-status "Interface status"
     (:idmask                   3 0 "Adapter version")
     (:blkio                    1 3 "'block IO completed' interface state")
     (:tap-state                4 4 "TAP state"
      ((#x1     :wrir           "Write-IR")
       (#x2     :wrdr           "Write-DR")
       (#x4     :rti            "Run-Test-Idle")
       (#x8     :pausedr        "Pause-DR"))))
   (:idcode "interface idcode"
     (:oncd-version            4 28 "multicore OnCD version designator"
      ((#x2     :mc-12-mcam      "multicore OnCD versions from MC12 to Multicam")
       (#x3     :mc-nvcom-mforce "multicore OnCD versions beginning with Navicom and Multiforce"))))
   (:ir "JTAG TAP IR"
     (:tap-opcode              32 0 "TAP opcode"
      ((#x0000  :extest         "EXTEST")
       (#x0001  :sample         "SAMPLE")
       (#x0004  :debug-request  "DEBUG_REQUEST, Ask the chip to stop and prepare to attachment.")
       (#x0005  :debug-enable   "DEBUG_ENABLE, Attach TDI/TDO to OnCD.")
       (#xFFFF  :bypass         "BYPASS"))))
   (:ird "OnCD IRd"
     (:regsel                   4 0 "JTAG TAP DR selector"
      ((#x000    :oscr    "status control")
       (#x001    :ombc    "breakpoint match counter")
       (#x002    :omlr0   "breakpoint match 0")
       (#x003    :omlr1   "breakpoint match 1")
       (#x004    :obcr    "breakpoint control")
       (#x005    :irdec   "decoded instruction opcode")
       (#x006    :trace-count "trace counter")
       (#x007    :pcdec   "decoded instruction address")
       (#x008    :pcexec  "executed instruction address")
       (#x009    :pcmem   "memaccess instruction address")
       (#x00a    :pcfetch "fetched instruction address")
       (#x00b    :omar    "memory access address")
       (#x00c    :omdr    "memory access data")
       (#x00d    :enmem   "immediate memory access command")
       (#x00e    :pcwb    "writeback instruction address")
       (#x00f    :engo    "debug exit command")
       (#x100    :enregf  "extended internals access command, MForce/NvCom+")))
     (:exit                     1 4 "Exit from debug mode")
     (:nstep                    1 5 "nStep: 0: free run, 1: single step")
     (:rdonly/push-irdec        1 6 "read only access to OnCD DRs/reloading of IRdec needed")
     (:modstep                  1 7 "0: full insn execution stepping 1: usual pipelined-mode step")
     (:data16                  16 8 "16-bit-wide IRd+ data field")
     (:data32                  32 8 "32-bit-wide IRd+ data field"))
   (:oscr "OnCD status register"
     (:slctmem                  1 0 "Allow Memory Access")
     (:ro                       1 1 "0: Write, 1: Read")
     (:trace-mode               1 2 "Trace Mode Enable")
     (:ime                      1 3 "Debug Interrupt Enable")
     (:erase                    1 4 "Flush CPU Pipeline At Debug Exit")
     (:rdym                     1 5 "RDY signal state")
     (:mbo                      1 6 "BreakPoint triggered")
     (:to                       1 7 "Trace Counter triggered")
     (:swo                      1 8 "SoftWare enter into Debug mode")
     (:so                       1 9 "CPU Mode, 0: running, 1: Debug")
     (:kern+mutexcpt            1 10 "Debug Mode On")
     (:no-dslot-stop            1 11 "Do not stop in delay slot")
     (:vbo                      1 12 "Exception catched")
     (:aerr-ban                 1 13 "Do not raise exception at pc fetch")
     (:b/wp0                    1 14 "Break/watch point 0 triggered")
     (:b/wp1                    1 15 "Break/watch point 1 triggered")
     (:bfc00000                 1 16 "Catch #xbfc00000 vector")
     (:80000000                 1 17 "Catch #x80000000 vector")
     (:80000180                 1 18 "Catch #x80000180 vector")
     (:80000200                 1 19 "Catch #x80000200 vector")
     (:bfc00200                 1 20 "Catch #xbfc00200 vector")
     (:bfc00380                 1 21 "Catch #xbfc00380 vector")
     (:bfc00400                 1 23 "Catch #xbfc00400 vector")
     (:dspbrkpq                 1 24 "DSP breakpoint stops CPU, interrupt suppressed")
     (:dspbrkpt                 1 25 "CPU stopped by DSP breakpoint")
     (:en-dbmp                  1 26 "nDE stops CPU in MP mode")
     (:dbmp-trig                1 27 "CPU stopped by nDE in MP mode"))
   (:obcr "OnCD breakpoint control register"
     (:mbs-0                    1 0 "breakpoint select 0: pc, 1: memory address")
     (:mbs-1                    1 1 "breakpoint select 1: pc, 1: memory address")
     (:write-0                  1 2 "0: disabled, 1: write, 2: read, 3 rw")
     (:read-0                   1 3 "0: disabled, 1: write, 2: read, 3 rw")
     (:cc-0                     2 4 "Stop if address relates to OMLR0 as:"
      ((#b00    :non-equal)
       (#b01    :equal)
       (#b10    :less-than)
       (#b11    :greater-than)))
     (:write-1                  1 6 "0: disabled, 1: write, 2: read, 3 rw")
     (:read-1                   1 7 "0: disabled, 1: write, 2: read, 3 rw")
     (:cc-1                     2 8 "Stop if address relates to OMLR1 as:"
      ((#b00    :non-equal)
       (#b01    :equal)
       (#b10    :less-than)
       (#b11    :greater-than)))
     (:unbound                  1 10 "unbound comparison mode: 0: both must match, 1: any match works")))
  (:layouts
   ((:parport-cmd      "Interface command")
    (:parport-cmd      0 :format :parport-cmd :doc "Interface command"))
   ((:parport-status   "Interface status")
    (:parport-status   0 :format :parport-status :doc "Interface status"))
   ((:tap-idcode       "OnCD TAP IDCODE access")
    (:idcode           0 :format :idcode :doc "TAP IDCODE access"))
   ((:tap-ir           "OnCD TAP instruction register")
    (:ir               0 :format :ir :doc "TAP Instruction Register"))
   ((:tap-ird          "TAP DR IRD+")
    (:ird              0 :format :ird :doc "TAP DR cell, via writedr TAP path"))
   ((:tap-dr           "OnCD TAP data registers")
    (:oscr        #x0000 :format :oscr :doc "status control"        :ext (32 t nil))
    (:ombc        #x0001 :doc "breakpoint match counter"            :ext (16 nil t))
    (:omlr0       #x0002 :doc "breakpoint match 0"                  :ext (32 nil t))
    (:omlr1       #x0003 :doc "breakpoint match 1"                  :ext (32 nil t))
    (:obcr        #x0004 :format :obcr :doc "breakpoint control"    :ext (12 nil t))
    (:irdec       #x0005 :doc "decoded instruction opcode"          :ext (32 nil nil))
    (:trace-count #x0006 :doc "trace counter"                       :ext (16 nil t))
    (:pcdec       #x0007 :doc "decoded instruction address"         :ext (32 t nil))
    (:pcexec      #x0008 :doc "executed instruction address"        :ext (32 t nil))
    (:pcmem       #x0009 :doc "memaccess instruction address"       :ext (32 t nil))
    (:pcfetch     #x000a :doc "fetched instruction address"         :ext (32 nil nil))
    (:omar        #x000b :doc "memory access address"               :ext (32 nil t))
    (:omdr        #x000c :doc "memory access data"                  :ext (32 t t))
    (:mem         #x000d :doc "immediate memory access command"     :ext (0 t nil))
    (:pcwb        #x000e :doc "writeback instruction address"       :ext (32 t nil))
    (:exit        #x000f :doc "debug exit command"                  :ext (0 t nil))
    (:enregf      #x0010 :doc "RegF access command"                 :ext (32 nil nil)))
   ((:ezusb-cmdhdr     "EZ-USB-based adapter command header")
    (:ezusb-cmdhdr     0 :format :ezusb-command-header))
   ((:ezusb-cmd        "EZ-USB-based adapter command pipe")
    (:ezusb-cmd        0 :format :ezusb-cmd))))

(extend-namespace :target
  (:register-formats
   (:irdec-regf "The blessing of MForce/NVCom."
     (:meta-selector    3 0 "extended content REGF selector"
      ((#b000    :rfcpu)
       (#b001    :rffpu)
       (#b010    :cop0)
       (#b011    :hilo)
       (#b100    :cop1control "FIR, FCSR")
       (#b101    :tlb-pfn0    "PFN0, C0, D0, V0")
       (#b110    :tlb-pfn1    "PFN1, C1, D1, V1")
       (#b111    :tlb-flags   "R, W, G, ASID, PM, VPN")))
     (:selector-bottom  5 3 "")
     (:selector-top     5 16 "")))
  (:layouts
   ((:irdec-regf "The blessing of MForce/NVCom.")
    (:regf       0 :format :irdec-regf))))


(define-device-class elvees-interface :interface (interface)
    ()
  (:layouts (:tap-ir     nil nil)
            (:tap-ird    nil nil)
            (:tap-idcode nil nil)
            (:tap-dr     nil nil))
  (:extended-layouts :tap-dr))


(defconstant +memory-iteration-period+ 1000000)

(defmethod interface-bus-word ((o elvees-interface) address)
  "Read 32 bits from a given bus address."
  (declare (type (unsigned-byte 32) address))
  (lret (result)
    (setc (devbits o :oscr (:slctmem :ro)) (t t)
          (devreg o :omar) address
          (devreg o :mem) 0)
    (busywait (test-devbits o :oscr :rdym)
              ((error 'interface-memory-timeout :interface o))
              :iteration-period +memory-iteration-period+ :timeout 300)
    (setc result (devreg o :omdr)
          (devbits o :oscr (:slctmem :ro)) (nil nil))))

(defmethod (setf interface-bus-word) (val (o elvees-interface) address)
  "Write 32 bits into a given bus address."
  (declare (type (unsigned-byte 32) val address))
  (setc (devbit o :oscr :slctmem) t
        (devreg o :omar) address
        (devreg o :omdr) val
        (devreg o :mem) 0)
  (busywait (test-devbits o :oscr :rdym)
            ((error "RDYM!"))
            :iteration-period +memory-iteration-period+ :timeout 300)
  (setc (devbit o :oscr :slctmem) nil)
  val)

(defmethod interface-bus-io ((o elvees-interface) buffer address size direction &optional (offset 0))
  (declare (type (simple-array (unsigned-byte 8)) buffer)
           (type (unsigned-byte 32) address)
           (type (unsigned-byte 28) size)
           (type (member :read :write) direction))
  (assert (zerop (ldb (byte 2 0) size)))
  (assert (zerop (ldb (byte 2 0) address)))
  (let ((addr address))
    (loop :for size :from size :above 3 :by 4
       :for buf-offset :upfrom offset :by 4
       :do
       (if (eq direction :read)
           (setf (u8-vector-word32le buffer buf-offset) (interface-bus-word o addr))
           (setf (interface-bus-word o addr) (u8-vector-word32le buffer buf-offset)))
       (incf addr 4))))