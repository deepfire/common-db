;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TEST-HARDWARE; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :test-hardware)


(set-namespace :interface :platform :core)


(defvar *failure-inspector*)
(defvar *log2-iota* (mapcar (curry #'ash 1) (iota 31 :start 0)))

(defcomdbtest :target internal-memory () (target)
  (expect-success (test-target-memory target (base (memory-region-extent (target-device target '(internal-memory 0))))
                                      #x1000)))

(defcomdbtest :target external-memory () (target)
  (expect-success (test-target-memory target (base (memory-region-extent (target-device target '(external-memory 0))))
                                      #x1000)))

(defcomdbtest :mips step-one () (core)
  (reset :core core)
  (step-core-synchronous core)
  t)

(defcomdbtest :mips run-one () (core)
  (reset :core core)
  (settrace 1)
  (run-core-asynchronous core #x80000000)
  (setf (saved-core-moment core) (core-moment core))
  (expect-core-fetch-address core (fetch #x80000004) (breakpoint-not-reached :breakpoint 0))
  (setf (state core) :debug)
  t)

(defcomdbtest :mips hw-breakpoint-simple () (core)
  (reset :core core)
  (with-bioable-mips-segment (core #x0)
    (emit-nops #x80))
  (with-free-hardware-breakpoints (core) ((b #x8000007c))
    (run-core-asynchronous core #x80000000)
    (sleep 0.005)
    (setf (saved-core-moment core) (core-moment core))
    (expect-core-fetch-address core (fetch #x80000080) (breakpoint-not-reached :breakpoint 0))
    (setf (state core) :debug)
    t))

(defun test-trans-funcall (core segment base marker)
  (let* ((base (remap-to-seg32 segment base))
         (cenv (nth-value
                1
                (with-bioable-mips-segment (core base)
                  (with-function-calls
                    (with-mips-gpri (:ret-reg :addr :value :addr1 :value1)
                      (emit-nops 3)
                      (emitting-function :place-marker ()
                        (emit-set-gpr :addr :arg0-ret)
                        (emit-set-gpr :value :arg1)
                        (emit* :sw :value 0 :addr))
                      (emitting-function :call-marker-placer ()
                        (emit-set-gpr :addr1 :arg0-ret)
                        (emit-set-gpr :value1 marker)
                        (emit* :addiu :addr1 :addr1 4)
                        (emit-near-function-call :place-marker :addr1 :value1))
                      (emit-global-tag :entry-point)
                      (emit-nops #x10)
                      (emit-global-tag :end-of-code)
                      (save-compilation-environment (env:find-environment 'gpr) *tag-domain*))))))
         (address-space (make-instance 'address-space :extent (extent-to-seg32 segment
                                                                               (memory-region-extent (target-device (backend core) '(internal-memory 0))))))
         (*log-state-changes* nil))
    (handler-case (trans-funcall* core cenv address-space :call-marker-placer base)
      (core:unexpected-stop-reason (c)
        #+nil
        (expect-core-fetch-address core (fetch ...)
            (breakpoint-not-reached :breakpoint #x80000044))))
    (memory-ref (backend core) (+ base 4))))

(defun run-trans-funcall-test (core segment state &aux
                               (marker #xf0afd00d))
  (with-temporary-state (core state)
    (with-subtest :meeting-foafdood
      (expect-value marker (test-trans-funcall
                            core segment (base (memory-region-extent (target-device (backend core) '(internal-memory 0)))) marker)))))

(defcomdbtest :mips trans-funcall-kseg1-stop () (core)
  (run-trans-funcall-test core kseg1 :stop))

(defcomdbtest :mips trans-funcall-kseg1-debug () (core)
  (run-trans-funcall-test core kseg1 :debug))

(defcomdbtest :mips trans-funcall-kseg0-stop () (core)
  (run-trans-funcall-test core kseg0 :stop))

(defcomdbtest :mips trans-funcall-kseg0-debug () (core)
  (run-trans-funcall-test core kseg0 :debug))

(defcomdbtest :mips trans-funcall-kuseg-stop () (core)
  (run-trans-funcall-test core kuseg :stop))

(defcomdbtest :mips trans-funcall-kuseg-debug () (core)
  (run-trans-funcall-test core kuseg :debug))

(defun emit-r1-fff-target+18-end+40-loading-sequence (core address)
  (with-bioable-mips-segment (core address)
    (emit* :xori :r1 :zero 0)
    (emit* :xori :r1 :r1 #b000000000001)
    (emit* :xori :r1 :r1 #b000000000010)
    (emit* :xori :r1 :r1 #b000000000100)
    (emit* :xori :r1 :r1 #b000000001000)
    (emit* :xori :r1 :r1 #b000000010000)
    (emit* :xori :r1 :r1 #b000000100000)
    (emit* :xori :r1 :r1 #b000001000000)
    (emit* :xori :r1 :r1 #b000010000000)
    (emit* :xori :r1 :r1 #b000100000000)
    (emit* :xori :r1 :r1 #b001000000000)
    (emit* :xori :r1 :r1 #b010000000000)
    (emit* :xori :r1 :r1 #b100000000000)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)))

(defcomdbtest :mips free-debug-stop () (core)
  (reset :core core)
  (unwind-protect
       (progn
         (emit-r1-fff-target+18-end+40-loading-sequence core #x0)
         (hw-break 1 #xbfc00380)
         (with-subtest :r1-clear
           (with-subtest :first-break-reached
             (hw-break 0 #x8000001c)
             (setc (state core) :stop)
             (run-core-synchronous core :address #x80000000 :exit-state :debug)
             (expect-core-fetch-address core (fetch #x80000020)
                 (breakpoint-not-reached :breakpoint #x8000001c)))
           (with-subtest :second-break-reached
             (hw-break 0 #x80000040)
             (setc (state core) :stop)
             (run-core-synchronous core :exit-state :debug)
             (expect-core-fetch-address core (fetch #x80000044)
                 (breakpoint-not-reached :breakpoint #x80000040)))
           (expect-value #xfff (with-temporary-state (core :debug)
                                 (gpr-by-name core :at)))))
    (hw-break 0 nil)
    (hw-break 1 nil)
    (clear-sw-breaks)))

(defun emit-r1-jumpclear-0x14-0x30 (core address)
  (with-bioable-mips-segment (core address)
    (emit* :ori :r1 :r0 0)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit-ref :past-ori (delta) :beq :r0 :r0 delta)
    (emit* :nop)
    (emit* :ori :r1 :r1 #b0001)
    (emit-tag :past-ori)
    (emit* :xori :r1 :r1 #b1000)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)))

(defun emit-r1-jumpclear-0x14-0x30-jal (core address)
  (with-bioable-mips-segment (core address)
    ;; #x0
    (emit* :ori :r1 :r0 0)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    ;; #x10
    (emit* :nop)
    (emit* :jal #x8)
    (emit* :nop)
    (emit* :ori :r1 :r1 #b0001)
    ;; #x20
    (emit* :xori :r1 :r1 #b1000)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)))

(defun emit-r1-jumpclear-0x14-0x30-jr (core address)
  (with-bioable-mips-segment (core address)
    ;; #x0
    (emit* :ori :r1 :r0 0)
    (emit* :lui :r2 #x8000)
    (emit* :ori :r2 :r2 #x20)
    (emit* :nop)
    ;; #x10
    (emit* :nop)
    (emit* :jr :r2)
    (emit* :nop)
    (emit* :ori :r1 :r1 #b0001)
    ;; #x20
    (emit* :xori :r1 :r1 #b1000)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)))

(defun emit-r1-jumpclear-0x14-0x30-dependent (core address)
  (with-bioable-mips-segment (core address)
    (emit* :ori :r1 :r0 #x8000)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :ori :r1 :r0 0)
    (emit-ref :past-ori (delta) :beq :r0 :r1 delta)
    (emit* :nop)
    (emit* :ori :r1 :r1 #b0001)
    (emit-tag :past-ori)
    (emit* :xori :r1 :r1 #b1000)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)))

(defun emit-r1-complex-jumpclear (core address)
  (with-bioable-mips-segment ((backend core) address)
    (emit* :ori :r1 :zero 0)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit-ref :past-ori-0 (delta) :beq :r0 :r0 delta)
    (emit* :nop)
    (emit* :ori :r1 :r1 #b0001)
    (emit-tag :past-ori-0)
    (emit-ref :past-ori-1 (delta) :beq :r0 :r0 delta)
    (emit* :nop)
    (emit* :ori :r1 :r1 #b0010)
    (emit-tag :past-ori-1)
    (emit-ref :past-ori-2 (delta) :beq :r0 :r0 delta)
    (emit* :nop)
    (emit* :ori :r1 :r1 #b0100)
    (emit-tag :past-ori-2)
    (emit-ref :past-ori-3 (delta) :beq :r0 :r0 delta)
    (emit* :nop)
    (emit* :ori :r1 :r1 #b1000)
    (emit-tag :past-ori-3)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)))

(defun emit-interrupt-loop (core address)
  (with-bioable-mips-segment (core #x18000000)
    (emit-nops #x10))
  (with-bioable-mips-segment (core address)
    (emit* :mfc0 :r1 :status 0)
    (emit* :mfc0 :r2 :cause 0)
    (emit* :ori :r1 :r1 (bits (:sw0 :ie)))
    (emit* :mtc0 :r1 :status 0)
    (emit* :ori :r2 :r2 (bits (:p-sw0)))
    (emit* :mtc0 :r2 :cause 0)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :nop)
    (emit* :eret)
    (emit* :nop)
    (setc (devbits core :status (:exl :erl)) (t nil)
          (devreg core :epc) #x98000004)))

(defun emit-software-interrupt-generator-epc=base+5 (core address)
  (with-bioable-mips-segment (core #xbfc00380)
    (emit-nops #x10))
  (with-bioable-mips-segment (core address)
    (emit* :mfc0 :r1 :status 0)
    (emit* :mfc0 :r2 :cause 0)
    (emit* :ori :r1 :r1 (bits (:sw0 :ie)))
    (emit* :mtc0 :r1 :status 0)
    (emit* :ori :r2 :r2 (bits (:p-sw0)))
    (emit* :mtc0 :r2 :cause 0)
    (emit* :nop)
    (setc (devbits core :status (:exl :erl)) (nil nil))))

(defun emit-setup-gpr-standalone-bits (core address)
  "Emit code to load the GPR RF with (mapcar (curry #'expt 2) (iota 31))."
  (with-bioable-mips-segment (core address)
    (emit-core-state-restorer *segment* (make-instance 'mips:state :gpr (list* 0 *log2-iota*)))
    (emit* :nop)))

(defun make-expect-r1 (x)
  (lambda (core)
    (expect-value x (gpr-by-name core :r1))))

(defun make-exception-checker (exception-type &optional expected-epc)
  (lambda (core)
    (with-subtest "IN-EXCEPTION"
      (unless (devbit core :status :exl)
        (test-error "~@<Cop0.status.exl is not set.~:@>")))
    (with-subtest ((format nil "CP0-EXCCODE-SAYS-~A" exception-type))
      (unless (= (devbit-value core :cause :exccode) (bits :exccode exception-type))
        (test-error "~@<Cop0.status.exccode is ~X, not ~X.~:@>" (devbit-value core :cause :exccode) exception-type)))
    (with-subtest "PC-IN-EXCEPTION-VECTOR"
      (let ((pc (moment-fetch (saved-core-moment core))))
        (unless (and (>= pc #xbfc00380) (< pc #xbfc003c0))
          (test-error "~@<PC is is ~X, not in proper exception.~:@>" pc))))
    (when expected-epc
      (with-subtest "EPC-POINTS-AT-PERPETRATOR"
        (unless (= (devreg core :epc) expected-epc)
          (test-error "~@<Cop0.epc is ~X, not expected ~X.~:@>" (devreg core :epc) expected-epc))))
    t))

(defun check-in-software-interrupt (core &optional expected-epc)
  (unwind-protect (funcall (make-exception-checker :interrupt expected-epc) core)
    (setc (devbits core :status (:exl :sw0)) (nil nil)
          (devbit core :cause :p-sw0) nil))
  t)

(defun production-stop-debug-stop (core)
  (stop-to-debug core)
  (debug-to-stop core))

(defparameter *runner-stepper-test-pack*
  `(#x80000000
    (:walkpad                      emit-r1-fff-target+18-end+40-loading-sequence 16 ,(make-expect-r1 #xfff))
    (:beq-r0-r0                    emit-r1-jumpclear-0x14-0x30                   16 ,(make-expect-r1 8))
    (:jal                          emit-r1-jumpclear-0x14-0x30-jal               16 ,(make-expect-r1 8))
    (:jr                           emit-r1-jumpclear-0x14-0x30-jr                16 ,(make-expect-r1 8))
    (:beq-dependent                emit-r1-jumpclear-0x14-0x30-dependent         16 ,(make-expect-r1 8))))

(defparameter *interrupt-test-pack*
  `(#x80000000
    (:interrupt       emit-software-interrupt-generator-epc=base+5  #x9 ,(rcurry #'check-in-software-interrupt #x80000018) nil nil)))

(defparameter *interrupt-loop-test-pack*
  `(#xbfc00380
    (:interrupt-loop  emit-interrupt-loop                           #x14 ,(rcurry #'check-in-software-interrupt #x98000004) nil nil)))

(defparameter *debug-stop-debug-test-pack*
  `(#x80000000
    (:dsd-prebranch-production     emit-r1-jumpclear-0x14-0x30                   #x10 ,(make-expect-r1 8)     t t #x80000010 production-stop-debug-stop)
    (:dsd-prebranch-production-jal emit-r1-jumpclear-0x14-0x30-jal               #x10 ,(make-expect-r1 8)     t t #x80000010 production-stop-debug-stop)
    (:dsd-prebranch-production-jr  emit-r1-jumpclear-0x14-0x30-jr                #x10 ,(make-expect-r1 8)     t t #x80000010 production-stop-debug-stop)
    (:dsd-prebranch-production-dep emit-r1-jumpclear-0x14-0x30-dependent         #x10 ,(make-expect-r1 8)     t t #x80000010 production-stop-debug-stop)
    (:dsd-branch-production        emit-r1-jumpclear-0x14-0x30                   #x10 ,(make-expect-r1 8)     t t #x80000014 production-stop-debug-stop)
    (:dsd-branch-production-jal    emit-r1-jumpclear-0x14-0x30-jal               #x10 ,(make-expect-r1 8)     t t #x80000014 production-stop-debug-stop)
    (:dsd-branch-production-jr     emit-r1-jumpclear-0x14-0x30-jr                #x10 ,(make-expect-r1 8)     t t #x80000014 production-stop-debug-stop)
    (:dsd-branch-production-dep    emit-r1-jumpclear-0x14-0x30-dependent         #x10 ,(make-expect-r1 8)     t t #x80000014 production-stop-debug-stop)
    (:dsd-jumpclear-production     emit-r1-fff-target+18-end+40-loading-sequence #x10 ,(make-expect-r1 #xfff) t t #x80000014 production-stop-debug-stop)))

(defcomdbtest :mips cop0-read/write-stability () (core)
  (reset :core core)
  (let ((testval (random #x100000000)))
    (with-temporary-state (core :debug)
      (setc (devreg core :count) testval)
      (expect-value testval (devreg core :count)))))

(defun emit-cop0-count-test (core address)
  (with-bioable-mips-segment (core address)
    (with-mips-gpri (:count-proxy)
      (emit* :mtc0 :r0 :count 0)          ; #x0
      (emit* :ori :count-proxy :r0 0)
      (emit* :nop)
      (emit* :nop)
      (emit* :nop)                      ; #x10
      (emit* :nop)
      (emit* :nop)
      (emit* :nop)
      (emit* :nop)                      ; #x20
      (emit* :nop)
      (emit* :nop)
      (emit* :nop)
      (emit* :nop)                      ; #x30
      (emit* :nop)
      (emit* :nop)
      (emit* :nop)
      (emit* :nop)                      ; #x40
      (emit* :mfc0 :count-proxy :count 0)
      (emit* :nop)
      (emit* :nop)
      (emit* :nop)                      ; #x50
      (emit* :nop))))

;;;;
;;;; Test runner engines
;;;;
(defun run-test-pack (core pack-runner-fn pack &optional (failure-inspector-fn 'values) test-length &aux (success t))
  (let ((*failure-inspector* failure-inspector-fn))
    (declare (special *failure-inspector*))
    (destructuring-bind (address . test-specs) pack
      (iter (for (test-name test-emit-and-prepare-fn . more-test-args) in test-specs)
            (reset :core core :state :debug)
            (let ((segment (funcall test-emit-and-prepare-fn core address)))
              (setf (core-insn-execution-limit core) (or test-length
                                                         (segment-emitted-insn-count segment))))
            (when *examine-test*
              (disasm address (* #x4 (core-insn-execution-limit core))))
            (andf success (apply pack-runner-fn core test-name address more-test-args))
            (finally (return success))))))

(defun run-maybe-swbreak-and-dsd-test (core name start-address insn-count successp &optional (trap-test-end t) (trap-exceptions t) break-addr (debug-stop-debug-fn 'values))
  (unwind-protect
       (with-subtest (name)
         (when break-addr
           (with-subtest ((format-symbol :keyword "~A-BREAKPOINT-REACHED" name) :noncritical-p t)
             (add-sw-breakpoint core break-addr)
             (run-core-synchronous core :address start-address :exit-state :stop)
             (expect-core-fetch-address core (fetch (+ break-addr 8))
                 (breakpoint-not-reached :breakpoint (+ break-addr 4))))
           (funcall debug-stop-debug-fn core))
         (with-subtest ((format-symbol :keyword "~A-TEST-EXECUTED" name))
           (with-maybe-free-hardware-breakpoints (trap-test-end core) ((test-end (+ start-address (* 4 insn-count))))
             (with-maybe-free-hardware-breakpoints (trap-exceptions core) ((trap #xbfc00380))
               (run-core-synchronous core :address (unless break-addr start-address))
               (setf (state core) :debug)
               (lret ((success (funcall successp core)))
                 (when trap-test-end
                   (expect-core-fetch-address core (fetch (+ start-address (* 4 (1+ insn-count))))
                       (breakpoint-not-reached :breakpoint (+ start-address (* 4 insn-count)))
                     (funcall *failure-inspector*))))))))
    (clear-sw-breaks)))

(defun step-through-test (core name start-address insn-count successp &optional trap-test-end trap-exceptions &aux (*print-right-margin* 120) (*log-state-changes* nil))
  (declare (ignore trap-test-end trap-exceptions))
  (with-subtest ((format-symbol :keyword "~A-STEPTHROUGH-WAS-GOOD" name))
    (setf (saved-core-moment core) (make-neutral-moment core start-address))
    (dotimes (i insn-count)
      (step 1 :display nil))
    (funcall successp core)))

;; Succeeds on MC24, fails on NVCom-01.
;;;;
;;;; Run the packed tests.
;;;;

(defcomdbtest :mips hw-breakpoints () (core)
  (reset :core core)
  (with-bioable-mips-segment (core #x0)
    (emit-nops 4))
  (with-free-hardware-breakpoints (core) (break-0 break-1)
    (with-subtest :second-shadows-first
      (hw-break break-0 #x80000008)
      (hw-break break-1 #x8000000c)
      (run-core-synchronous core :address #x80000000)
      (expect-core-fetch-address core (fetch #x8000000c)
          (breakpoint-not-reached :breakpoint 0)))
    (with-subtest :second-shadows-first-2
      (hw-break break-1 #x80000008)
      (hw-break break-0 #x8000000c)
      (run-core-synchronous core :address #x80000000)
      (expect-core-fetch-address core (fetch #x8000000c)
          (breakpoint-not-reached :breakpoint 1)))
    (with-subtest :second-before-first
      (hw-break break-1 #x8000000c)
      (hw-break break-0 #x80000008)
      (run-core-synchronous core :address #x80000000)
      (expect-core-fetch-address core (fetch #x8000000c)
          (breakpoint-not-reached :breakpoint 0)))
    (with-subtest :first-after-second-undone
      (hw-break break-0 nil)
      (run-core-synchronous core :address #x80000000)
      (expect-core-fetch-address core (fetch #x80000010)
          (breakpoint-not-reached :breakpoint 1)))))

(defcomdbtest :mips watchpoints () (core &aux insns)
  (reset :core core)
  (unwind-protect
       (with-temporary-state (core :stop)
         (with-bioable-mips-segment (core #x0)
           (emit-nops 4)
           (emit-set-gpr :r1 #x0)
           (emit-set-gpr :r3 #x1000)
           (emit-nops 2)
           (setf insns (segment-emitted-insn-count *segment*))
           (emit-load32 :r1 #x0)
           (emit-load32 :r3 #x1000)
           (emit-nops 2)
           (emit-nops 6))
         (with-subtest :watchpoint-1-fired
           (settrace 1024)
           (watch 1 #x00001000)
           (run-core-synchronous core :address #x80000000)
           (expect (and (typep (core-stop-reason core) 'hardware-breakpoint)
                        (= 1 (trap-id (core-stop-reason core))))
               (breakpoint-not-reached :expected '(:hardware-break-watch 1) :actual (core-stop-reason core) :breakpoint 1)))
         (with-subtest :watchpoint-0-fired
           (settrace 1024)
           (watch 1 nil)
           (watch 0 #x00000000)
           (run-core-synchronous core :address #x80000000)
           (expect (and (typep (core-stop-reason core) 'hardware-breakpoint)
                        (= 0 (trap-id (core-stop-reason core))))
               (breakpoint-not-reached :expected '(:hardware-break-watch 0) :actual (core-stop-reason core) :breakpoint 0))))
    (hw-break 0 nil)
    (hw-break 1 nil)))

(defcomdbtest :mips sw-breakpoints () (core)
  (reset :core core)
  (emit-r1-fff-target+18-end+40-loading-sequence core #x0)
  (unwind-protect
       (with-free-hardware-breakpoints (core)
           ((target #x80000040)
            (trap #xbfc00380))
         (with-subtest :expect-fff-pattern
           (with-subtest :swbreak-stops
             (sw-break #x80000018)
             (run-core-synchronous core :address #x80000000)
             (expect-core-fetch-address core (fetch #x80000020)
                 (breakpoint-not-reached :breakpoint #x8000001c)))
           (with-subtest :test-executed
             (run-core-synchronous core)
             (expect-core-fetch-address core (fetch #x80000044)
                 (breakpoint-not-reached :breakpoint #x80000040)))
           (expect-value #xfff (gpr-by-name core :at))))
    (clear-sw-breaks)))

(defcomdbtest :mips run-through-jumpclear-full () (core)
  (run-test-pack core #'run-maybe-swbreak-and-dsd-test *runner-stepper-test-pack*
                 (lambda ()
                   (syncformat t "~&Test stop reason: ~S/~S~%" (core-stop-reason core) (devreg-decode core :cause)))))

(defcomdbtest :mips step-through-jumpclear-full () (core)
  (run-test-pack core #'step-through-test *runner-stepper-test-pack*))

(defcomdbtest :mips debug-stop-debug-jumpclear-swbreak-full () (core)
  (run-test-pack core #'run-maybe-swbreak-and-dsd-test *debug-stop-debug-test-pack*
                 (lambda ()
                   (syncformat t "~&Test stop reason: ~S/~S~%" (core-stop-reason core) (devreg-decode core :cause)))
                 #x20))

(defcomdbtest :mips debug-stop-debug-jumpclear-hwbreak-complex () (core)
  (reset :core core)
  (unwind-protect
       (progn
         (hw-break 0 #x80000040)
         (setc (state core) :debug)
         (emit-r1-complex-jumpclear core #x0)
         (hw-break 1 #xbfc00380)
         (with-subtest :r1-clear
           (with-subtest :prebranch-executed
             (hw-break 0 #x8000000c)
             (run-core-synchronous core :address #x80000000)
             (expect-core-fetch-address core (fetch #x80000010)
                 (breakpoint-not-reached :breakpoint #x8000000c)))
           (with-subtest :branch-executed
             (hw-break 0 #x8000001c)
             (run-core-synchronous core)
             (expect-core-fetch-address core (fetch #x80000020)
                 (breakpoint-not-reached :breakpoint #x8000001c)))
           (with-subtest :postdslot-executed
             (hw-break 0 #x80000040)
             (run-core-synchronous core)
             (expect-core-fetch-address core (fetch #x80000044)
                 (breakpoint-not-reached :breakpoint #x80000040)))
           (expect-value #x0 (gpr-by-name core :at))))
    (hw-break 0 nil)
    (hw-break 1 nil)))

(defcomdbtest-expected-failure :mips debug-stop-debug-jumpclear-swbreak-complex () (core)
  (reset :core core)
  (unwind-protect
       (progn
         (setc (state core) :debug)
         (emit-r1-complex-jumpclear core #x0)
         (hw-break 1 #xbfc00380)
         (sw-break #x8000000c)
         (sw-break #x8000001c)
         (sw-break #x80000040)
         (with-subtest :r1-clear
           (with-subtest :prebranch-executed
             (run-core-synchronous core :address #x80000000)
             (expect-core-fetch-address core (fetch #x80000014)
                 (breakpoint-not-reached :breakpoint #x80000010)))
           (with-subtest :branch-executed
             (run-core-synchronous core)
             (expect-core-fetch-address core (fetch #x80000024)
                 (breakpoint-not-reached :breakpoint #x80000020)))
           (with-subtest :postdslot-executed
             (run-core-synchronous core)
             (expect-core-fetch-address core (fetch #x80000048)
                 (breakpoint-not-reached :breakpoint #x80000044)))
           (expect-value #x0 (gpr-by-name core :at))))
    (hw-break 1 nil)
    (clear-sw-breaks)))

(defcomdbtest-unstable-failure :mips run-through-interrupt () (core)
  (run-test-pack core #'run-maybe-swbreak-and-dsd-test *interrupt-test-pack* 'values #x9))

(defcomdbtest-expected-failure :mips step-through-interrupt () (core)
  (run-test-pack core #'step-through-test *interrupt-test-pack*))

(defcomdbtest-unstable-failure :mips run-through-interrupt-loop () (core)
  (run-test-pack core #'run-maybe-swbreak-and-dsd-test *interrupt-loop-test-pack* 'values #x14))

(defcomdbtest-expected-failure :mips step-through-interrupt-loop () (core)
  (run-test-pack core #'step-through-test *interrupt-loop-test-pack*))

(defun manual-test (core address pack-test)
  (destructuring-bind (test-name test-emit-and-prepare-fn &rest more-test-args) pack-test
    (declare (ignore test-name more-test-args))
    (reset :core core :state :debug)
    (funcall test-emit-and-prepare-fn core address)
    (setf (saved-core-moment core) (make-neutral-moment core address))))
