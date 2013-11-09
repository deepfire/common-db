;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)

(defpackage #:core ;; needed by mips, common-db, test-hardware
  (:use :common-lisp :alexandria :pergamum :iterate :setc :bitmop :device-model :environment :allocation-pool :isa :isa-mips :assem :assem-mips
        :options :portability :spaces :address-map :generic :platform :tgt)
  (:shadowing-import-from :options #:arg)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   ;; addressing
   #:core-bus-address
   ;; moment
   #:moment
   #:moment-fetch
   #:moment-opcode
   #:fetch
   #:opcode
   #:make-moment
   #:current-core-moment
   #:set-current-core-moment
   #:make-neutral-moment
   #:derive-moment
   #:save-core-moment
   #:restore-core-moment
   #:reinstate-saved-moment
   ;; trail
   #:trail
   #:trail-decode
   #:trail-execute
   #:current-core-trail
   #:set-current-core-trail
   #:save-core-trail
   #:restore-core-trail
   #:make-neutral-trail
   #:listify-trail
   #:parse-trail
   ;; pc
   #:pc
   ;; core classes and accessors
   #:core
   #:core-isa
   #:core-nopcode
   #:core-instruction-counter
   #:core-stop-reason
   #:saved-core-moment
   #:core-moment
   #:set-core-moment
   #:core-moment-changed-p
   #:saved-core-trail
   #:core-trail
   #:set-core-trail
   #:core-trail-important-p
   #:slave-core
   #:slave-master
   #:general-purpose-core
   #:core-slaves
   #:core-executable
   #:core-insn-execution-limit
   #:do-core-traps
   #:do-core-controlled-traps
   #:do-core-vector-traps
   #:do-core-hardware-breakpoints
   #:do-core-software-breakpoints
   #:coerce-to-trap
   #:traps
   #:hwbreak
   ; over the id-keyed pool
   #:hw-breakpoint
   #:default-sw-breakpoint-type
   #:default-hw-breakpoint-type
   #:mmu-core
   #:core-tlb-entries-nr
   ;; conditions
   #:core-condition
   #:core-error
   #:simple-core-error
   #:no-core-breakpoint
   #:core-halt-failure
   #:invalid-core-frequency-multiplier
   #:core-execution-error
   #:unexpected-stop-reason
   ;; GPR
   #:gpr
   #:set-gpr
   #:gpr-by-name
   #:set-gpr-by-name
   ;; FPR
   #:fpr
   #:set-fpr
   #:fpr-by-name
   #:set-fpr-by-name
   ;; pipeline
   #:finish-core-pipeline
   #:patch-core-pipeline-reginstances
   #:core-pipeline-addresses
   #:default-core-pc
   #:print-pipeline
   #:print-pipeline-terse
   ;; initialisation
   #:reset-platform
   ;; state query and management
   #:core-running-p
   #:step-core-asynchronous
   #:step-core-synchronous
   #:step-core-debug
   #:reset-core
   #:free-to-stop
   #:stop-to-debug
   #:debug-to-stop
   #:stop-to-free
   #:poll-core-interruptible
   #:analyse-core
   #:wait-core
   #:core-enter-debug
   #:prime-core-executable
   #:run-core-asynchronous
   #:run-core-synchronous
   #:state
   #:with-state
   #:with-temporary-state
   #:with-maybe-temporary-state
   #:with-retry-with-state-restart
   #:retry-with-state
   #:deeper-state
   #:shallower-state
   ;; stop reasoning
   #:core-stop-reason
   #:user-interruption
   #:trap
   #:controlled-trap
   #:address-trap
   #:skippable-trap
   #:enumerated-trap
   #:volatile-address-trap
   #:vector-trap
   #:memory-access-trap
   #:instruction-count-trap
   #:intercore-trap
   #:trap-core 
   #:trap-causing-core
   #:trap-enabled-p
   #:trap-address
   #:trap-skipcount
   #:trap-id
   #:enable-trap
   #:disable-trap
   #:deduce-stop-reason
   #:breakpoint
   #:hardware-trap
   #:trap-owned-p
   #:software-breakpoint
   #:software-breakpoint-saved-insn
   #:set-core-insn-execution-limit
   #:invoke-with-traps
   #:with-traps
   #:recognise-sw-breakpoint
   #:add-sw-breakpoint
   #:setup-hw-trap
   #:add-hw-breakpoint
   #:add-cell-watchpoint
   #:allocate-hardware-breakpoint
   #:invoke-with-maybe-free-hardware-breakpoints
   #:with-maybe-free-hardware-breakpoints
   #:with-free-hardware-breakpoints
   ;; TLB
   #:address-mapped-p
   #:current-page-size
   #:tlb-active-p
   #:tlb
   #:tlb-entry
   #:make-clear-tlb-entry
   #:set-tlb-entry
   #:decode-tlb-entry
   #:listify-tlb-entry
   #:parse-tlb-entry
   #:probe-tlb
   #:get-tlb
   #:set-tlb
   #:tlb-address-map
   ;; clearing
   #:clear-core
   #:clear-core-gprs
   #:clear-core-fpu
   #:clear-core-tlb
   #:clear-core-sysregs
   ;; frequency multipliers
   #:core-frequency-multiplier
   #:default-core-frequency-multiplier
   #:core-frequency-multiplier-valid-p
   ;; slaves
   #:freeze-core-slaves
   #:thaw-core-slaves
   ;; miscellaneous
   #:flush-core-instruction-cache
   #:flush-core-data-cache
   #:core-report
   #:core-call-stack
   #:capture-instruction-counters
   #:restore-instruction-counters
   #:reset-instruction-counters
   ;; execution
   #:trace-segment
   #:execute-segment
   #:with-executed-segment
   #:core-trans-stack-trace
   #:watch-core
   #:prepare-trans-args
   #:trans-funcall
   #:trans-funcall*
   #:*trace-trans-calls*
   #:*disasm-trans-calls*
   #:with-traced-trans-calls
   #:with-maybe-traced-trans-calls
   #:with-trans-funcallability
   #:handle-execution-error
   ;; disassembly
   #:core-disassemble
   #:default-disassembly-line-printer
   ;; state.lisp
   #:physical-pages
   #:virtual-pages
   #:page-size
   #:core-physical-pages
   #:set-core-physical-pages
   #:core-virtual-pages
   #:set-core-virtual-pages
   #:state
   #:state-moment
   #:state-trail
   #:state-gpr
   #:state-regs
   #:state-fpr
   #:state-tlb
   #:state-page-size
   #:state-virtual-pages
   #:state-physical-pages
   #:state-physical-cells
   #:physical-cells
   #:regs
   ;; actual meat
   #:capture-state-using-state
   #:capture-state
   #:apply-state
   #:apply-bank
   #:write-state-to-stream
   #:write-state
   #:write-core-state
   #:read-state-for-core
   #:emit-state-restorer
   #:state-restorer-extent
   #:write-state-restorer-bank))

