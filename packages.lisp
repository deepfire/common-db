;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)


(defpackage #:options
  (:nicknames :opt)
  (:use :common-lisp)
  (:export
   ;; logging
   #:*log-stream*                       ; options
   #:*log-bus-events*                   ; bus
   #:*log-loadable-processing*          ; loadable
   #:*log-executable-processing*        ; executable
   #:*log-tap-register-access*          ; interface
   #:*log-graft-access*                 ; interface-parport
   #:*log-state-changes*                ; core
   #:*log-platform-processing*          ; platform
   #:*log-system-configuration*         ; platform-cpu
   #:*log-core-pipeline-crit*           ; mips
   #:*log-interface-bus-discovery*      ; common-db-core
   #:*verbose-interface-init*           ; interface

   #:*virtual-interface-enabled*        ; interface
   #:*virtual-target-enabled*           ; targets/virtual
   #:*disable-usb-interfaces*
   #:*skip-platform-init*               ; target
   #:*memory-detection-threshold*       ; platform-memory
   #:*orgify*                           ; common-db-tui
   #:*print-backtrace-on-errors*        ; common-db-tui
   ;;
   #:*explain*
   #:*examine-tlb*
   #:*map-to-zeroth-page*
   #:*keep-target-intact*
   #:*forced-platform*
   #:*force-memory-detection*
   #:*inhibit-memory-detection*
   #:*display*
   #:*watch*
   #:*watch-fn*))

(defpackage #:spaces                    ; standalone
  (:use :common-lisp :bitmop :device-model)
  (:shadowing-import-from :bitmop #:space)
  (:export
   ;; access
   #:interface
   #:platform
   #:target
   #:32bit-bus-target
   #:64bit-bus-target
   #:little-endian-target
   #:big-endian-target
   #:system
   ;; cores
   #:general-purpose-core
   #:little-endian-core
   #:big-endian-core
   #:mmu-core
   #:mips-core
   #:mipsel
   #:mipseb
   #:mips-mmu-core
   #:torn-pipeline-mips-core
   ;; memory
   #:cache
   #:memory
   #:internal-memory
   #:external-memory
   #:ram
   #:dcache
   #:icache
   #:scache
   ;; devices
   #:flash
   #:standard-flash
   ))

(defpackage #:bus ;; needed by interface host-pci
  (:use :common-lisp :alexandria :iterate :bitmop :device-model :pergamum
        :options :spaces)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:bus
   #:root-bus
   #:enumerating-bus
   #:probe-discovery-bus
   #:bus-name
   #:bus-device
   #:device-bus
   #:device-bus-id
   #:device-bus-address
   #:child-bus
   #:bus-error
   #:simple-bus-error
   #:bus-devices
   #:map-bus-devices
   #:do-bus-devices
   #:bus-probe-address
   #:bus-id-at-address
   #:bus-id-equalp
   #:bus-occupied-addresses
   #:bus-notice-survivor
   #:bus-add
   #:bus-populate-address
   #:bus-remove
   #:bus-scan))

(defpackage #:host-pci ;; needed by platform-pci
  (:use :common-lisp :cffi
        :options :spaces)
  (:export
   #:pci-system-init
   #:pci-system-cleanup
   #:pci-device-map-region
   #:pci-device-unmap-region
   #:pci-device-probe
   #:pci-device-next
   #:pci-device-find-by-slot
   #:pci-device-config-32bit
   #:+pci-match-any+
   #:pci-id-match
   #:pci-slot-match
   #:pci-id-match-iterator-create
   #:pci-slot-match-iterator-create
   #:pci-iterator-destroy
   #:pci-mem-region))

(defpackage #:graft ;; standalone
  (:use :common-lisp :alexandria :pergamum :setc :bitmop :device-model
        :options :portability :spaces)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:with-graft-io-capture
   #:set-port-forward
   #:set-port-reverse
   #:port-reset
   #:port-status
   #:port
   #:port-bit
   #:port-bits
   #:get-permission
   #:clear-epp-timeout
   #-disable-parport #:parport-error
   #-disable-parport #:parport-insufficient-privileges-error
   #-disable-parport #:parport-reset-error))

(defpackage #:bank ;; standalone
  (:use :common-lisp :alexandria :iterate :pergamum
        :options)
  (:export
   #:read-bank-extents
   #:u8-extent-print-as-bank
   #:print-bank-extents
   #:load-bank-extents
   #:write-extents-as-bank))

(defpackage #:generic ;; needed by target, platform, common-db, test-hardware
  (:use :common-lisp :pergamum :bitmop :device-model
        :options)
  (:shadowing-import-from :bitmop #:space)
  (:export
   ;; memory-device
   #:memory-device
   #:little-endian-memory-device
   #:big-endian-memory-device
   #:8bit-memory-device
   #:16bit-memory-device
   #:32bit-memory-device
   #:64bit-memory-device
   #:bioable-memory-device
   #:memory-device-byte-width
   #:memory-device-8bit-ref
   #:memory-device-8bit-set
   #:memory-device-16bit-ref
   #:memory-device-16bit-set
   #:memory-device-32bit-ref
   #:memory-device-32bit-set
   #:memory-device-64bit-ref
   #:memory-device-64bit-set
   #:memory-ref
   #:memory-set
   #:merge-u8-extremity
   #:bioable-memory-io
   ;; mapped-device
   #:mapped-device
   #:mapped-device-p
   #:mapped-device-base
   #:mapped-device-scale
   #:mapped-device-mutator-fn
   #:mapped-device-get-fn
   #:mapped-device-set-fn
   #:mapped-ref
   #:mapped-device-register-address
   #:mapped-reginstance-address
   ;; memory-region
   #:memory-region
   #:memory-region-extent
   #:slave-memory-region))

(defpackage #:address-map ;; needed-by mips, common-db
  (:use :common-lisp :pergamum
        :options)
  (:export
   #:page-align
   #:address-map
   #:make-address-map
   #:address-map-page-size
   #:address-map-add
   #:address-map-miss
   #:virt-to-phys
   #:addr-in-map-p
   #:phys-to-virt
   #:copy-mapping-by-virt-addr
   #:copy-mapping-by-phys-addr
   #:trim-address-map-virtual
   #:trim-address-map-physical
   #:map-address-map))

(defpackage #:loadable ;; needed by common-db
  (:use :common-lisp :alexandria :pergamum :iterate :bintype :elf
        :options :bank)
  (:export
   #:loadable
   #:loadable-entry-point
   #:loadable-sections
   #:loadable-load-size
   #:loadable-error
   #:make-loadable
   #:extract-loadable
   #:report-section
   #:report-checksummed-section
   #:dump-section
   #:upload-loadable))

(defpackage #:interface ;; needed by test-hardware
  (:nicknames :iface)
  (:use :common-lisp :alexandria :iterate :pergamum :bintype :setc :bitmop :device-model
        :options :portability :spaces :bus)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:interface-bus
   #-disable-virtcore #:virtif-bus
   #-disable-parport  #:parport-bus
   #:ezusb-bus
   #:interface
   #:iface-name
   #:iface-idcode
   #:iface-targets
   #:iface-version
   #:iface-fastio
   #:interface-error
   #:with-interface-error-trap-and-return
   #:simple-interface-error
   #:interface-status-timeout
   #:interface-memory-timeout
   #:persistent-interface-error
   #:interface-state-transition-timeout
   #:interface-debug-quiescence-timeout
   #:make-interface-for-device-name
   #:interface-reset
   #:interface-target-discrimination-tree
   #:interface-attach-target
   #:interface-stop-target
   #:stop-target-using-interface
   #:interface-reset-target
   #:reset-target-using-interface
   #:interface-close
   #:interface-bus-word
   #:interface-bus-io
   #:scan-interface-busses
   #:interfaces
   ))

(defpackage #:platform ;; needed by :test-hardware
  (:use :common-lisp :alexandria :pergamum :iterate :bintype :setc :bitmop :device-model
        :portability :options :spaces :generic)
  (:shadowing-import-from :bitmop #:space)
  (:export
   #:platform-target
   #:platform-memory-configurations
   #:platform-memory-configuration-order
   #:platform-memory-configuration
   #:platform-predefined-devices
   #:platform-memory-map
   #:define-platform
   #:all-platform-classes
   #:do-platforms
   #:platform-condition
   #:platform-error
   #:simple-platform-error
   #:platform-essentials-missing
   #:platform-invalid-core-frequence-multiplier
   #:platform-device
   #:platform-memory-region
   #:mapped-platform-memory-region
   ;;
   #:detect-platform-memory-size
   ;;
   #:device-platform
   #:configure-target
   #:configure-platform-system
   #:configure-platform-memory
   #:platform-address-region))

(defpackage #:tgt ;; needed by platform, core, system, common-db, test-hardware
  (:use :common-lisp :alexandria :pergamum :iterate :discrimination :isa :isa-mips :assem :assem-mips :setc :bitmop :device-model
        :options :portability :spaces :generic :platform :interface)
  (:shadow #:step)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   #:target-enumpool
   #:target-platform
   #:target-mapped-artifact-map
   #:find-target-class-for-interface
   #:detect-target-platform
   #:target-platform-discrimination-tree
   #:configure-target-platform
   #:target-artifact-by-address
   #:target-devices
   #:target-devices-by-type
   #:target-device
   ;; enumeration & its fruit
   #:add-target-device
   #:remove-target-device
   #:target-reginstance
   #:target-reg-addr
   #:target-reg
   #:set-target-reg
   ;;
   #:make-target-device
   #:create-target-device-from-spec
   #:with-target-devices
   #:busmem
   ;; conditions
   #:target-condition
   #:target-error
   #:unknown-target-device
   #:target-platform-detection-error
   ;; execution
   #:exec-raw
   ;; address space
   #:fixmap-address))

(defpackage #:core ;; needed by mips, common-db, test-hardware
  (:use :common-lisp :alexandria :pergamum :iterate :setc :bitmop :device-model :environment :allocation-pool :isa :isa-mips :assem :assem-mips
        :options :portability :spaces :address-map :generic :platform :tgt)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   ;; moment
   #:moment
   #:moment-fetch
   #:moment-opcode
   #:fetch
   #:opcode
   #:make-moment
   #:core-moment
   #:set-core-moment
   #:make-neutral-moment
   #:derive-moment
   #:reinstate-saved-moment
   ;; trail
   #:trail
   #:trail-decode
   #:core-trail
   #:set-core-trail
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
   #:core-moment-changed-p
   #:saved-core-trail
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
   #:save-core-pipeline
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
   #:tlb-entry
   #:set-tlb-entry
   #:decode-tlb-entry
   #:probe-tlb
   #:get-tlb
   #:set-tlb
   #:tlb-address-map
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
   #:core-physical-pages
   #:set-core-physical-pages
   #:core-virtual-pages
   #:set-core-virtual-pages
   #:state
   #:state-pc
   #:state-gpr
   #:state-regs
   #:state-fpr
   #:state-tlb
   #:state-page-size
   #:state-virtual-pages
   #:state-physical-pages
   #:state-physical-cells
   ;; actual meat
   #:capture-state
   #:apply-state
   #:write-state-to-stream
   #:write-state
   #:write-core-state
   #:read-state
   #:emit-nonmemory-state-restorer))

(defpackage #:sysdev ;; needed by common-db
  (:use :common-lisp :alexandria :pergamum :iterate :discrimination :isa :isa-mips :assem :assem-mips :setc :bitmop :device-model
        :options :portability :spaces :generic :platform :interface :tgt)
  (:shadow #:step)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   #:*memory-configurations*
   #:*memory-configuration-order*
   #:*memory-detection-threshold*
   #:memory-config
   #:make-memory-config
   #:memory-config-name
   #:memory-config-register-values
   #:do-memory-configs
   #:memory-config-valid-for-device-classes-p
   #:memory-config-valid-for-platform-p
   #:apply-memory-config
   #:test-target-memory
   #:platform-no-usable-memory-detected-error
   #:platform-no-usable-memory-detected
   #:configure-platform-memory))

(defpackage #:mips ;; needed by common-db, test-hardware
  (:use :common-lisp :alexandria :pergamum :iterate :setc :bitmop :device-model :isa :isa-mips :assem :assem-mips :address-map
        :options :spaces :generic :interface :tgt :core)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   ;; classes & accessors
   #:mips-core-bank
   #:pipeline
   #:mips-core-watchpoint-stop-skew
   #:mips-bank-gpr-shadow
   #:mips-bank-cop0-shadow
   #:torn-pipeline-mips-core-broken-by
   #:mips-software-breakpoint
   #:mips-hardware-breakpoint
   #:mips-state
   ;; address space
   #:kuseg
   ;; sane 32 bit
   #:kseg0
   #:kseg1
   #:kseg2
   #:kseg3
   #:seg32p
   #:kusegp
   #:ksegp
   #:kseg0p
   #:kseg1p
   #:kseg2p
   #:kseg3p
   #:remap-to-seg32
   #:remap-to-kuseg
   #:remap-to-kseg0
   #:remap-to-kseg1
   #:remap-to-kseg2
   #:remap-to-kseg3
   #:extent-to-seg32
   ;; sign-extended 32 bit
   #:x32kseg0
   #:x32kseg1
   #:x32kseg2
   #:x32kseg3
   #:x32seg32p
   #:x32kseg0p
   #:x32kseg1p
   #:x32kseg2p
   #:x32kseg3p
   #:remap-to-x32seg32
   #:remap-to-x32kseg0
   #:remap-to-x32kseg1
   #:remap-to-x32kseg2
   #:remap-to-x32kseg3
   #:extent-to-x32seg32
   ;; execution
   #:exec
   ;; unsorted
   #:patch-core-interface-pipeline-reginstances
   #:save-registers
   #:restore-registers
   #:mips-core-entrance-oscr
   ;; TLB
   #:mips-tlb-entry
   #:mips-tlb-entry-hi
   #:mips-tlb-entry-lo0
   #:mips-tlb-entry-lo1
   ;; state.lisp
   #:state-restorer-as-memory
   #:write-state-restorer-bank
   #:apply-bank
   ;; insanity-lurks-nearby.lisp
   #:+insane-trampoline-address+))

(defpackage #:dsp ;; needed by common-db, test-hardware
  (:use :common-lisp :alexandria :pergamum :iterate :setc :bitmop :device-model :isa :isa-mips :assem :assem-mips :address-map
        :options :spaces :generic :interface :tgt :core)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   ;; classes & accessors
   #:dsp
   #:dsp-software-breakpoint
   #:dsp-hardware-breakpoint))

(defpackage #:flash
  (:use :common-lisp :alexandria :iterate
        :pergamum
        :bintype
        :environment
        :isa :isa-mips :assem :assem-mips
        :setc :bitmop :device-model
        :portability :options :spaces :bus :address-map :loadable :generic :interface :platform :tgt :core
        :mips :sysdev)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   #:flash-error
   #:simple-flash-error
   #:intel-cmdset-flash
   #:amd-cmdset-flash
   #:stmaa-cmdset-flash
   ;;
   #:flash-base
   #:flash-size
   #:flash-code-seg
   #:probe-flash
   ))

(defpackage #:pci
  (:use :common-lisp :alexandria :iterate
        :pergamum
        :isa :isa-mips :assem :assem-mips
        :setc :bitmop :device-model
        :portability :options :spaces :bus :address-map :loadable :generic :interface :platform :tgt :core :mips)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:export
   #:pci-host
   #:pci-host-busses
   #:pci-host-probe-busses
   #:pci-bus
   #:pci-bus-number
   #:pci-bus-devices
   #:pci-device
   #:pci-device-slot
   #:pci-device-function
   #:pci-device-maps
   #:pci-device-config-register
   #:pci-device-map
   #:pci-device-map-number
   #:pci-device-unmap
   ))

(defpackage #:platform-definitions                  ; standalone
  (:nicknames :platdefs)
  (:use :common-lisp
        :pergamum
        :spaces :platform :tgt :sysdev :mips))

(defpackage #:common-db ;; needed by test-hardware
  (:nicknames :comdb)
  (:use :common-lisp :alexandria :iterate :pergamum :custom-harness :setc :bitmop :device-model :isa :isa-mips :assem :assem-mips
        :portability :options :spaces :bus :address-map :loadable :generic :interface :platform :tgt :core :sysdev
        :mips :dsp :flash)
  (:shadowing-import-from :isa #:disassemble)
  (:shadowing-import-from :bitmop #:space #:*space*)
  (:shadow #:get #:set #:step #:catch #:trace #:reset)
  (:export
   ;; re-export
   apropos describe load sleep quit
   ;;
   #:*syms*
   #:*initargs*
   #:*default-initargs*
   ;; target contexts
   #:target-context
   #:*target-contexts*
   #:*current*
   #:*interface*
   #:*target*
   #:*core*
   #:*examine-test*
   #:ctx-interface
   #:ctx-target
   #:ctx-core
   #:ctx-display-list
   #:do-all-target-contexts
   #:display-list
   #:default-display-list
   ;; for apps
   #:*additional-help-en*
   #:*additional-help-ru*))

(defpackage #:test-hardware ;; standalone
  (:nicknames :testhw)
  (:use :common-lisp :alexandria :pergamum :iterate :setc :bitmop :device-model :isa :isa-mips :assem :assem-mips :custom-harness
        :options :spaces :generic :platform :interface :tgt :core :sysdev :mips :dsp
        :common-db)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:shadowing-import-from :common-db #:get #:set #:reset #:trace #:catch #:step)
  (:export
   #:*failure-inspector*
   #:*log2-iota*
   #:*runner-stepper-test-pack*
   #:*interrupt-loop-test-pack*
   #:*interrupt-test-pack*
   #:*debug-stop-debug-test-pack*
   ;;
   #:unexpected-formatted-value
   #:breakpoint-not-reached
   #:invisibility-failure
   #:expect-formatted-value
   #:expect-core-fetch-address
   #:with-interface-cushion
   #:defcomdbtest
   #:defcomdbtest-expected-failure
   #:defcomdbtest-unstable-failure
   ;;
   #:emit-r1-complex-jumpclear
   #:emit-r1-fff-target+18-end+40-loading-sequence
   #:emit-r1-jumpclear-0x14-0x30
   #:emit-r1-jumpclear-0x14-0x30-jal
   #:emit-r1-jumpclear-0x14-0x30-jr
   #:emit-r1-jumpclear-0x14-0x30-dependent
   #:emit-setup-gpr-standalone-bits
   #:run-test-pack
   #:run-maybe-swbreak-and-dsd-test
   #:make-expect-r1
   #:production-stop-debug-stop))

(defpackage #:common-db-user
  (:nicknames :comdb-user)
  (:shadowing-import-from :bitmop #:space)
  (:shadowing-import-from :isa #:disassemble)
  (:shadowing-import-from :common-db #:get #:set #:reset #:trace #:catch #:step)
  (:use :common-lisp :alexandria :pergamum :iterate :setc :bitmop :device-model :isa :isa-mips :assem :assem-mips
        :options :spaces :generic :platform :interface :tgt :core :mips :dsp
        :common-db))

(defpackage #:mdb-emulation
  (:use :common-lisp :alexandria :iterate
        :options)
  (:shadow #:step #:catch #:trace))
