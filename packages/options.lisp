;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(common-lisp:in-package #:common-lisp-user)


(defpackage #:options
  (:nicknames :opt)
  (:use :common-lisp)
  (:export
   ;;
   ;; The all-knowing, all-seeing oracle.
   #:*initargs*
   ;;
   ;; Logging
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
   #:*trace-exchange*                   ; all network code
   ;;
   ;; Grab bag
   #:*virtual-interface-enabled*        ; interface
   #:*virtual-target-enabled*           ; targets/virtual
   #:*manual-memory-config*             ; 
   #:*orgify*                           ; common-db-tui
   #:*map-to-zeroth-page*
   ;;
   ;; UI
   #:*explain*
   #:*examine-tlb*
   #:*display*
   #:*watch*
   #:*watch-fn*
   ;;
   ;; Functional interface to *initargs*
   #:args
   #:arg
   #:set-arg))

