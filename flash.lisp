;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLASH; Base: 10; indent-tabs-mode: nil -*-
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

(in-package :flash)


(define-namespace :flash
  (:documentation "Flash memory")
  (:register-formats
   (:micron-mfgr-id ""
     (:mfgr 8 0 ""
        ((#x89 :intel)
         (#x2c :micron))))
   (:micron-device-id ""
     (:size 8 16 ""
        ((#x16 :32mb)
         (#x17 :64mb)
         (#x18 :128mb)
         (#x1d :256mb))))
   (:micron-locked-id ""
     (:locked 1 0 ""))
   (:micron-mt28fxxxxx-cbl-error-codes ""
     (:micron-mt28fxxxxx-cbl-error-codes 32 0 ""
        ((#x00300030 :invalid-clear-block-lock-sequence "")
         (#x00280028 :vpen<vpenlk ""))))
   (:micron-mt28fxxxxx-status ""
     (:dev-prot-enabled 1 1 "")
     (:prog-suspended 1 2 "")
     (:low-prog-voltage 1 3 "")
     (:err-prog-or-set-lock 1 4 "")
     (:err-erase-or-clear-lock 1 5 "")
     (:erase-suspended 1 6 "")
     (:write-machine-ready 1 7 ""))
   (:cfi-command "JEDEC 68.01 command register format"
     (:cfi-opcode 8 0 ""
        ((#x90 :id                          "JEDEC ID")
         (#x98 :query                       "Query access")
         (#xf0 :exit)
         (#xff :read-array))))
   (:intel-command "CFI command sets 001, 003 200"
     (:intel-opcode 8 0 ""
        ((#x01 :cyc2-set-block-lock)
         (#x10 :word/byte-program-v1)
         (#x1a :program-status-mask         "NMITD")
         (#x20 :block-erase-begin)
         (#x38 :block-lock-status-mask      "NMITD")
         (#x40 :word/byte-program-v2)
         (#x50 :clear-status)
         (#x60 :begin-change-block-lock)
         (#x70 :read-status)
         (#x80 :quiescence                  "NMITD")
         (#xb0 :block-erase/program-suspend)
         (#xb8 :configuration)
         (#xc0 :protection-program)
         (#xd0 :finalize-block-erase/program-resume/cyc2-clear-block-lock/cyc2-block-erase "")
         (#xe8 :write-to-buffer))))
   (:amd-command "CFI command set 002"
     (:amd-opcode 8 0 ""
        ((#x10 :cyc2-erase-chip)
         (#x30 :cyc2-erase-block)
         (#x50 :cyc2-erase-sector)
         (#x80 :cyc1-erase)
         (#x85 :user-secid/prog-lockout)
         (#x88 :query-secid)
         (#xa0 :word-program)
         (#xa5 :user-secid/word-program))))
   #+(or)
   (:staa-command "CFI command set 020"
     (:staa-opcode 8 0 ""
        ()))))

(defbintype cfi-erase-region ()
  (:documentation "JEDEC 68.01 Erase Region Info structure")
  (:type :structure)
  (:prefix er-)
  (:fields
   (expr  region-blocks          (unsigned-byte 16) (unsigned-byte 17) (1+ _) :doc "erase region block count")
   (expr  block-size             (unsigned-byte 16) (unsigned-byte 28) (let ((x (ash _ 8))) (if (zerop x) #x7f x)) :doc "erase block size")))

(defbintype cfi-query ()
  (:documentation "JEDEC 68.01 Query structure")
  (:type :structure)
  (:prefix query-)
  (:fields
   (match magic                        (terminated-string 3 0) (("QRY" t)) :ignore t)
   (value primary-algo                 (unsigned-byte 16))
   (match primary-algo-vendor          (pure (unsigned-byte 16) (path-value *self* 'primary-algo))
                                       (((#x0001 #x0003 #x0200) 'intel-cmdset-flash)
                                        (#x0002 'amd-cmdset-flash)
                                        (#x0020 'stmaa-cmdset-flash))
                                       :doc "primary algorithm command set and control interface id code")
   (value primary-query-offt           (unsigned-byte 16) :doc "address for primary algorithm extended query")
   (value alternate-algo               (unsigned-byte 16) :doc "alternative algorithm command set and control interface id code")
   (value alternate-query-offt         (unsigned-byte 16) :doc "address for alternative algorithm extended query")
   (expr  vcc-min                      (bcd 8) rational (/ _ 10) :doc "Vcc logic supply minimum program/erase or write voltage, volts.")
   (expr  vcc-max                      (bcd 8) rational (/ _ 10) :doc "Vcc logic supply maximum program/erase or write voltage, volts.")
   (expr  vpp-min                      (bcd 8) rational (/ _ 10) :doc "Vpp programming supply minimum program/erase voltage, volts.")
   (expr  vpp-max                      (bcd 8) rational (/ _ 10) :doc "Vpp programming supply maximum program/erase voltage, volts.")
   (expr  timeo-cell-typical           (unsigned-byte 8) (unsigned-byte 28) (expt 2 _) :doc "typical timeout for 1-cell program, 2^x ms")
   (expr  timeo-maxprog-typical        (unsigned-byte 8) (unsigned-byte 28) (expt 2 _) :doc "typical timeout for multi-byte program, 2^x ms")
   (expr  timeo-block-erase-typical    (unsigned-byte 8) (unsigned-byte 28) (expt 2 _) :doc "typical timeout for block erase, 2^x ms")
   (expr  timeo-full-erase-typical     (unsigned-byte 8) (unsigned-byte 28) (expt 2 _) :doc "typical timeout for chip erase, 2^x ms")
   (expr  timeo-cell-max-factor        (unsigned-byte 8) (unsigned-byte 28) (expt 2 _) :doc "max timeout for 1-cell program, 2^x * typical")
   (expr  timeo-maxprog-max-factor     (unsigned-byte 8) (unsigned-byte 28) (expt 2 _) :doc "max timeout for multi-byte program, 2^x * typical")
   (expr  timeo-block-erase-max-factor (unsigned-byte 8) (unsigned-byte 28) (expt 2 _) :doc "max timeout for block erase, 2^x * typical")
   (expr  timeo-full-erase-max-factor  (unsigned-byte 8) (unsigned-byte 28) (expt 2 _) :doc "max timeout for chip erase, 2^x * typical")
   (expr  size                         (unsigned-byte 8) (unsigned-byte 28) (expt 2 _) :doc "device size, 2^x")
   (value dev-interface-code           (unsigned-byte 16) :doc "flash device interface code description (ref. JEP137)")
   (expr  max-multibyte                (unsigned-byte 16) (unsigned-byte 28) (expt 2 _) :doc "max 2^x bytes per multi-byte program")
   (value erase-block-region-count     (unsigned-byte 8) :doc "erase block region count")
   (value erase-block-regions	       (sequence (path-value *self* 'erase-block-region-count)
                                                 :element-type cfi-erase-region :stride 32 :format :vector))))

(define-protocol-device-class flash :flash (bioable)
  ((base :accessor flash-base :type (integer 0) :initarg :base)
   (size :accessor flash-size :type (integer 0) :initarg :size)
   (bank-width :accessor flash-bank-width :type (integer 1 4) :initarg :bank-width :documentation "Bank width in octets.")
   (type :accessor flash-type :type (integer 1 4) :initarg :type)
   (interleave :accessor flash-interleave :type (integer 1 4) :initarg :interleave)
   (reader-fn :accessor flash-reader-fn)
   (writer-fn :accessor flash-writer-fn)
   (emit-reader-fn :accessor flash-emit-reader-fn)
   (emit-composer-fn :accessor flash-emit-composer-fn)
   (emit-writer-fn :accessor flash-emit-writer-fn)
   (preferred-erase-block-size :accessor flash-preferred-erase-block-size)
   (query :accessor flash-query :initarg :query))
  (:documentation
   "Protocol class for flash devices. The backend must conform to the
MEMORY-DEVICE protocol.")
  (:default-initargs
   :size 0
   :enumeration-class 'flash
   :query nil))

(define-condition flash-error (error)
  ((flash :accessor condition-flash :type flash :initarg :flash)
   (address :accessor condition-address :type (integer 0) :initarg :address)))

(define-simple-error flash-error :object-initarg :flash)

(define-reported-condition flash-quiescence-failure (flash-error) ()
  (:report (address flash) "~@<Quiescence failure at address ~8,'0X of ~S~:@>" address flash))

(define-reported-condition flash-status-error (flash-error)
  ((status :accessor condition-status :type (integer 0) :initarg :status))
  (:report (status address flash) "~@<Status ~S at address ~8,'0X of ~S~:@>" status address flash))

(define-reported-condition flash-write-protected (flash-status-error) ()
  (:report (address flash) "~@<Write-protected block at address ~8,'0X of ~S~:@>" address flash))

(define-reported-condition invalid-set-or-clear-lock-command (flash-status-error) ()
  (:report (address flash) "~@<Invalid SET LOCK or CLEAR LOCK command sequence at address ~8,'0X of ~S~:@>" address flash))

;;;
;;; Subclasses
;;;
(define-device-class standard-flash :flash (flash) ())

(define-device-class transprogrammed-flash :flash (standard-flash)
  ((as :accessor flash-as)
   (compilation-environment :accessor flash-compilation-environment)
   (code-seg :accessor flash-code-seg :initarg :code-seg)))

(define-device-class intel-cmdset-flash :flash (transprogrammed-flash standard-flash) ())
(define-device-class amd-cmdset-flash :flash (transprogrammed-flash standard-flash) ())
(define-device-class stmaa-cmdset-flash :flash (standard-flash) ())

(define-print-object-method ((o standard-flash :unbound-slot-value nil) bank-width type interleave query size base preferred-erase-block-size)
    "~@<#<~;~A bank: ~Dbit, type: ~Dbit, interleave: ~D, query: ~A~:[~;, #x~:*~X bytes~] @ ~8,'0X~:[~;, preferred erase block size: #x~:*~X bytes~]>~:@>"
  (type-of o) (when bank-width (ash bank-width 3)) (when type (ash type 3)) interleave query
  size base preferred-erase-block-size)

;;;
;;; API
;;;
(defgeneric prepare (flash))
(defgeneric lock (flash extent-spec))
(defgeneric unlock (flash extent-spec))
(defgeneric block-locked-p (flash addr))
(defgeneric lock-block (flash addr))
(defgeneric unlock-block (flash addr))
(defgeneric erase-sector (flash address))
(defgeneric erase-block (flash address))
(defgeneric erase-chip (flash))
(defgeneric program-sector (flash address vector))
(defgeneric program-block (flash address vector))
(defgeneric program-aligned-u8-extent (flash extent &optional compute-csums print-csums))
(defgeneric program-unaligned-u8-extent (flash spec extent block-size))
;;;
;;; Transprogrammed flashes
;;;
(defgeneric emit-flash-region-writer (flash base))

;;;
;;; Machinery
;;;
(set-namespace :flash)

(defun make-cfi-command-function (f bank-width interleave)
  "Given a flash device F, BANK-WIDTH and INTERLEAVE, produce three functions,
   reader, command composer and a writer, as multiple values.
   The reader function accepts one argument, access base.
   The writer function accepts an additional argument, the CFI opcode."
  (declare (type (integer 1 4) bank-width interleave))
  (let ((memory-device (backend f))
        (map-base (flash-base f)))
    (macrolet ((frob-composer (i b)
                 `(lambda (id &aux (op (bits :cfi-opcode id)))
                    ,@(when (> i 2) `((logiorf op (ash op ,(ash (/ b i) 4)))))
                    ,@(when (> i 1) `((logiorf op (ash op ,(ash (/ b i) 3)))))
                    op))
               (frob-writer (i b)
                 `(lambda (reg id &aux (op (bits :cfi-opcode id)) (base (if (integerp reg) reg (register-selector (space :flash) :cfi reg))))
                    ,@(when (> i 2) `((logiorf op (ash op ,(ash (/ b i) 4)))))
                    ,@(when (> i 1) `((logiorf op (ash op ,(ash (/ b i) 3)))))
                    ,(case b
                           (1 `(memory-device-8bit-set memory-device (+ map-base (ash base 0)) op))
                           (2 `(memory-device-16bit-set memory-device (+ map-base (ash base 1)) op))
                           (4 `(memory-device-32bit-set memory-device (+ map-base (ash base 2)) op)))))
               (frob-writer-emitter (i b)
                 `(lambda (addr id)
                    (declare (ignorable addr id))
                    (emit-set-gpr :temp (if (keywordp id)
                                            id
                                            (bits :cfi-opcode id)))
                    ,@(when (> i 2)
                            `((emit* :sll :temp2 :temp ,(ash (/ b i) 4))
                              (emit* :or :temp :temp2 :temp)))
                    ,@(when (> i 1)
                            `((emit* :sll :temp2 :temp ,(ash (/ b i) 3))
                              (emit* :or :temp :temp2 :temp)))
                    (emit* :nop)
                    ,@(ecase b
                             ((1 2) nil)
                             (4 `((if (keywordp addr)
                                      (progn
                                        (emit* :sll addr addr 2)
                                        (emit* :add addr addr :flash-base)
                                        (emit* :sw :temp 0 addr))
                                      (emit* :sw :temp (ash addr 2) :flash-base)))))))
               (frob-composer-emitter (i b)
                 `(lambda (value)
                    ,@(when (> i 2)
                            `((emit* :sll :temp value ,(ash (/ b i) 4))
                              (emit* :or value :temp value)))
                    ,@(when (> i 1)
                            `((emit* :sll :temp value ,(ash (/ b i) 3))
                              (emit* :or value :temp value)))
                    ,@(when (= i 1) `((declare (ignore value))))))
               (frob-frobber (frobber)
                 `(ecase interleave
                    (4 (ecase bank-width
                         (4 (,frobber 4 4))))
                    (2 (ecase bank-width
                         (4 (,frobber 2 4))
                         (2 (,frobber 2 2))))
                    (1 (ecase bank-width
                         (4 (,frobber 1 4))
                         (2 (,frobber 1 2))
                         (1 (,frobber 1 1)))))))
      (values (ecase bank-width
                (1 (lambda (reg)
                     (memory-device-8bit-ref memory-device (+ map-base (ash reg 0)))))
                (2 (lambda (reg)
                     (memory-device-16bit-ref memory-device (+ map-base (ash reg 1)))))
                (4 (lambda (reg)
                     (memory-device-32bit-ref memory-device (+ map-base (ash reg 2))))))
              (frob-frobber frob-composer)
              (frob-frobber frob-writer)
              (ecase bank-width
                ((1 2) nil)
                (4 (lambda (base dest-reg)
                     (emit* :lw dest-reg base :flash-base)
                     (emit* :nop))))
              (frob-frobber frob-composer-emitter)
              (frob-frobber frob-writer-emitter)))))

(defun flash-write (f addr cmd)
  (funcall (flash-writer-fn f) addr cmd))

(defun flash-read (f addr)
  (funcall (flash-reader-fn f) addr))

(defun issue-command (f addr command)
  (flash-write f #x555 #xaa)
  (flash-write f #x2aa #x55)
  (flash-write f addr command))

(defun emit-flash-read (f addr dest-reg)
  (funcall (flash-emit-reader-fn f) addr dest-reg))

(defun emit-flash-compose (f cmdreg)
  (funcall (flash-emit-composer-fn f) cmdreg))

(defun emit-flash-write (f addr cmd)
  (funcall (flash-emit-writer-fn f) addr cmd))

(defun probe-flash (f)
  "Given a flash device F (whose BANK-WIDTH slot must be bound),
detect its geometry. This includes interleave, type and command builder 
function. In case of success, return a parsed Query structure, and NIL
otherwise."
  (let ((bank-width (flash-bank-width f)))
    (flet ((read-query-8 (f reg)
             (logand #xff (flash-read f reg)))
           (sense-qry (reader-fn composer-fn)
             (let ((qry (mapcar reader-fn '(#x10 #x11 #x12)))
                   (expected (mapcar (compose composer-fn #'char-code) '(#\Q #\R #\Y))))
               (equal qry expected))))
      (iter (for chip-count initially 4 then (ash chip-count -1))
            (until (zerop chip-count))
            (let ((interleave chip-count))
              (iter (for type initially (/ bank-width chip-count) then (ash type 1))
                    (while (<= type 4))
                    (multiple-value-bind (reader composer writer reader-emitter composer-emitter writer-emitter) (make-cfi-command-function f bank-width interleave)
                      ;; (funcall writer #x00 :exit)
                      ;; (funcall writer #x00 :read-array)
                      ;; (funcall writer #x55 :query)
                      (funcall writer #x0 #xf0)
                      (funcall writer #x555 #xaa)
                      (funcall writer #x2aa #x55)
                      (funcall writer #x555 :query)
                      ;; (format t "~&after QUERY command:~%")
                      ;; (dotimes (i (ash #x1000 (- bank-width)))
                      ;;   (format t "@~X: ~X~%" (ash i bank-width) (funcall reader i)))
                      (when (sense-qry reader composer)
                        (setf (flash-interleave f) interleave
                              (flash-reader-fn f) reader
                              (flash-writer-fn f) writer
                              (flash-emit-reader-fn f) reader-emitter
                              (flash-emit-composer-fn f) composer-emitter
                              (flash-emit-writer-fn f) writer-emitter
                              (flash-type f) type
                              (flash-query f)
                              (let ((length (+ #x1d (* 4 (read-query-8 f #x2c))))) ;; erase block region data
                                (parse 'cfi-query (map 'vector (curry #'read-query-8 f) (iota length :start #x10)) :error-p nil)))
                        (unless (and (flash-query f)
                                     (plusp (query-erase-block-region-count (flash-query f))))
                          (return-from probe-flash nil))
                        ;; (flash-write f #x0 #xf0)
                        ;; (issue-command f #x555 :id)
                        ;; (flash-write f #x0 #xf0)
                        ;; (flash-write f #x0 #xff)
                        (issue-command f #x555 :exit)
                        (flash-write f #x555 :read-array)
                        (let* ((preferred-region (first (sort (copy-sequence 'list (query-erase-block-regions (flash-query f))) #'< :key #'er-block-size)))
                               (size (* interleave (query-size (flash-query f))))
                               #+(or)
                               (new-base (logandc1 (1- size) (flash-base f))))
                          (setf (flash-size f) size)
                          #+(or)
                          (unless (= (flash-base f) new-base)
                            (warn "~@<Rebasing ~S from #x~X to #x~X.~:@>" f (flash-base f) new-base))
                          (setf #+(or) (flash-base f) #+(or) new-base
                                (flash-preferred-erase-block-size f) (* (flash-interleave f) (er-block-size preferred-region))))
                        (change-class f (query-primary-algo-vendor (flash-query f)))
                        (return-from probe-flash (flash-query f))))))))))

(defmethod read-block ((o flash) address vector &optional start end)
  (read-block (backend o) (+ (flash-base o) address) vector start end))

(defmacro with-unlocked-flash ((flash extent-spec) &body body)
  (once-only (flash extent-spec)
    `(progn
       (unlock ,flash ,extent-spec)
       (unwind-protect
            (progn ,@body)
         (lock ,flash ,extent-spec)))))

;;;
;;; ...INTMEM start ->|...code...|... ... ... ... unallocated ...... ... ...|...stack-allocation...|<- end INTMEM...
;;;                ...|...code+entry...|align-10|...io-chunk...|...unused...|...stack-allocation...|...
;;;
(defmethod prepare ((o transprogrammed-flash) &aux
                    (target (backend o))
                    (intmem (target-device target '(internal-memory 0)))
                    (extent (extent-to-seg32 (flash-code-seg o) (memory-region-extent intmem))))
  (with-trans-funcallability (cenv as (make-instance 'address-space :extent extent) :adjust-data-segment t)
      (nth-value 1 (emit-flash-region-writer o (base extent)))
    (setf (size (as-data as)) (ash 1 (1- (integer-length (size (as-data as)))))
          (flash-as o) as
          (flash-compilation-environment o) cenv)))

(defmethod program-aligned-u8-extent ((o transprogrammed-flash) extent &optional compute-csums print-csums &aux
                                      (target (backend o))
                                      (core (target-device target '(general-purpose-core 0)))
                                      (dataseg (as-data (flash-as o))))
  #+disable-ironclad
  (declare (ignore print-csums))
  (let ((io-chunk (min (size dataseg) (size extent)))
        (*log-state-changes* nil))
    (iter (for piece-offset from 0 below (size extent) by io-chunk)
          (for flash-address = (+ (flash-base o) (base extent) piece-offset))
          (for iolen = (min io-chunk (- (size extent) piece-offset)))
          (write-block target (base dataseg) (extent-data extent) piece-offset (+ piece-offset iolen))
          (trans-funcall* core (flash-compilation-environment o) (flash-as o)
                          :program-region flash-address (base dataseg) (ash iolen -2) `(:flash-base . ,(flash-base o)))
          (when compute-csums
            #+disable-ironclad
            (error "~@<COMMON-DB was built without IRONCLAD: checksumming not supported.~:@>")
            #-disable-ironclad
            (let ((iovec (make-array iolen :element-type '(unsigned-byte 8)))
                  (w-csum (ironclad:digest-sequence :md5 (extent-data extent) :start piece-offset :end (+ piece-offset iolen))))
              (flash-write o 0 :read-array)
              (read-block target flash-address iovec)
              (let* ((r-csum (ironclad:digest-sequence :md5 iovec))
                     (mismatch (not (equalp w-csum r-csum))))
                (when (or print-csums mismatch)
                  (format t "~&~5,' X @ ~8,'0X: w ~A, r ~A, ~:[OK~;FAIL~]~%"
                          iolen flash-address
                          (digest-as-string w-csum) (digest-as-string r-csum)
                          mismatch))))))))

(defmethod erase-block ((o transprogrammed-flash) address &aux
                        (target (backend o))
                        (core (target-device target '(general-purpose-core 0)))
                        (*log-state-changes* nil))
  (trans-funcall* core (flash-compilation-environment o) (flash-as o)
                  :erase-sector (+ (flash-base o) address) 0 0 `(:flash-base . ,(flash-base o))))

(defmethod write-u8-extents ((f transprogrammed-flash) extents &key preserve-holes before-fn (stream t) compute-csums print-csums
                             &aux (before-fn (or before-fn #'values)))
  "Write a list of non-intersecting EXTENTS into transprogrammable flash F.
When PRESERVE-HOLES is non-nil, the area around extents is preserved."
  (prepare f)
  (when extents
    (let* ((sorted (sort (copy-list extents) #'< :key #'base))
           (first-cell (base (first sorted)))
           (just-past-last-cell (end (lastcar sorted)))
           (granularity (flash-preferred-erase-block-size f))
           split-tail)
      (labels ((align (x) (logandc1 (1- granularity) x))
               (tail (x) (logand (1- granularity) x))
               (alignedp (x) (zerop (tail x)))
               (first-unaffected-extent () (if (alignedp just-past-last-cell)
                                               just-past-last-cell
                                               (+ (align just-past-last-cell) granularity)))
               (in-granule-p (base x)
                 (and (>= x base) (< x (+ base granularity)))))
        (iter (for granule-base from (align first-cell) below (first-unaffected-extent) by granularity)
              (let* ((pure-hits (remove-if-not (curry #'in-granule-p granule-base) sorted :key #'base))
                     (hits (append split-tail pure-hits)))
                (when hits
                  (dolist (pure-hit pure-hits)
                    ;; Mainly to notify the user
                    (funcall before-fn stream pure-hit))
                  (setf split-tail nil)
                  (let ((last-hit (lastcar hits)))
                    (when-let ((last-hit-split-p (not (or (in-granule-p granule-base (end last-hit))
                                                          (alignedp (end last-hit))))))
                      (removef hits last-hit)
                      (with-split-extent (split-in split-for-next) last-hit (- granularity (tail (base last-hit)))
                        (push split-for-next split-tail)
                        (appendf hits (list split-in))))
                    (if (and preserve-holes
                             ;; Not a single, properly-aligned, granularity-sized extent.
                             (not (and (null (cdr hits))
                                       (alignedp (base (first hits)))
                                       (= granularity (size (first hits))))))
                        (let ((composite-extent (make-extent
                                                 'extent granule-base
                                                 (apply #'concatenate '(array (unsigned-byte 8) 1)
                                                        (append
                                                         (unless (alignedp (base (first hits)))
                                                           (list (extent-data (u8-extent f (extent granule-base (- (base (first hits)) granule-base))))))
                                                         (iter (for (hit . rest) on hits)
                                                               (collect (extent-data hit))
                                                               (let ((inter-spec (extent (end hit) (- (if-let ((next (first rest)))
                                                                                                        (base next)
                                                                                                        (+ granule-base granularity))
                                                                                                      (end hit)))))
                                                                 (when (plusp (cdr inter-spec))
                                                                   (collect (extent-data (u8-extent f inter-spec)))))))))))
                          (erase-block f granule-base)
                          (program-aligned-u8-extent f composite-extent compute-csums print-csums))
                        (progn
                          (erase-block f granule-base)
                          (dolist (hit hits)
                            (program-aligned-u8-extent f (align-extend-u8-extent 4 hit) compute-csums print-csums))))))))))))

(defvar *poison-flash-writer-stack* t)

;;;
;;; Implementation: AMD/Spansion
;;;   works for: SST38VF6401B
;;;
(defmethod lock ((f amd-cmdset-flash) exspec) (declare (ignore exspec)))
(defmethod unlock ((f amd-cmdset-flash) exspec) (declare (ignore exspec)))

(defmethod emit-flash-region-writer ((f amd-cmdset-flash) base &aux (*poison-mips-stack* *poison-flash-writer-stack*))
  (with-bioable-mips-segment ((backend f) base)
    (with-function-calls ()
      (with-mips-gpri (:flash-base :src :dest :ctrpoll :wordctr
                       :addr :value
                       :ret-reg :temp :temp2)
        (emit-nops 2)
        (emitting-function :issue-command () ; arg0: flash-relative command address; arg1: command value
          (emit-flash-write f #x555 #xaa)
          (emit-flash-write f #x2aa #x55)
          (emit-flash-write f :arg0-ret :arg1))
        (emitting-function :issue-command-abs () ; arg0: absolute command address; arg1: command value
          (emit-flash-compose f :arg1)
          (emit-flash-write f #x555 #xaa)
          (emit-flash-write f #x2aa #x55)
          (emit* :sw :arg1 0 :arg0-ret)
          (emit* :nop))
        (emitting-function :poll-s6-toggle-stop () ; arg0: absolute poll address
          (emitting-iteration (#x07FFFFFF :exit :ctrpoll)
            (emit* :lw :temp 0 :arg0-ret)
            (emit* :nop)
            (emit* :lw :temp2 0 :arg0-ret)
            (emit* :nop)
            (emit* :andi :temp :temp #x40)
            (emit* :andi :temp2 :temp2 #x40)
            (emit-jump-if-eq :exit :temp :temp2)
            (emit* :nop)))
        (emitting-function :poll-s7-high () ; arg0: absolute poll address
          (emitting-iteration (#x07FFFFFF :exit :ctrpoll)
            (emit* :lw :temp 0 :flash-base)
            (emit* :nop)
            (emit* :andi :temp :temp #x80)
            (emit-jump-if-ne :exit :temp :zero)))
        (emitting-function :program-word () ; arg0: absolute address; arg1: value
          ;; this should be done automatically by the prologue routine
          (emit-set-gpr :addr :arg0-ret)
          (emit-set-gpr :value :arg1)
          (emit-near-function-call :issue-command #x555 (bits :amd-opcode :word-program))
          (emit* :sw :value 0 :addr)
          (emit-near-function-call :poll-s6-toggle-stop :addr))
        (emitting-function :program-region () ; arg0: dest base; arg1: src base; arg2: word count
          (emit-set-gpr :dest :arg0-ret)
          (emit-set-gpr :src :arg1)
          (emitting-iteration (:arg2 :exit :wordctr)
            (emit* :lw :temp 0 :src)
            (emit* :nop)
            (emit-near-function-call :program-word :dest :temp)
            (emit* :addiu :src :src 4)
            (emit* :addiu :dest :dest 4)))
        (emitting-function :erase-sector ()
          (emit-set-gpr :addr :arg0-ret)
          (emit-near-function-call :issue-command #x555 (bits :amd-opcode :cyc1-erase))
          (emit-near-function-call :issue-command-abs :addr (bits :amd-opcode :cyc2-erase-sector))
          (emit-near-function-call :poll-s6-toggle-stop :addr))
        (emitting-function :erase-block ()
          (emit-set-gpr :addr :arg0-ret)
          (emit-near-function-call :issue-command #x555 (bits :amd-opcode :cyc1-erase))
          (emit-near-function-call :issue-command-abs :addr (bits :amd-opcode :cyc2-erase-block))
          (emit-near-function-call :poll-s6-toggle-stop :addr))
        (emitting-function :erase-chip ()
          (emit-near-function-call :issue-command #x555 (bits :amd-opcode :cyc1-erase))
          (emit-near-function-call :issue-command 0 (bits :amd-opcode :cyc2-erase-chip))
          (emit-near-function-call :poll-s7-high 0))
        (emit-nops #x8)
        (emit-global-tag :entry-point)
        (emit-nops #x20)
        (emit-global-tag :end-of-code)
        (save-compilation-environment (find-environment 'gpr) *tag-domain*)))))

;;;
;;; Implementations: STAA
;;;

;;;
;;; Implementations: Intel
;;;   works for: MT28F640J3
;;;
(defmethod lock ((f intel-cmdset-flash) exspec) (declare (ignore exspec)))
(defmethod unlock ((f intel-cmdset-flash) exspec) (declare (ignore exspec)))

;;; An ugly fixup.
(defmethod write-u8-extents :after ((o intel-cmdset-flash) extents &key preserve-holes before-fn stream)
  (declare (ignore extents preserve-holes before-fn stream))
  (flash-write o 0 :read-array))

(defmethod emit-flash-region-writer ((f intel-cmdset-flash) base &aux (*poison-mips-stack* *poison-flash-writer-stack*))
  (with-bioable-mips-segment ((backend f) base)
    (with-function-calls ()
      (with-mips-gpri (:flash-base :block-addr :burstctr :wordctr
                       :addr :value
                       :src :dest
                       :ret-reg :ctr :ctr2 :temp :temp2)
        (emit-nops 2)
        (emitting-function :issue-command-abs () ; arg0: absolute command address; arg1: command value
          (emit-flash-compose f :arg1)
          (emit* :sw :arg1 0 :arg0-ret)
          (emit* :nop))
        (emitting-function :poll ()     ; arg0: absolute poll address
          (emit-set-gpr :value (bits :intel-opcode :quiescence))
          (emit-flash-compose f :value)
          (emitting-iteration (#x07FFFFFF :exit :ctr)
            (emit* :lw :temp 0 :arg0-ret)
            (emit* :nop)
            (emit* :and :temp :temp :value)
            (emit-jump-if-eq :exit :temp :value)
            (emit* :nop)))
        (emitting-function :poll-issue-write-to-buffer () ; arg0: absolute poll address
          (emit-set-gpr :addr :arg0-ret)
          (emit-set-gpr :value (bits :intel-opcode :quiescence))
          (emit-flash-compose f :value)
          (emitting-iteration (#x07FFFFFF :exit :ctr)
            (emit-near-function-call :issue-command-abs :addr (bits :intel-opcode :write-to-buffer))
            (emit* :lw :temp 0 :addr)
            (emit* :nop)
            (emit* :and :temp :temp :value)
            (emit-jump-if-eq :exit :temp :value)
            (emit* :nop)))
        (emitting-function :program-burst () ; arg0: word count
          (emit-set-gpr :burstctr :arg0-ret)
          (emit-set-gpr :block-addr #xfffc0000)
          (emit* :and :block-addr :block-addr :dest)
          (emit-near-function-call :issue-command-abs :block-addr (bits :intel-opcode :read-status))
          (emit-near-function-call :poll :block-addr)
          (emit-near-function-call :poll-issue-write-to-buffer :block-addr)
          (emit* :addiu :burstctr :burstctr #xffff) ;; burst count sig is (1- word count)
          (emit-near-function-call :issue-command-abs :block-addr :burstctr)
          (emit* :addiu :burstctr :burstctr #x1)
          (emitting-iteration (:burstctr :exit :ctr2)
            (emit* :lw :temp 0 :src)
            (emit* :nop)
            (emit* :sw :temp 0 :dest)
            (emit* :addiu :src :src 4)
            (emit* :addiu :dest :dest 4))
          (emit-near-function-call :issue-command-abs :block-addr (bits :intel-opcode :finalize-block-erase/program-resume/cyc2-clear-block-lock/cyc2-block-erase)))
        (emitting-function :program-region () ; arg0: dest base; arg1: src base; arg2: word count
          (emit-set-gpr :dest :arg0-ret)
          (emit-set-gpr :src :arg1)
          (emit-set-gpr :wordctr :arg2)
          (emit-near-function-call :issue-command-abs :dest (bits :intel-opcode :clear-status))
          (emit-near-function-call :issue-command-abs :addr (bits :cfi-opcode :read-array))
          (emit-tag :region-loop)
          (emit* :slti :temp :wordctr #x10)
          (emit-ref :call-4 (delta) :beq :temp :zero delta)
          (emit* :nop)
          (emit-near-function-call :program-burst :wordctr)
          (emit-ref :region-loop-exit (delta) :beq :zero :zero delta)
          (emit* :nop)
          (emit-tag :call-4)
          (emit-near-function-call :program-burst #x10)
          (emit* :addiu :wordctr :wordctr #xfff0)
          (emit-ref :region-loop (delta) :bne :wordctr :zero delta)
          (emit* :nop)
          (emit-tag :region-loop-exit)
          ;; (emit* :addiu :dest :dest #xfffc)
          ;; (emit-near-function-call :issue-command-abs :dest (bits :intel-opcode :read-status))
          ;; (emit-near-function-call :poll :dest)
          (emit-near-function-call :issue-command-abs 0 (bits :intel-opcode :read-status))
          (emit-near-function-call :issue-command-abs 0 (bits :intel-opcode :clear-status))
          (emit-near-function-call :issue-command-abs 0 (bits :cfi-opcode :read-array)))
        (emitting-function :erase-sector ()
          (emit-set-gpr :addr :arg0-ret)
          (emit-near-function-call :issue-command-abs :addr (bits :intel-opcode :clear-status))
          (emit-near-function-call :issue-command-abs :addr (bits :intel-opcode :begin-change-block-lock))
          (emit-near-function-call :issue-command-abs :addr (bits :intel-opcode :finalize-block-erase/program-resume/cyc2-clear-block-lock/cyc2-block-erase))
          (emit-near-function-call :poll :addr)
          (emit-near-function-call :issue-command-abs :addr (bits :intel-opcode :block-erase-begin))
          (emit-near-function-call :issue-command-abs :addr (bits :intel-opcode :finalize-block-erase/program-resume/cyc2-clear-block-lock/cyc2-block-erase))
          (emit-near-function-call :poll :addr)
          (emit-near-function-call :issue-command-abs :addr (bits :intel-opcode :clear-status))
          (emit-near-function-call :issue-command-abs :addr (bits :cfi-opcode :read-array)))
        (emit-nops #x8)
        (emit-global-tag :entry-point)
        (emit-nops #x20)
        (emit-global-tag :end-of-code)
        (save-compilation-environment (find-environment 'gpr) *tag-domain*)))))
