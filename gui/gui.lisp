;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB-GUI; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2007-2008, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2007-2008 by
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

(in-package :common-db-gui)

(defun later-p (a b)
  (if (> (size a) 1)
      (>= (base a) (end b))
      (>= (base a) (1- (end b)))))

(defmethod pprint-bb ((o bb) s &aux (*print-level* nil) (*print-length* nil))
  (loop :for (nil nil mnemo . params) :across (extent-data o)
        :for i :from 0 :do
     (pprint-logical-block (s nil)
       (format s "~8,'0X " (* 4 (+ (base o) i)) )
       (write mnemo :stream s :circle nil) (write #\Space :stream s :escape nil) 
       (dolist (p params)
         (write #\Space :stream s :escape nil) (write p :stream s :circle nil))
       (pprint-newline :mandatory s))))

(defclass victim-bb (linked-bb) ())
(defclass aggressor-bb (linked-bb) ())
(defclass damage (flagged-mixin)
  ((bons :accessor damage-bons :initarg :bons)
   (aggr-addr :accessor damage-aggr-addr :initarg :aggr-addr)
   (vict-addr :accessor damage-vict-addr :initarg :vict-addr)
   (reg :accessor damage-reg :initarg :reg)))

(defmethod print-object ((o damage) s &aux (*print-level* nil) (*print-length* nil))
  (format s "~A, ~X -> ~X" (damage-reg o) (* 4 (damage-aggr-addr o)) (* 4 (damage-vict-addr o))))

(defmethod print-object ((o victim-bb) s &aux (*print-level* nil) (*print-length* nil))
  (print-unreadable-object (o s)
    (format s "VICTIM ~X, insn-addr: ~X, reg: ~S, aggressor-base: ~X"
            (* 4 (base o)) (* 4 (linked-addr o)) (linked-reg o) (* 4 (base (linked-to o))))))

(defmethod print-object ((o aggressor-bb) s &aux (*print-level* nil) (*print-length* nil))
  (print-unreadable-object (o s)
    (format s "AGGR ~X, insn-addr: ~X, reg: ~S, victim-base: ~X"
            (* 4 (base o)) (* 4 (linked-addr o)) (linked-reg o) (* 4 (base (linked-to o))))))

(defmethod print-object ((o linked-bb) s &aux (*print-level* nil) (*print-length* nil))
  (print-unreadable-object (o s)
    (format s "PATH insn-addr: ~X, reg: ~S, to: ~X"
            (* 4 (linked-addr o)) (linked-reg o) (* 4 (base (linked-to o))))))

(define-presentation-method present (o (type bb) s view &key)
  (surrounding-output-with-border (s :shape :drop-shadow)
    (pprint-bb o s)))

(define-presentation-method present (o (type linked-bb) s view &key)
  (surrounding-output-with-border (s :shape :drop-shadow :background +peru+)
    (pprint-bb o s)))

(define-presentation-method present (o (type victim-bb) s view &key)
  (surrounding-output-with-border (s :shape :drop-shadow :background +green4+)
    (pprint-bb o s)))

(define-presentation-method present (o (type aggressor-bb) s view &key)
  (surrounding-output-with-border (s :shape :drop-shadow :background +red3+)
    (pprint-bb o s)))

(define-presentation-method highlight-presentation :after ((type linked-bb) record stream (state (eql :highlight)))
  (let ((bb (slot-value record 'climi::object)))                            
   (draw-tooltip stream
                 (format nil "insn: ~8,'0X, reg ~S, to: ~S"
                         (* 4 (linked-addr bb)) (linked-reg bb) (linked-to bb))
                 :region (orec-relative->absolute-region record stream))))

(define-presentation-method highlight-presentation :after ((type linked-bb) record stream (state (eql :unhighlight)))
  (declare (ignore record))
  (erase-tooltip stream))

(defclass bb-view (view)
  ((filename :reader filename :initarg :filename)
   (ehdr :accessor ehdr :initarg :ehdr)
   (sectionname :reader sectionname :initarg :sectionname)
   (bbnet :accessor bbnet :initarg :bbnet)
   (tree :accessor tree :initarg :tree)
   (selection :accessor selection :initarg :selection)
   (focus :accessor focus :initarg :focus))
  (:default-initargs :filename nil :ehdr nil :sectionname nil :selection nil :focus nil))

(defclass victim-bb-view (bb-view)
  ((danger-window :accessor danger-window :initarg :danger-window)
   (damage-focus :reader damage-focus :initform nil :initarg :damage-focus)
   (victims :accessor victims :initarg :victims))
  (:default-initargs :danger-window 10 :victims nil :damage-focus nil))

(defvar *bb-view*)

(define-application-frame common-db ()
  ()
  (:command-table (common-db-command-table :inherit-from (main-command-table elf-command-table bb-command-table)))
  (:menu-bar main-command-table)
  (:pointer-documentation t)
  (:panes
   (file-btn
    :push-button :name 'file-button :label "ELF file" :activate-callback
    (lambda (btn)
      (declare (ignore btn))
      (execute-frame-command *application-frame* `(com-choose-elf))
      (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'choice-pane) :force-p t)
      (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'main-pane) :force-p t)))
   (section-btn
    :push-button :name 'section-button :label "section"  :activate-callback
    (lambda (btn)
      (declare (ignore btn))
      (execute-frame-command *application-frame* `(com-choose-section))
      (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'choice-pane) :force-p t)
      (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'main-pane) :force-p t)))
   (filename :text-editor :name 'filename :nlines 1 :ncolumns 52 :max-width +fill+
             :value (write-to-string (filename *bb-view*)))
   (sectionname :text-editor :name 'sectionname :nlines 1 :ncolumns 52 :max-width +fill+
                :value (write-to-string (sectionname *bb-view*)))
   (choice-pane :application :name 'choice-pane :height 150 :display-function 'display-choice-pane :default-view *bb-view*)
   (main-pane :application :name 'main-pane :min-height 200 :min-width 320 :width 800 :height 800 :display-function 'display-main-pane :default-view *bb-view*)
;;    (interactor :interactor :height 128)
   )
  (:layouts
   (default (vertically ()
              (horizontally () file-btn filename +fill+)
              (horizontally () section-btn sectionname +fill+)
              choice-pane
              main-pane
;;               interactor
              ))))

(defun present-print (o s)
  (present o (presentation-type-of o) :stream s))

(defun present-bb-list (list s)
  (iter (for (bb . rest) on list)
        (present-print bb s)
        (terpri)))

(defun display-bb-view-to-stream (stream view)
  (with-slots ((bb focus) tree) view
    (when (and bb tree)
      (multiple-value-bind (ifree iall) (unzip (curry #'later-p bb) (bb-ins bb))
        (multiple-value-bind (ofree oall) (unzip (curry #'later-p bb) (bb-outs bb))
          (formatting-table (stream :equalize-column-widths t)
            (formatting-column (stream)
              (formatting-cell (stream :align-x :center :min-width 300 :min-height 200)
                (format stream "in~%") (present-bb-list ifree stream))
              (formatting-cell (stream))
              (formatting-cell (stream :align-x :center)
                (format stream "out~%") (present-bb-list oall stream)))
            (formatting-column (stream)
              (formatting-cell (stream :align-y :bottom)
                (when-let (prev (intree:resolve (1- (base bb)) tree))
                  (present-bb-list (list prev) stream)))
              (formatting-cell (stream :align-y :baseline)
                (surrounding-output-with-border (stream)
                  (present-bb-list (list bb) stream)))
              (formatting-cell (stream :align-y :top)
                (when-let (next (intree:resolve (end bb) tree))
                  (present-bb-list (list next) stream))))
            (formatting-column (stream)
              (formatting-cell (stream :align-x :center)
                (format stream "out~%") (present-bb-list ofree stream))
              (formatting-cell (stream))
              (formatting-cell (stream :align-x :center)
                (format stream "in~%") (present-bb-list iall stream))))))
      (change-space-requirements stream
       :width (bounding-rectangle-width (stream-output-history stream))
       :height (bounding-rectangle-height (stream-output-history stream))))))

(defgeneric (setf sectionname) (sectionname view))
(defmethod (setf sectionname) (sectionname (o bb-view))
  (when-let (section (first (elf:ehdr-sections (ehdr o) (lambda (shdr)
                                                          (and (elf:shdr-executable-p shdr)
                                                               (string= (symbol-name (elf:shdr-name shdr)) sectionname))))))
    (setf (values (bbnet o) (tree o)) (unturing:insn-vector-to-basic-blocks *mips-isa* section)
          (gadget-value (find-pane-named *application-frame* 'sectionname)) (symbol-name (elf:section-name section))
          (slot-value o 'sectionname) sectionname)))

(defun make-damage (bons aggr-addr vict-addr reg)
  (make-instance 'damage :bons bons :aggr-addr aggr-addr :vict-addr vict-addr :reg reg))

(defun recalculate-victims (victim-bb-view)
  (with-accessors ((bbnet bbnet) (danger-window danger-window) (victims victims) (selection selection)
                   (damage-focus damage-focus) (focus focus)) victim-bb-view
    (setf victims (mapcar (curry #'apply #'make-damage) (isa-mips::find-mc24rt2-victims bbnet danger-window))
          selection (mapcar (compose #'bar #'damage-bons) victims)
          damage-focus (first victims))))

(defmethod (setf sectionname) :after (sectionname (o victim-bb-view))
  (declare (ignore sectionname))
  (recalculate-victims o))

(defgeneric (setf filename) (filename view))
(defmethod (setf filename) (filename (o bb-view) &aux exec-section)
  (if (and filename (probe-file filename)
           (setf (ehdr o) (bintype:parse 'elf:ehdr (file-as-vector filename) :error-p nil))
           (setf exec-section (first (elf:ehdr-sections (ehdr o) #'elf:shdr-executable-p))))
      (setf (sectionname o) (elf:section-name exec-section))
      (setf (slot-value o 'sectionname) nil (tree o) nil (bbnet o) nil (selection o) nil (focus o) nil))
  (setf (gadget-value (find-pane-named *application-frame* 'filename)) (write-to-string filename)
        (slot-value o 'filename) filename))

(defun unmark-damage (damage)
  (mapc (rcurry #'change-class 'bb) (shortest-bons-path (damage-bons damage))))

(defun mark-damage (damage)
  (declare (type damage damage))
  (with-slots (bons aggr-addr vict-addr reg) damage
    (change-class (bar bons) 'aggressor-bb :to (bdr bons) :addr aggr-addr :reg reg)
    (change-class (bdr bons) 'victim-bb :to (bar bons) :addr vict-addr :reg reg)
    (do-path-internal-nodes (bb (shortest-bons-path bons))
      (change-class bb 'linked-bb :to (bdr bons) :addr vict-addr :reg reg))))

(defmethod (setf damage-focus) (new (o victim-bb-view) &aux (old (damage-focus o)))
  (when old (unmark-damage old))
  (when new
    (mark-damage new)
    (setf (focus o) (bdr (damage-bons new))))
  (setf (slot-value o 'damage-focus) new))

(define-command-table elf-command-table
 :menu (("Set ELF file" :command com-choose-elf)
        ("Inspect current ELF file" :command com-inspect-elf)
        ("Save selected paths" :command com-save-selected-paths)
        ("Set ELF section" :command com-choose-section)))

(define-command (com-choose-elf :name t :command-table elf-command-table) ()
  (setf (focus *bb-view*) nil ;; clear the main pane...
        (values) (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'main-pane))
        (filename *bb-view*)
        (file-selector:select-file :stream (find-pane-named *application-frame* 'main-pane))))

(define-command (com-inspect-elf :name t :command-table elf-command-table) ()
  (clouseau:inspector (ehdr *bb-view*)))

(define-command (com-choose-section :name t :command-table elf-command-table) ()
  (setf (sectionname *bb-view*)
        (menu-choose (mapcar (compose #'symbol-name #'elf:section-name)
                             (elf:ehdr-sections (ehdr *bb-view*) #'elf:shdr-executable-p)))))

(define-command (com-save-selected-paths :name t :command-table elf-command-table) ()
  (when-let ((filename (file-selector:select-file :own-window t)))
    (with-output-to-file (s filename)
      (format s "reg, addr-from -> addr-to, basic-block path~%")
      (iter (for damage in (victims *bb-view*))
            (when (flag damage)
              (with-slots (bons aggr-addr vict-addr reg) damage
                (format s "~3@A, ~8,'0X -> ~8,'0X, ~{ ~8,'0X~}~%" reg
                        (* 4 aggr-addr) (* 4 vict-addr)
                        (mapcar (compose (curry #'* 4) #'base) (shortest-bons-path bons)))))))))

(define-command-table bb-command-table
 :menu (("Set BB by address" :command com-set-bb-by-addr)
        ("Go linked BB" :command com-go-linked-bb)))

(define-flag-toggle-and-translator bb-command-table)

(define-command (com-set-bb-by-addr :name t :command-table bb-command-table) ()
  (let (bbaddr)
    (accepting-values (*query-io* :own-window t)
      (setf bbaddr (ash (accept 'integer :prompt "BB Address" :stream *query-io*
                                :default (base (focus *bb-view*)) :insert-default t)
                        -2)))
    (when-let (bb (find bbaddr (bbnet *bb-view*) :key #'base))
      (setf (focus *bb-view*) bb))))

(define-command (com-go-linked-bb :name t :command-table bb-command-table) ((bb 'linked-bb :gesture :menu))
  (setf (focus *bb-view*) (linked-to bb)))

(define-command-table main-command-table
    :menu (("ELF" :menu elf-command-table)
           ("BB" :menu bb-command-table)
           ("Quit" :command com-quit)))

(define-command (com-set-damage :name t :command-table main-command-table) ((damage 'damage))
  (setf (damage-focus *bb-view*) damage))

(define-command (com-set-bb :name t :command-table main-command-table) ((bb 'bb))
  (setf (focus *bb-view*) bb))

(define-common-db-command (com-previous :name t :command-table main-command-table :keystroke :up) ()
  (let* ((view *bb-view*)
         (bb (focus view)))
    (when-let (prev (intree:resolve (1- (base bb)) (tree view)))
      (setf (focus view) prev))))

(define-common-db-command (com-next :name t :command-table main-command-table :keystroke :down) ()
  (let* ((view *bb-view*)
         (bb (focus view)))
    (when-let (next (intree:resolve (end bb) (tree view)))
      (setf (focus view) next))))

(define-presentation-to-command-translator set-damage
    (damage com-set-damage main-command-table :documentation "Set damage" :pointer-documentation "Set damage")
    (object)
  (list object))

(define-presentation-to-command-translator set-bb
    (bb com-set-bb main-command-table :documentation "Set BB" :pointer-documentation "Set BB")
    (object)
  (list object))

(define-command (com-quit :name t :command-table main-command-table) ()
  (frame-exit *application-frame*))

(defun display-choice-to-stream (pane view)
  (dolist (bb (selection view))
    (with-output-as-presentation (pane bb (presentation-type-of bb))
      (format pane "~S~%" bb))))

(defun display-damage-choice-to-stream (pane view)
  (dolist (damage (victims view))
    (present (make-instance 'flag :who damage) 'flag :stream pane)
    (with-output-as-presentation (pane damage 'damage)
      (format pane "~S~%" damage))))

(defgeneric display-main-pane-with-view (frame pane view)
  (:method (frame pane (view bb-view))
    (display-bb-view-to-stream pane view)))

(defgeneric display-choice-pane-with-view (frame pane view)
  (:method (frame pane (view bb-view))
    (display-choice-to-stream pane view))
  (:method (frame pane (view victim-bb-view))
    (display-damage-choice-to-stream pane view)))

(defun display-main-pane (frame pane)
  (display-main-pane-with-view frame pane (stream-default-view pane)))

(defun display-choice-pane (frame pane)
  (display-choice-pane-with-view frame pane (stream-default-view pane)))

(defun bb-graph-explorer (all select &aux (select (ensure-list select)))
  (setf *bb-view* (make-instance 'bb-view :bbnet all :selection select :focus (first select) :tree (bbnet-tree all)))
  (let ((*print-base* #x10))
    (run-frame-top-level (make-application-frame 'common-db))))

(defun victim-explorer ()
  (setf *bb-view* (make-instance 'victim-bb-view))
  (let ((*print-base* #x10))
    (handler-bind ((simple-error (lambda (c)
                                   (invoke-restart (find-restart 'abort c)))))
      (run-frame-top-level (make-application-frame 'common-db)))))

(define-application-frame fs ()
  ((own-window-p :accessor own-window-p :initarg :own-window-p))
  (:pointer-documentation t)
  (:panes
   (own-btn :push-button :name 'open-button :label "Own window"
             :activate-callback (lambda (btn)
                                  (declare (ignore btn))
                                  (format *query-io* "~S" (file-selector:select-file :own-window t))))
   (not-own-btn :push-button :name 'open-button :label "Not own window"
                :activate-callback (lambda (btn)
                                     (declare (ignore btn))
                                     (format *query-io* "~S" (file-selector:select-file :own-window nil))))
   (interactor :interactor :height 128))
  (:layouts
   (default (vertically ()
              (horizontally () own-btn not-own-btn +fill+)
              interactor))))

(defun fs (&optional own-window-p)
  (run-frame-top-level (make-application-frame 'fs :own-window-p own-window-p)))

;;;
;;; The use case.
;;;
;;; (cdb-gui:gui bb (isa-mips::analyse-for-mc24rt-bug bb))