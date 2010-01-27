;;; -*- Mode: Common-Lisp; indent-tabs-mode: nil -*-

;;;    Use tooltips with CLIM presentations
;;;    Copyright (C) 2007 SRI International.
;;;
;;;    This library is free software; you can redistribute it and/or
;;;    modify it under the terms of the GNU Library General Public
;;;    License as published by the Free Software Foundation; either
;;;    version 2 of the License, or (at your option) any later version.

;;;    This library is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;    Library General Public License for more details.

;;;    You should have received a copy of the GNU Library General Public
;;;    License along with this library; if not, write to the
;;;    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;    Boston, MA  02111-1307  USA.

(in-package :common-db-gui)

;;============================= CLIM Tooltips ===============================
;; Written by Suzanne Paley.
;; To use tooltips w/ clim presentations, define after methods for the clim
;; presentation-method clim:highlight-presentation.  When the state argument
;; is :highlight, the method should call draw-tooltip.  When the state
;; argument is :unhighlight, the method should call erase-tooltip.  Here's
;; a sample call (which assumes the fn get-tooltip-text returns a string 
;; or NIL):
;;
;;;(clim:define-presentation-method clim:highlight-presentation :after
;;;  ((type t) record stream (state (eql :highlight)))
;;;  (draw-tooltip stream (get-tooltip-text record)
;;;                :region (orec-relative->absolute-region record stream)))
;;;
;;;(clim:define-presentation-method clim:highlight-presentation :after
;;;  ((type t) record stream (state (eql :unhighlight)))
;;;  (declare (ignore record))
;;;  (erase-tooltip stream)
;;;  )
;;
;; At some point, we may want to extend the tooltip functionality to allow
;; for arbitrary output w/in a tooltip (i.e. not just a string, maybe
;; graphics too).

(defvar *tooltip-textstyle* 
    (clim:make-text-style :sans-serif :roman :very-small))

;; Once an application frame has been initialized, an application will 
;; probably want to change this color to something more appealing!
(defvar *tooltip-color* clim:+yellow+)

(defvar *tooltip-orec* nil)


;; ============================================================= draw-tooltip 
;; [API]
;; Description : Draw a box containing text, to be used as a tooltip.  
;;  Either a region or x- and y-coordinates should be supplied.  
;;  If coordinates are supplied, the box will have its upper left
;;  corner at that position.  If  no coordinates are supplied but a clim
;;  region is, the box will be positioned close to that region, but not
;;  overlapping it.  Currently it is displayed immediately below the region,
;;  but this should be changed to ensure that the entire box is positioned
;;  within the viewport.  If no position and no region are supplied, the box
;;  is drawn at (0,0).  We actually output the text twice, once
;;  to compute its size and the second time to actually render it -- we
;;  may be able to make this more efficient.
;; 
;;      Arguments : stream: the clim stream to output to
;;                  text: a string or NIL
;;                  region: a clim region (optional) -- the tooltip should be
;;                   positioned near this region.
;;                  x,y: integers (optional) -- if supplied, the tooltip will
;;                   have its upper left corner at this position.
;;                  text-render-fn: A function, defaulting to clim:draw-text*,
;;                   used to render the text.  Must the same arguments 
;;                   as clim:draw-text*.
;;        Returns : nothing
;;   Side Effects : sets *tooltip-orec*
;; Update History : gilham:Jul-10-2007 More intelligent positioning of tooltip.

(defun draw-tooltip (stream text
                     &key region x y
                          (text-render-fn #'clim:draw-text*))
  (erase-tooltip stream) ;; clear previous tooltip, if there is one
  (when (and text (not (equal text "")))
    (clim:with-drawing-options (stream :text-style *tooltip-textstyle*
                                       :ink clim:+black+)
      (let ((margin 2))
        (multiple-value-bind (wd ht)
            (clim:bounding-rectangle-size 
             (clim:with-output-to-output-record (stream)
               (funcall text-render-fn stream text 0 0)))
          (setf *tooltip-orec*
            (clim:with-output-recording-options (stream :draw nil :record t)
              (clim:with-new-output-record (stream)
                (clim:draw-rectangle* stream (- margin) (- margin)
                                      (+ wd margin) (+ ht margin)
                                      :ink *tooltip-color*)
                (clim:draw-rectangle* stream (- margin) (- margin)
                                      (+ wd margin) (+ ht margin)
                                      :filled nil)
                (funcall text-render-fn stream text 0 0
                                      :align-x :left :align-y :top)
                )))
          (when (and region (not (and x y)))
            ;; We may want to get smarter about positioning of the tooltip
            (multiple-value-setq (x y) (clim:stream-pointer-position stream))
            (if (and x y)
                (let* ((viewport-br (clim:window-viewport stream))
                       (viewport-max-x (clim:bounding-rectangle-max-x viewport-br))
                       (viewport-min-x (clim:bounding-rectangle-min-x viewport-br))
                       (viewport-max-y (clim:bounding-rectangle-max-y viewport-br))
                       (viewport-min-y (clim:bounding-rectangle-min-y viewport-br))
                       (tooltip-max-x (clim:bounding-rectangle-max-x *tooltip-orec*))
                       (tooltip-max-y (clim:bounding-rectangle-max-y *tooltip-orec*))
                       )
                  ;; Get the tool tip clear of the pointer.
                  (setf x (+ x 10)
                        y (- y 10 tooltip-max-y))
                  ;; Try to keep the tool tip in the viewport.
                  (when (> (+ x tooltip-max-x) viewport-max-x)
                    (decf x (+ (- (+ x tooltip-max-x) viewport-max-x) margin))
                    (when (< x viewport-min-x) (setf x viewport-min-x)))
                  (when (< y viewport-min-y)
                    (incf y 40)
                    (when (> y viewport-max-y) (setf y (- viewport-max-y tooltip-max-y)))
                  ))
              (setq x (or x 0)
                    y (or y 0))))
          ))
      (setf (clim:output-record-position *tooltip-orec*) (values x y))
      (clim:tree-recompute-extent *tooltip-orec*)
      (clim:replay *tooltip-orec* stream)
      )))

;; Erase a tooltip drawn by draw-tooltip
;;   Side Effects : sets *tooltip-orec* to nil
(defun erase-tooltip (stream)
  (when *tooltip-orec*
    (clim:erase-output-record *tooltip-orec* stream nil)
    (setf *tooltip-orec* nil)
    ))


;; ============================================ orec-relative->absolute-region
;; [API]
;;    Description : Given an output record, return a clim
;;                   region that reflects its actual position in the window.
;;      Arguments : orec: an output-record
;;          stream: the stream on which orec was displayed
;;        Returns : a clim region
;;   Side Effects : none
;; Update History :

(defun orec-relative->absolute-region (orec stream)
  (multiple-value-bind (xoff yoff)
      #+mcclim
      (climi::convert-from-relative-to-absolute-coordinates stream orec)
      #-mcclim
      (clim:convert-from-relative-to-absolute-coordinates stream orec)
    (clim:transform-region (clim:make-translation-transformation xoff yoff)
                           orec)))


;;;
;;; Test code
;;;
#||
(clim:define-presentation-method clim:highlight-presentation :after
  ((type t) record stream (state (eql :highlight)))
  (unless (eq type 'clim:blank-area)
    (draw-tooltip stream (get-tooltip-text record)
                  :region (orec-relative->absolute-region record stream))))

(clim:define-presentation-method clim:highlight-presentation :after 
  ((type t) record stream (state (eql :unhighlight)))
  (declare (ignore record))
  (unless (eq type 'clim:blank-area)
    (erase-tooltip stream)
  ))

(defun get-tooltip-text (record)
;;  (declare (ignore record))
;;  "This is a tooltip!"
  (format nil "~A" record)
)
||#
