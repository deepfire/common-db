;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-DB; Base: 10 -*-
;;;
;;;  (c) copyright 2009 by
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


(in-package :common-db)


(defvar *flasher-help-en*
  "  Flasher options:
    --raw-file <raw-filename>   Upload raw file at specified base address.
                                Must provide --raw-base.
    --raw-base <raw-filename-base>  Specify the base address for raw file
                                    upload.
    --elf-file <ELF-filename>   Upload an ELF file.
    --preserve-holes            Preserve data neighbouring by the erase blocks
                                  with that which is written.
    --flash-base                Specify the flash base address.
                                  Defaults to #xBFC00000.
    --dump-base <address>       Print memory contents.
    --dump-size <bytes>         Specify the amount of memory to print. 
                                  Defaults to #x100.
    --no-check                  Don't check integrity of uploaded data.
    --dry-run                   Don't do anything, besides printing.")

(defvar *flash*)

(defun flasher ()
  (setf common-db::*additional-help-en* *flasher-help-en*
        common-db::*additional-help-ru* *flasher-help-ru*)
  (comdb::comdb-toplevel-wrapper #'flasher-toplevel
                                 '((:raw-file :string) :raw-base (:elf-file :string) :flash-base :dump-base :dump-size
                                   (:fcode :binary))
                                 '(:erase-chip :preserve-holes :no-check :dry-run)
                                 :disable-debugger t
                                 :no-memory-detection t
                                 :help-needed-discriminator
                                 (lambda (args)
                                   (null (intersection args '(:run-tests :raw-file :elf-file :dump-base))))))

(defun flasher-toplevel (&key verbose
                         (flash-base #xbfc00000) (fcode kuseg)
                         raw-file raw-base
                         elf-file
                         dump-base (dump-size #x100)
                         erase-chip preserve-holes no-check dry-run)
  (let (loadables)
    (when erase-chip
      (error "~@<Whole-chip erasing not implemented.~:@>~%"))
    (unless (or raw-file elf-file dump-base) ; we were just running tests
      (quit))
    (when raw-file
      (unless raw-base
        (error "~@<Raw file specified, but no base provided.~:@>"))
      (push (loadable:extract-loadable :raw raw-file :base raw-base :raw-section-name (intern (string-upcase raw-file))) loadables))
    (when elf-file
      (push (loadable:extract-loadable :elf elf-file) loadables))
    (when loadables
      (dolist (l loadables)
        (dolist (section (loadable:loadable-sections l))
          (when (< (base section) flash-base)
            (error "~@<Loadable section ~S below the flash base.~:@>" section))
          (decf (base section) flash-base)))
      (let ((f (make-instance 'standard-flash :backend comdb:*target*
                              :base flash-base :bank-width 4))
            (*log-loadable-processing* verbose))
        (setf *flash* f
              (state *core*) :debug)
        (probe-flash f)
        (when (eq (type-of f) 'standard-flash) 
          (error "~@<Unable to find a flash at #x~8,'0X~:@>" flash-base))
        (setf (flash-code-seg f) fcode)
        (if verbose
            (format t "~@<Found flash: ~S~:@>~%" f)
            (format t "~@<Flash base: ~8,'0X~:@>~%" (flash-base f)))
        (dolist (l loadables)
          (when verbose (format t "~@<Uploading loadable ~S.~:@>~%" l))
          (unless dry-run
            (loadable:upload-loadable f l :section-before-fn #'loadable:report-section :check (not no-check)
                                      :preserve-holes preserve-holes)))
        (format t "~@<All done.~:@>~%")))
    (when dump-base
      (dump dump-base dump-size))))
