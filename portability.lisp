;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PORTABILITY; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2009, ГУП НПЦ "Элвис"
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

(defpackage #:portability
  (:use :common-lisp :alexandria :iterate :pergamum)
  (:export
   #:set-and-activate-repl-fun
   #:argv
   #:argv0-executable-name
   #:getenv
   #:function-arglist
   #:quit
   #:with-quit-restart
   #:sigint
   #:handle-sigint
   #:with-sigint-trap
   #:class-finalized-p
   #:finalize-inheritance
   #:class-prototype
   #:class-direct-subclasses
   #:function-lambda-list
   #:with-pinned-objects
   #:without-gcing-and-interrupts
   #:nanosleep
   #:busywait
   #:busywait-interruptible-executing
   #:digest-as-string))

(in-package :portability)

(defun set-and-activate-repl-fun (fn)
  #-(or sbcl ecl clisp) (not-implemented 'set-and-activate-repl-fun)
  #+sbcl (progn
           (setf sb-impl::*repl-fun-generator*
                 (constantly (lambda (x)
                               (declare (ignore x))
                               (funcall fn))))
           (sb-impl::toplevel-repl nil))
  #+(or ecl clisp) (funcall fn))

(defun argv ()
  #-(or sbcl ecl clisp) (not-implemented 'argv)
  #+sbcl sb-ext:*posix-argv*
  #+ecl (si::command-args)
  #+clisp (ext:argv))

(defun argv0-executable-name ()
  "Return the name of the executable."
  (subseq (first (argv)) (let ((slash-posn (position #+win32 #\\ #-win32 #\/ (first (argv)) :from-end t)))
                           (if slash-posn (1+ slash-posn) 0))))

(defun (setf argv) (value)
  #-(or sbcl ecl) (not-implemented '(setf argv))
  (setf
   #+sbcl sb-ext:*posix-argv*
   #+ecl (si::command-args)
   value))

(defun getenv (name)
  #-(or sbcl ecl) (not-implemented 'getenv)
  #+sbcl (sb-posix:getenv name)
  #+ecl (si:getenv name))

(defun function-arglist (function-name)
  #-sbcl nil
  #+sbcl (sb-kernel:%simple-fun-arglist (fdefinition function-name)))

(defun quit (&optional (status 0))
  "Exit."
  #-(or sbcl ecl clisp) (not-implemented 'quit)
  #+sbcl (sb-ext:quit :unix-status status)
  #+clisp (ext:quit status)
  #+ecl (si:quit status))

(defmacro with-quit-restart (&body body)
  "Execute BODY with a QUIT restart, whose activation will quit the Lisp."
  `(restart-case (progn ,@body)
     (quit ()
       :report "Quit Lisp."
       (quit))))

(defmacro handle-sigint (form &body when-sigint-body)
  #+(not (or ecl (and sbcl (not win32)))) ;; No implementation.
  form
  #+ecl
  `(handler-case ,form
     (si::interactive-interrupt ()
       ,@when-sigint-body))
  #+(and sbcl (not win32))
  (let ((old-sigint (gensym)))
    `(let (,old-sigint)
       (unwind-protect
            (progn (setf ,old-sigint (sb-unix::enable-interrupt
                                      sb-unix::sigint
                                      (lambda (a b c)
                                        (declare (ignorable a b c))
                                        ,@when-sigint-body)))
                   ,form)
         (when ,old-sigint
           (sb-unix::enable-interrupt sb-unix::sigint ,old-sigint))))))

(defmacro with-sigint-trap (sigint-val &body body)
  (let ((sigint-block (gensym)))
    `(block ,sigint-block
       (handle-sigint (progn ,@body)
         (return-from ,sigint-block ,sigint-val)))))

(defun class-finalized-p (class)
  #+sbcl (sb-mop:class-finalized-p class)
  #+clisp (clos:class-finalized-p class)
  #+ecl (clos:class-finalized-p class))

(defun finalize-inheritance (class)
  #+sbcl (sb-mop:finalize-inheritance class)
  #+clisp (clos:finalize-inheritance class)
  #+ecl (clos:finalize-inheritance class))

(defun class-prototype (class)
  #+sbcl (sb-mop:class-prototype class)
  #+clisp (clos:class-prototype class)
  #+ecl (clos:class-prototype class))

(defun class-direct-subclasses (class)
  #+sbcl (sb-mop:class-direct-subclasses class)
  #+clisp (clos:class-direct-subclasses class)
  #+ecl (clos:class-direct-subclasses class))

(defun function-lambda-list (function)
  #-sbcl (not-implemented 'function-lambda-list)
  #+sbcl (sb-introspect:function-lambda-list function))

(defmacro with-pinned-objects ((object) &body body)
  #-sbcl
  `(progn ,@body)
  #+sbcl
  `(sb-sys:with-pinned-objects (,object)
     ,@body))

(defmacro without-gcing-and-interrupts (&body body)
  #-sbcl
  `(progn ,@body)
  #+sbcl
  `(sb-sys:without-interrupts
     (sb-sys:without-gcing
       ,@body)))

#+(and ecl mingw32)
(cffi:defcfun ("Sleep" win32-sleep) :void (milliseconds :unsigned-int))

#+(or) (declaim (ftype (function ((unsigned-byte 30)) (values)) nanosleep))
(defun nanosleep (nsecs)
  (declare (optimize (speed 3) (debug 0) (safety 0)) (type (unsigned-byte 30) nsecs))
  #+(not (or (and sbcl (or unix windows))
             (and ecl (or unix mingw32))))
  (not-implemented 'nanosleep)
  ;; #+(and sbcl unix) (nanosleep-fast nsecs)
  #+(and sbcl unix) (sb-unix:nanosleep 0 nsecs)
  #+(and sbcl windows) (sb-win32:millisleep (truncate nsecs 1000000))
  #+(and ecl mingw32) (win32-sleep (truncate nsecs 1000000))
  #+(and ecl unix) (sleep (/ nsecs 1000000000))
  (values))

(defmacro busywait (condition (&body iffail) &key (timeout 1000) (iteration-period 10000))
  (with-gensyms (condval i loop-name)
    (once-only (timeout iteration-period)
      `(iter ,loop-name
             (declare (iterate:declare-variables))
             (for ,i from 0)
             (declare (fixnum ,i))
             (for ,condval = ,condition)
             (until ,condval)
             (nanosleep ,iteration-period)
             (when (>= ,i (the fixnum ,timeout))
               ,@iffail
               (return-from ,loop-name nil))
             (finally (return-from ,loop-name ,condval))))))

(defmacro busywait-interruptible-executing (condition periodform &key (iteration-period 10000000) (watch-period 100) run-time)
  "Poll CONDITION every ITERATION-PERIOD seconds either until satisfaction,
or until user interruption, evaluating PERIODFORM every WATCH-PERIOD iteration.
Optionally RUN-TIME, specifies a soft cap on whole execution, by terminating
execution when at least RUN-TIME nanoseconds have passed. Note that
the accumulated time spent executing CONDITION and PERIODFORM isn't accounted
for.
The return value is T if the CONDITION was satisfied, or NIL in the
case user has intervened, or :TIMEOUT if time ran out, as specified by RUN-TIME."
  (with-gensyms (i iter)
    (once-only (iteration-period watch-period run-time)
      `(with-sigint-trap nil
         (iter (when ,condition
                 (return t))
               (for ,i from 0)
               (for ,iter from 0)
               (when (and ,run-time (> ,iter ,run-time))
                 (return :timeout))
               (when (= ,i ,watch-period)
                 ,periodform
                 (setf ,i 0))
               (nanosleep ,iteration-period))))))

;;;;
;;;; Digestion
;;;;
(defun digest-as-string (digest)
  (with-output-to-string (s)
    (iter (for x in-sequence digest) (format s "~2,'0X" x))))
