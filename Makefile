all: comdb

version := $(shell date +%y.%02W)$(if $(patchlevel),.$(patchlevel))
date := $(shell date +%c)
commit-id := $(shell git log | head -n1 | cut -d\  -f2- | head -c 8)
ostype := $(if $(findstring GNU/Linux,$(shell uname -o)),linux,win32)
platform := $(shell uname -m)
id = $(1)-$(version)-$(ostype)-$(platform)

COMMON_SBCL_INIT = (setf *compile-print* nil *compile-verbose* nil *load-verbose* nil *features* (list* $(FEATURES) *features*) $(SETTINGS)) \
                   (declaim (optimize $(OPTIMIZATIONS)) $(DECLARATIONS)) \
                   (require :asdf) \
                   (funcall (find-symbol "OOS" :asdf) (find-symbol "LOAD-OP" :asdf) $(SYSTEM_NAME) :verbose nil) \
                   (defvar *output-filename* \
                           (concatenate (find-symbol "STRING" :cl) \
                                        (string-downcase (string (car (or (quote ($(RELECUTABLE_NAME))) \
                                                                          (quote ($(EXECUTABLE_NAME))))))) \
                                        \#+win32 ".exe"))
SBCL_DUMP_FORM = (sb-ext:save-lisp-and-die *output-filename* :executable t $(ADDITIONAL_SAVE_LISP_AND_DIE_ARGS) \
                                           :toplevel (function $(FUNCTION)))

sources: $(wildcard apps/*.lisp) $(wildcard commands/*.lisp) $(wildcard tests/*.lisp) $(wildcard *.lisp) $(wildcard *.asd) apps/help-cp1251.lisp

apps/help-cp1251.lisp: apps/help-unicode.lisp
	@iconv --from utf-8 --to cp1251 $^ > $@

comdb comdb-ru comdb-debug:        EXECUTABLE_NAME := comdb
flasher:                           EXECUTABLE_NAME := flasher
helpfn:                            EXECUTABLE_NAME := helpfn
comdb comdb-ru comdb-debug:        SYSTEM_NAME := :common-db-opfr-toplevel
flasher:                           SYSTEM_NAME := :common-db-flasher
comdb comdb-ru comdb-debug:        FEATURES := :opfr-toplevel
comdb comdb-ru comdb-debug:        FUNCTION := comdb::opfr-toplevel
comdb comdb-ru comdb-debug flasher helpfn: ADDITIONAL_SAVE_LISP_AND_DIE_ARGS := :save-runtime-options t
comdb-ru:                          FEATURES += :help-ru
comdb-debug:                       OPTIMIZATIONS := debug safety
flasher:                           FEATURES := :flasher
flasher:                           FUNCTION := comdb::flasher

comdb-release flasher-release: export RELEASE-P := t
comdb-release flasher-release: export VERSION := $(version)

core: export DATE := $(date)
core: export COMMIT-ID := $(commit-id)
core: sources
	@sbcl --noinform --eval '(progn $(COMMON_SBCL_INIT))' --eval '$(SBCL_DUMP_FORM)'

comdb comdb-ru comdb-debug flasher: core

check-tree-clean:
	@git diff --exit-code

comdb-release:   RELECUTABLE_NAME := $(call id,comdb)
flasher-release: RELECUTABLE_NAME := $(call id,flasher)
comdb-release:   check-tree-clean comdb
flasher-release: check-tree-clean flasher

helpfn:      SETTINGS := *compile-verbose* nil *compile-print* nil
helpfn:      FEATURES := :help-ru
helpfn:      FUNCTION := comdb::print-api-documentation
helpfn: sources
	@sbcl --noinform --eval '(progn $(COMMON_SBCL_INIT))' --eval "$(SBCL_DUMP_FORM)"

load-comdb: sources
	@sbcl --eval '(progn #+win32 (load "d:/usr/chain/asdf-op.lisp")   #+(or)\
	                     (push :opfr-toplevel *features*)             #+(or)\
	                     (require :common-db-opfr-toplevel))' \
	      --eval '(comdb::opfr-toplevel)'

comdb-ecl:
	ecl.exe -load $(USR)/chain/asdf-op.lisp -eval '(push :opfr-toplevel *features*)' -eval '(require :common-db-opfr-toplevel)' -eval '(setf c::*delete-files* nil)' -eval '(asdf:make-build :common-db :type :program :epilogue-code (list (find-symbol "FUNCALL" :CL) (list (find-symbol "FIND-SYMBOL" :CL) "OPFR-TOPLEVEL" :comdb)))' -eval '(format t "~&Done.~%")' -eval '(si:quit)'

comdb-ecl-msvc:
	ecl2.exe -load $(USR)/chain/asdf-op.lisp -eval '(push :opfr-toplevel *features*)' -eval '(require :common-db-opfr-toplevel)' -eval '(setf c::*delete-files* nil)' -eval '(asdf:make-build :common-db :type :program :epilogue-code (list (find-symbol "FUNCALL" :CL) (list (find-symbol "FIND-SYMBOL" :CL) "OPFR-TOPLEVEL" :comdb)))' -eval '(format t "~&Done.~%")' -eval '(si:quit)'

clean:
	rm -f comdb comdb-ru flasher
	find . -name '*.fasl' -or -name '*.fas' -or -name '*.o' -or -name '*.a' -or -name '*-mono' -or -name '*.exe' | xargs rm -f
