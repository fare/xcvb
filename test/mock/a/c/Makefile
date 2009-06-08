### Makefile to test XCVB

all: obj/c.image

XCVB_ROOT := $(shell readlink -f ../../../..)

include ${XCVB_ROOT}/configure.mk

export XCVB_PATH := ${XCVB_ROOT}/test/mock/d:${XCVB_ROOT}/test/mock:${XCVB_ROOT}

$(shell ( export XCVB_PATH=${XCVB_PATH} ) >&2)

xcvb.mk: setup.lisp
	xcvb make-makefile --setup /c/setup --build /c

ifeq ($(wildcard xcvb.mk),xcvb.mk)
  include xcvb.mk
endif

setup.lisp:
	( ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -B print_lisp_launcher ; \
	  ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -i "(let ((*package* (find-package :cl-launch))) (format t \"~S~%\" \`(setf asdf:*central-registry*',asdf:*central-registry*)))" \
	) > $@

.PHONY: force all default
