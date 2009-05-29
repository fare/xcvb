### Makefile to test XCVB

XCVB_ROOT := $(shell readlink -f ../../../..)

include ${XCVB_ROOT}/configure.mk

export XCVB_PATH := ${XCVB_ROOT}/test/mock/d:${XCVB_ROOT}/test/mock:${XCVB_ROOT}

$(shell ( export XCVB_PATH=${XCVB_PATH} ) >&2)

ifeq ($(wildcard xcvb.mk),xcvb.mk)
  include xcvb.mk
else
default: xcvb.mk
endif

all: obj/c.image

setup.lisp:
	( ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -B print_lisp_launcher ; \
	  ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -iw "\`(setf asdf:*central-registry*',asdf:*central-registry*)" \
	) > $@

xcvb.mk: setup.lisp
	xcvb make-makefile --setup /c/setup --build /c

.PHONY: force all default
