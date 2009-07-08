### Makefile to test XCVB

# The ultimate target in this example build: a Lisp image
# IN YOUR PROJECT, your ultimate target will usually be something else
# that is based on the image but requires additional processing and installation.
all: obj/c.image
# XCVB TODO: add some feature to XCVB and/or cl-launch
# for building executables from images.

# This leverages the configuration the user already did for XCVB.
# IN YOUR PROJECT, until Lisp paths and features are standardized,
# you may have to instead require a similar configuration of your own,
# though at some point you might be able to autodetect it.
XCVB_ROOT := $(shell readlink -f ../../../..)
include ${XCVB_ROOT}/configure.mk

# Autodetect the XCVB_PATH for the example.
# IN YOUR PROJECT, this might be set as part of the user configuration,
# or left wholly to the default user and/or system settings.
export XCVB_PATH := ${XCVB_ROOT}/test/mock/d:${XCVB_ROOT}/test/mock:${XCVB_ROOT}
$(shell ( export XCVB_PATH=${XCVB_PATH} ) >&2)

# This is where XCVB is actually invoked.
# /c is the fullname of the current BUILD.
# Note that xcvb depends on a module named setup in the current BUILD.
# Said module contains initializations to make sure ASDF is loaded and configured,
# and otherwise setup optimization proclamations.
# IN YOUR PROJECT, you may have to let the user specify his setup,
# or to somehow create the setup from the previous configuration.
# This example shows how you can leverage the above configuration of cl-launch
# into a setup.lisp that replicates this configuration.

xcvb.mk: setup.lisp force
	xcvb make-makefile --setup /c/setup --build /c

# You need to "make xcvb.mk" before you can actually "make" the ultimate target.
obj/c.image: xcvb.mk
	${MAKE} -f xcvb.mk -j $@

setup.lisp:
	( ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -B print_lisp_launcher ; \
	  ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -i "(let ((*package* (find-package :cl-launch))) (format t \"~S~%\" \`(setf asdf:*central-registry*',asdf:*central-registry*)))" \
	) > $@


# Declare some targets as phony.
.PHONY: force all default

# The end.
