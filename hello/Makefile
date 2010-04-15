### Makefile for an example program using XCVB.

# The ultimate target in this example build: an executable program
# IN YOUR PROJECT,
# your ultimate target will usually likewise be an executable.
all: hello

# This leverages the configuration the user already did for XCVB.
# IN YOUR PROJECT,
# until Lisp paths and features are standardized,
# you may have to instead require a similar configuration of your own.
# Your toplevel project would have a configure.mk just like XCVB,
# and any subproject would somehow locate and reuse that configuration.
# IN THE FUTURE,
# XCVB may instead look for its configuration in a global file such as
# ~/.config/common-lisp/xcvb.ini or /etc/common-lisp/xcvb.ini
# (that you can override on the command-line) that includes all the configuration
# for your user or site.
XCVB_ROOT := $(shell readlink -f ..)
#May also work: XCVB_ROOT := $(shell xcvb ssp | grep '^ (:BUILD "/xcvb") ' | cut -d'"' -f4)
include ${XCVB_ROOT}/configure.mk

# Autodetect the CL_SOURCE_REGISTRY for the example.
# IN YOUR PROJECT,
# this might be set as part of the user configuration,
# or left wholly to the default user and/or system settings.
export CL_SOURCE_REGISTRY := ${XCVB_ROOT}//:${CL_SOURCE_REGISTRY}
$(shell ( echo export CL_SOURCE_REGISTRY=${CL_SOURCE_REGISTRY} ) >&2)

# This is where XCVB is actually invoked.
# IN YOUR PROJECT,
# you should replace the /xcvb/hello with the fullname of your project.
#
# NOTE: if your project has ASDF dependencies not (yet) converted to XCVB,
# you may have to setup ASDF's source-registry. Please use ASDF 2,
# and see its documentation about the source-registry.
#	http://common-lisp.net/project/asdf/
#
xcvb.mk: force
	xcvb make-makefile \
	     --build /xcvb/hello \
	     --lisp-implementation ${LISP} \
	     --object-directory ${XCVB_OBJECT_DIRECTORY}/hello

# The PARALLELIZE trick allows the user to disable or tweak build parallelism with
# make PARALLELIZE= or make PARALLELIZE=-j8 or make PARALLELIZE=-l4
# The XCVB_DEBUGGING trick allows the user to debug the build when and only when it fails.
PARALLELIZE ?= -j
${XCVB_OBJECT_DIRECTORY}/hello/xcvb/hello.image: xcvb.mk
	${MAKE} -f xcvb.mk ${PARALLELIZE} || XCVB_DEBUGGING=t ${MAKE} -f xcvb.mk

# This uses cl-launch to build an executable from the image created by xcvb.
# For portability across Lisp implementation, we use an image on the side.
# With SBCL and CLISP, you could use "--dump !" to create a standalone executable instead
# IN THE FUTURE,
# xcvb may have builtin option to specify such executables.
hello-using-mk: ${XCVB_OBJECT_DIRECTORY}/hello/xcvb/hello.image
	mkdir -p ${INSTALL_BIN}
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} --image $< --no-include \
		--output ${INSTALL_BIN}/hello --dump ${INSTALL_IMAGE}/hello.image \
		--init '(xcvb-hello::main)'

hello: hello-using-mk

# If your project has ASDF dependencies that haven't been converted,
# or if you are building with ASDF, or using our ASDF or POIU backend,
# you will have to have a setup file that ensures ASDF is present and configured.
# The setup.lisp better have at the start a declaration such as
# #+xcvb (module (:build-depends-on ()))
# then somehow load ASDF, configure its *central-registry*,
# and otherwise setup optimization proclamations.
# Note that the configuration of the central registry is redundant with ASDF 2.
#
# If your source registry is properly configured and your implementation comes with ASDF 2,
# all you need is
# --setup /xcvb/require-asdf
#
# If your source registry is properly configured and contains ASDF 2, you may use
# --setup /asdf/asdf
#
# This example shows how you can leverage XCVB-style configuration of cl-launch
# into a setup.lisp that replicates this configuration.
# You can then specify the setup file with
# --setup /xcvb/hello/setup
# (changing the fullname to whatever fits your project)
#
setup.lisp:
	( ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -B print_lisp_launcher ; \
	  ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -i "(let ((*package* (find-package :cl-launch))) (format t \"~S~%\" \`(setf asdf:*central-registry*',asdf:*central-registry*)))" \
	) > $@


# This builds hello directly with ASDF, using cl-launch, using a generated xcvb-hello.asd
hello-using-asdf: xcvb-hello.asd
	mkdir -p ${INSTALL_BIN}
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} --system xcvb-hello \
		--output ${INSTALL_BIN}/hello --dump ${INSTALL_IMAGE}/hello.image \
		--init '(xcvb-hello::main)'

xcvb-hello.asd: force
	xcvb x2a --build /xcvb/hello --build /xcvb/hello/lib --name xcvb-hello

# This builds hello using our non-enforcing Makefile backend
hello-using-nemk:  ${XCVB_OBJECT_DIRECTORY}/hello_ne/xcvb-tmp.image
	mkdir -p ${INSTALL_BIN}
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} --image $< --no-include \
		--output ${INSTALL_BIN}/hello --dump ${INSTALL_IMAGE}/hello.image \
		--init '(xcvb-hello::main)'

${XCVB_OBJECT_DIRECTORY}/hello_ne/xcvb-tmp.image: xcvb-ne.mk
	${MAKE} -f xcvb-ne.mk

xcvb-ne.mk: force setup.lisp
	xcvb non-enforcing-makefile \
	     --setup /xcvb/hello/setup \
	     --build /xcvb/hello \
	     --lisp-implementation ${LISP} \
	     --object-directory ${XCVB_OBJECT_DIRECTORY}/hello_ne


# Declare some targets as phony.
.PHONY: force all default hello hello-using-mk hello-using-nemk hello-using-asdf

# The end.
