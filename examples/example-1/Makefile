XCVB_ROOT := $(shell cd ../.. ; /bin/pwd)
include ${XCVB_ROOT}/configure.mk
EXE=${XCVB_WORKSPACE}/bin/example-1
XCVB_MK := ${XCVB_WORKSPACE}/example/example-1/xcvb.mk

# Required for xcvb to find the source registry
export CL_SOURCE_REGISTRY := ${XCVB_ROOT}//:${CL_SOURCE_REGISTRY}


all: build-with-parallel-make

# Build by having xcvb run the simple-build steps itself.
build-with-simple-build:
	xcvb simple-build \
		--build "/xcvb/example-1/example-1" \
		--lisp-implementation ${LISP} \
		--verbosity 10

# Build by creating the xcvb.mk file and running it.
build-with-parallel-make: ${EXE}
${EXE}: ${XCVB_MK}
	make -f ${XCVB_MK}

${XCVB_MK}: build.xcvb $(wildcard *.lisp)
	xcvb make-makefile \
		--build "/xcvb/example-1/example-1" \
		--lisp-implementation ${LISP} \
		--verbosity 10

clean:
	rm -rf ${XCVB_WORKSPACE}
