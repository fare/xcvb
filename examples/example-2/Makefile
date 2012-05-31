XCVB_ROOT := $(shell cd ../.. ; /bin/pwd)
include ${XCVB_ROOT}/configure.mk
EXE=${XCVB_WORKSPACE}/bin/example-2
IMAGE=${XCVB_WORKSPACE}/lib/images/example-2.image
XCVB_MK := ${XCVB_WORKSPACE}/example/example-2/xcvb.mk

export XCVB_WORKSPACE
export XCVB_CACHE

# Required for xcvb to find the source registry
export CL_SOURCE_REGISTRY := ${XCVB_ROOT}//:${CL_SOURCE_REGISTRY}

all: build-with-xcvb

build-with-asdf:
	mkdir -p $$(dirname ${EXE}) $$(dirname ${IMAGE})
	cl-launch --lisp 'sbcl sbcl clisp ccl' \
		--image ${IMAGE} \
		--no-include \
		--output ${EXE} \
		--dump '!' \
		--init '(xcvb-example-2::main)'

build-with-xcvb:
	xcvb make-build \
		--build "/xcvb/example-2" \
		--lisp-implementation ${LISP}

clean:
	rm -rf ${XCVB_WORKSPACE}
