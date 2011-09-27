TARGET=example-2
XCVB_ROOT := $(shell cd ../.. ; /bin/pwd)
XCVB_OBJECT_DIRECTORY=$(XCVB_ROOT)/examples/${TARGET}/obj
XCVB_IMAGE=${XCVB_OBJECT_DIRECTORY}/${TARGET}.image

# Required for xcvb to find the source registry
export CL_SOURCE_REGISTRY := ${XCVB_ROOT}//:${CL_SOURCE_REGISTRY}

all: ${TARGET}

example-2: ${XCVB_IMAGE}
	cl-launch --lisp 'sbcl sbcl clisp ccl' \
		--image ${XCVB_IMAGE} \
		--no-include \
		--output ${TARGET} \
		--dump '!' \
		--init '(xcvb-example-2::main)'

${XCVB_IMAGE}: xcvb.mk
	make -f xcvb.mk 

xcvb.mk: build.xcvb $(wildcard *.lisp) $(wildcard lib-a/*.lisp) $(wildcard lib-b/*.lisp)
	mkdir -p ${XCVB_OBJECT_DIRECTORY}
	xcvb make-makefile \
		--build "${TARGET}" \
		--lisp-implementation sbcl \
		--object-directory ${XCVB_OBJECT_DIRECTORY}

clean:
	rm -rf ${XCVB_OBJECT_DIRECTORY} xcvb.mk ${TARGET}
