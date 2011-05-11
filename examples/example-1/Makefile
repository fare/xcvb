
LISP = sbcl
XCVB_ROOT := $(shell readlink -f ../..)
XCVB_OBJECT_DIRECTORY=`xcvb show object-directory`
XCVB_IMAGE=${XCVB_OBJECT_DIRECTORY}/example-1.image

# Required for xcvb to find the source registry
export CL_SOURCE_REGISTRY := ${XCVB_ROOT}//:${CL_SOURCE_REGISTRY}

# Build by having xcvb run the simple-build steps itself.
build:
	xcvb simple-build \
		--build "example-1" \
		--lisp-implementation ${LISP} \
		--verbosity 10
	cl-launch --lisp ${LISP} \
		--image ${XCVB_IMAGE} \
		--no-include \
		--dump '!' \
		--output example-1 \
		--init '(xcvb-example-1::main)'

# Build by creating the xcvb.mk file and running it. 
all: example-1

example-1: ${XCVB_IMAGE}
	cl-launch --lisp ${LISP} \
		--image ${XCVB_IMAGE} \
		--no-include \
		--output example-1 \
		--dump '!' \
		--init '(xcvb-example-1::main)'

${XCVB_IMAGE}: xcvb.mk
	make -f xcvb.mk 

xcvb.mk: build.xcvb $(wildcard *.lisp)
	xcvb make-makefile \
		--build "example-1" \
		--lisp-implementation ${LISP} \
		--verbosity 10

clean:
	rm -rf xcvb.mk example-1
