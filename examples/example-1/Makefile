LISP ?= sbcl
XCVB_ROOT := $(shell readlink -f ../..)
XCVB_OBJECT_DIRECTORY := $(shell xcvb show --lisp-implementation ${LISP} object-directory)
EXAMPLE_1_EXE=${XCVB_OBJECT_DIRECTORY}/xcvb/example-1/example-1

# Required for xcvb to find the source registry
export CL_SOURCE_REGISTRY := ${XCVB_ROOT}//:${CL_SOURCE_REGISTRY}

# Build by having xcvb run the simple-build steps itself.
build:
	xcvb simple-build \
		--build "/xcvb/example-1/example-1" \
		--lisp-implementation ${LISP} \
		--verbosity 10
	cp ${EXAMPLE_1_EXE} example-1

# Build by creating the xcvb.mk file and running it. 
all: example-1

${EXAMPLE_1_EXE}: xcvb.mk
	make -f xcvb.mk 

xcvb.mk: build.xcvb $(wildcard *.lisp)
	xcvb make-makefile \
		--build "/xcvb/example-1/example-1" \
		--lisp-implementation ${LISP} \
		--object-directory ${XCVB_OBJECT_DIRECTORY} \
		--verbosity 10

clean:
	rm -rf xcvb.mk example-1
