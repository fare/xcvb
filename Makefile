### Makefile for XCVB ###

## default target
all: install

install: xcvb lisp-install

## Ensure presence of configuration file
ifeq ($(wildcard configure.mk),)
  $(warning You need to create the configuration file configure.mk)
  $(warning copy it from doc/configure.mk.example and edit.)
  $(warning cp doc/configure.mk.example configure.mk ; emacs configure.mk)
  $(error Configuration file not found.)
endif

include configure.mk

## Check that the variables used by Makefile are defined in configure.mk.
ifndef INSTALL_IMAGE
  $(error Please define INSTALL_IMAGE in your configure.mk.)
endif
ifndef INSTALL_XCVB
  $(error Please define INSTALL_XCVB in your configure.mk.)
endif
ifndef CL_LAUNCH
  $(error Please define CL_LAUNCH in your configure.mk.)
endif
ifndef CL_LAUNCH_FLAGS
  $(error Please define CL_LAUNCH_FLAGS in your configure.mk.)
endif
ifndef CL_LAUNCH_MODE
  $(error Please define CL_LAUNCH_MODE in your configure.mk.)
endif
ifndef XCVB_CACHE
  $(error Please define XCVB_CACHE in your configure.mk.)
endif

export INSTALL_XCVB
export XCVB_CACHE
export XCVB_OBJECT_CACHE := ${XCVB_WORKSPACE}/obj
export XCVB_WORKSPACE

LISP_SOURCES := $(wildcard *.lisp */*.lisp *.asd */*.asd)
LISP_INSTALL_FILES := build.xcvb *.asd *.lisp

export XCVB_DIR := $(shell pwd)

## cl-launch mode: standalone executable, script+image or script+fasls?
define CL_LAUNCH_MODE_standalone
	--output ${INSTALL_BIN}/$1 --dump !
endef
define CL_LAUNCH_MODE_image
	--output ${INSTALL_BIN}/$1 --dump ${INSTALL_IMAGE}/$1.image
endef
define CL_LAUNCH_MODE_fasls
	--output ${INSTALL_BIN}/$1
endef

LISP_BIN := $(shell ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -B print_lisp_binary_path)
LISP_IMPL := $(shell ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -B print_lisp_implementation)

ifeq (${LISP_IMPL},sbcl)
  export SBCL_HOME:=$(shell ${LISP_BIN} \
    --noinform --no-userinit --no-sysinit --eval '(progn(princ(posix-getenv "SBCL_HOME"))(quit))')
endif

xcvb: xcvb-using-xcvb # If broken, fall back to xcvb-using-asdf

XCVB_MK := ${XCVB_WORKSPACE}/xcvb/xcvb.mk
MK_XCVB := ${MAKE} -C ${XCVB_WORKSPACE} -f ${XCVB_MK}

XCVB_IMPLEMENTATION_OPTIONS := \
	     --lisp-implementation ${LISP_IMPL} \
	     --lisp-binary-path ${LISP_BIN} \
	     ${FEATURE_OPTIONS}

mk: ${XCVB_MK}
${XCVB_MK}: force
	xcvb make-makefile \
	     --build /xcvb/xcvb \
	     --output-path $@ \
	     ${XCVB_IMPLEMENTATION_OPTIONS}

PARALLELIZE ?= -j

#${XCVB_WORKSPACE}/bin/xcvb: ${XCVB_MK}
${XCVB_OBJECT_CACHE}/xcvb/xcvb: ${XCVB_MK}
	${MK_XCVB} ${PARALLELIZE} || XCVB_DEBUGGING=t ${MK_XCVB}

xcvb-using-xcvb: ${INSTALL_BIN}/xcvb

#${INSTALL_BIN}/xcvb: ${XCVB_WORKSPACE}/bin/xcvb
${INSTALL_BIN}/xcvb: ${XCVB_OBJECT_CACHE}/xcvb/xcvb
	mkdir -p ${INSTALL_BIN}
	cp -f $< $@

XCVB_INIT :=	--final "(xcvb::prepare-image \
				:version \#.(xcvbd::get-xcvb-version) \
				:directory \"${INSTALL_XCVB}/\")" \
		--init '(apply (function xcvb::main) cl-launch::*arguments*)'

## If you don't have XCVB, but have a cl-launch with properly ASDF setup in configure.mk,
## then you can bootstrap XCVB with the following target:
xcvb-using-asdf:
	mkdir -p ${INSTALL_BIN} ${INSTALL_IMAGE}
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} \
	--file require-asdf.lisp \
	--system xcvb ${XCVB_INIT} \
	$(call CL_LAUNCH_MODE_${CL_LAUNCH_MODE},xcvb)

## The non-enforcing backend
XCVB_NE_MK := ${XCVB_WORKSPACE}/xcvb/xcvb-ne.mk

${XCVB_NE_MK}: force
	xcvb non-enforcing-makefile \
	     --build /xcvb \
	     --setup /xcvb/setup \
	     --output-path $@ \
	     --object-cache ${XCVB_OBJECT_CACHE}/_ne \
	     ${XCVB_IMPLEMENTATION_OPTIONS}

${XCVB_OBJECT_CACHE}/_ne/xcvb-tmp.image: ${XCVB_NE_MK}
	${MAKE} -f ${XCVB_NE_MK}

xcvb-using-nemk: ${XCVB_OBJECT_CACHE}/_ne/xcvb-tmp.image
	mkdir -p ${INSTALL_BIN} ${INSTALL_IMAGE}
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} \
	--image $< ${XCVB_INIT} --no-include \
	$(call CL_LAUNCH_MODE_${CL_LAUNCH_MODE},xcvb)


## Installing Lisp files needed at runtime
lisp-install:
	mkdir -p ${INSTALL_XCVB}/
	rsync -av ${LISP_INSTALL_FILES} ${INSTALL_XCVB}/

## Janitoring
tidy:
	rm -f *.*fasl *.*fsl *.lib *.fas *.amd64f *.x86f *.o *.a *.xcl *.abcl
	cd doc ; rm -f *.aux *.out *.bbl *.dvi *.log *.blg
	rm -f xcvb.mk xcvb-ne.mk examples/*/xcvb*.mk \
		examples/hello/xcvb-hello.asd examples/hello/version.lisp \
		examples/example-1/example-1 examples/example-2/example-2

clean: tidy
	rm -rf xcvb xcvb-bootstrapped obj tmp cache workspace
	cd doc ; rm -f *.html *.pdf

mrproper: clean
	rm -f configure.mk

wc:
	wc *.lisp | sort -nr | less

hello:
	${MAKE} -C examples/hello

## For use on common-lisp.net
%.html: %.rest
	case "$<" in (*slides*) rst2s5 $< $@ ;; (*) rst2html $< $@ ;; esac

ILCPAPER=ilc09-xcvb-paper

doc/$(ILCPAPER).pdf: doc/$(ILCPAPER).tex doc/xcvb.bib
	cd doc && pdflatex $(ILCPAPER) && \
	bibtex $(ILCPAPER) && pdflatex $(ILCPAPER) && \
	bibtex $(ILCPAPER) && pdflatex $(ILCPAPER)

xpdf: doc/$(ILCPAPER).pdf
	xpdf $<

doc: $(patsubst %.rest, %.html, $(wildcard doc/*.rest)) doc/$(ILCPAPER).pdf

online-doc: doc
	rsync -av doc/*.html doc/*.pdf doc/ui common-lisp.net:/project/xcvb/public_html/doc/

pull:
	git pull common-lisp.net:/project/xcvb/public_html/git/xcvb.git master:master
	git pull git@github.com:fare/xcvb.git master:master

push:
	git push --tags common-lisp.net:/project/xcvb/public_html/git/xcvb.git master:master
	git push --tags git@github.com:fare/xcvb.git master:master
	git pull

show-current-revision:
	git show --pretty=oneline HEAD | head -1 | cut -d' ' -f1

TMP ?= /tmp

export RELEASE_EXCLUDE := \
	--exclude build --exclude obj --exclude obj-ne \
	--exclude "*~" --exclude ".\#*" --exclude "xcvb*.mk" \
# To exclude revision information: --exclude .git --exclude _darcs

XCVB_TEST := ${INSTALL_BIN}/xcvb-test
xcvb-test: ${XCVB_TEST}

${XCVB_TEST}: ${INSTALL_BIN}/xcvb $(wildcard t/*.lisp t/build.xcvb)
	xcvb make-build --build /xcvb/t/xcvb-test \
		${XCVB_IMPLEMENTATION_OPTIONS}
	cp -f ${XCVB_OBJECT_CACHE}/xcvb/t/xcvb-test $@

unit-tests: ${XCVB_TEST}
	${XCVB_TEST} unit-tests

test:	${XCVB_TEST}
	${XCVB_TEST} validate-xcvb-dir --xcvb-dir ${XCVB_DIR} --verbosity 99

fulltest:	${XCVB_TEST}
	${XCVB_TEST} validate-xcvb-dir-all-lisps --xcvb-dir ${XCVB_DIR} --verbosity 99

export RELEASE_DIR := ${TMP}/xcvb-release

test-driver: ${XCVB_TEST}
	xcvb-test driver-tests

release: release-directory release-tarballs test-and-release-tarballs

release-directory:
	mkdir -p ${RELEASE_DIR} && \
	${MAKE} -C ${RELEASE_DIR} -f ${XCVB_DIR}/doc/Makefile.release \
		checkout reset update gc prepare-release && \
	{ rm -rf "${RELEASE_DIR}/build/" ; \: ;}

release-tarballs: xcvb-test
	xcvb-test eval '(xcvb-test::make-release-tarballs :release-dir "${RELEASE_DIR}/")'

test-release-directory: ${XCVB_TEST}
	${XCVB_TEST} validate-release-dir-all-lisps --release-dir ${RELEASE_DIR} --verbosity 99

test-and-release-tarballs: release-tarballs test-release-directory
	cd ${RELEASE_DIR}/xcvb && \
	VERSION=$$(git describe --tags) && \
	cd ${TMP} && \
	rsync -av xcvb-$$VERSION.tar.bz2 xcvb.tar.bz2 \
		xcvb-$$VERSION-and-dependencies.tar.bz2 xcvb-and-dependencies.tar.bz2 \
		common-lisp.net:/project/xcvb/public_html/releases/

fake-release-directory: xcvb-test
	xcvb-test eval '(xcvb-test::make-fake-release-directory :release-dir "${RELEASE_DIR}/")'

pre-release-test: fake-release-directory test-release-directory
	${MAKE} -C ${RELEASE_DIR} reset

show-config:
	@echo "LISP=${LISP}" ; \
	echo "CL_LAUNCH=${CL_LAUNCH}" ; \
	echo "CL_LAUNCH_FLAGS=${CL_LAUNCH_FLAGS}" ; \
	echo "CL_LAUNCH_MODE=${CL_LAUNCH_MODE}" ; \
	echo "INSTALL_BIN=${INSTALL_BIN}" ; \
	echo "INSTALL_LISP=${INSTALL_LISP}" ; \
	echo "INSTALL_SOURCE=${INSTALL_SOURCE}" ; \
	echo "INSTALL_SYSTEM=${INSTALL_SYSTEM}" ; \
	echo "INSTALL_IMAGE=${INSTALL_IMAGE}" ; \
	echo "INSTALL_XCVB=${INSTALL_XCVB}" ; \
	echo "XCVB_WORKSPACE=${XCVB_WORKSPACE}" ; \
	echo "XCVB_CACHE=${XCVB_CACHE}" ; \
	echo "XCVB_OBJECT_CACHE=${XCVB_OBJECT_CACHE}" ; \
	:

WRONGFUL_TAGS := xcvb_0.1 xcvb_0.11 xcvb_0.300 xcvb_0.539
# Delete wrongful tags from local repository
fix-local-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git tag -d $$i ; done

# Delete wrongful tags from remote repository
fix-remote-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git push $${REMOTE:-origin} :refs/tags/$$i ; done

.PHONY: all install lisp-install tidy clean mrproper \
	xpdf doc online-doc pull push show-current-revision force \
	release release-directory release-tarball test-and-release-tarball \
	xcvb-asdf-install \
	test fulltest show-config mk \
	fix-local-git-tags fix-remote-git-tags

# To check out a particular revision: git fetch; git merge $commit
