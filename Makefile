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

export INSTALL_XCVB

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

### Not needed by XCVB anymore since it's fully bootstrapped, but that's how
### you may automatically dump a setup.lisp that has the same ASDF configuration
### as cl-launch invoked with the same options (requires cl-launch 2.21 or later).
setup.lisp:
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -B print_lisp_setup > $@

xcvb: xcvb-using-xcvb

xcvb.mk: force
	xcvb make-makefile \
	     --build /xcvb \
	     --setup /xcvb/no-asdf \
	     --lisp-implementation ${LISP_IMPL} \
	     --lisp-binary-path ${LISP_BIN}

PARALLELIZE := -j

obj/xcvb.image: xcvb.mk
	${MAKE} -f xcvb.mk ${PARALLELIZE} $@ || XCVB_DEBUGGING=t ${MAKE} -f xcvb.mk $@

xcvb-using-xcvb: obj/xcvb.image
	${MAKE} xcvb-bootstrapped-install

XCVB_INIT :=	--init "(setf xcvb::*xcvb-lisp-directory* (pathname \"${INSTALL_XCVB}/\"))" \
		--init '(xcvb::main)'

xcvb-bootstrapped-install:
	mkdir -p ${INSTALL_BIN}
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} --image ${XCVB_DIR}/obj/xcvb.image --no-include \
		$(call CL_LAUNCH_MODE_${CL_LAUNCH_MODE},xcvb) \
		${XCVB_INIT}

## If you don't have XCVB, but have a cl-launch with properly ASDF setup in configure.mk,
## then you can bootstrap XCVB with the following target:
xcvb-using-asdf:
	mkdir -p ${INSTALL_BIN} ${INSTALL_IMAGE}
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} \
	--system xcvb ${XCVB_INIT} \
	$(call CL_LAUNCH_MODE_${CL_LAUNCH_MODE},xcvb)

## The non-enforcing backend
xcvb-ne.mk: force
	xcvb non-enforcing-makefile \
	     --build /xcvb \
	     --setup /xcvb/setup \
	     --lisp-implementation ${LISP_IMPL} \
	     --lisp-binary-path ${LISP_BIN}

obj-ne/xcvb-tmp.image: xcvb-ne.mk
	${MAKE} -f xcvb-ne.mk $@

xcvb-using-nemk: obj-ne/xcvb-tmp.image
	mkdir -p ${INSTALL_BIN} ${INSTALL_IMAGE}
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} \
	--image ${XCVB_DIR}/obj-ne/xcvb-tmp.image ${XCVB_INIT} --no-include \
	$(call CL_LAUNCH_MODE_${CL_LAUNCH_MODE},xcvb)


## Installing Lisp files needed at runtime
lisp-install:
	rsync -av ${LISP_INSTALL_FILES} ${INSTALL_XCVB}/

## Janitoring
tidy:
	rm -f *.*fasl *.*fsl *.lib *.fas
	cd doc ; rm -f *.aux *.out *.bbl *.dvi *.log *.blg

clean: tidy
	rm -rf xcvb xcvb-bootstrapped obj tmp
	cd doc ; rm -f *.html *.pdf

mrproper: clean
	rm -f configure.mk

wc:
	wc *.lisp | sort -n | less

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
	git pull git@github.com:fare/xcvb.git master:master
	git pull common-lisp.net:/project/xcvb/public_html/git/xcvb.git master:master

push:
	git push --tags git@github.com:fare/xcvb.git master:master
	git push --tags common-lisp.net:/project/xcvb/public_html/git/xcvb.git master:master
	git pull

show-current-revision:
	git show --pretty=oneline HEAD | head -1 | cut -d' ' -f1

TMP ?= /tmp

# EXCLUDE_REVISION_INFO := --exclude .git --exclude _darcs
RELEASE_EXCLUDE := --exclude build --exclude obj --exclude obj-ne

test:
	./test/runme.zsh XCVB_DIR=${XCVB_DIR} validate_xcvb_dir

fulltest:
	./test/runme.zsh XCVB_DIR=${XCVB_DIR} validate_xcvb_dir_all_lisps

export RELEASE_DIR := ${TMP}/xcvb-release

release: release-directory release-tarball test-and-release-tarball

release-directory:
	mkdir -p ${RELEASE_DIR} && \
	cp doc/Makefile.release ${RELEASE_DIR}/Makefile && \
	cd ${RELEASE_DIR} && \
	make checkout update gc prepare-release

release-tarball:
	cd ${RELEASE_DIR} && \
	VERSION=$$(cat xcvb/version.lisp | grep '^ *".*")' | cut -d\" -f2) && \
	cd .. && rm -f xcvb-$$VERSION && ln -sf xcvb-release xcvb-$$VERSION && \
	tar ${RELEASE_EXCLUDE} -hjcf xcvb-$$VERSION.tar.bz2 xcvb-$$VERSION/ && \
	ln -sf xcvb-$$VERSION.tar.bz2 xcvb.tar.bz2

test-release-tarball:
	cd ${RELEASE_DIR}/xcvb && \
	./test/runme.zsh validate_release_dir

test-and-release-tarball: test-release-tarball
	cd ${RELEASE_DIR} && \
	VERSION=$$(cat xcvb/version.lisp | grep '^ *".*")' | cut -d\" -f2) && \
	cd ${TMP} && \
	rsync -av xcvb-$$VERSION.tar.bz2 xcvb.tar.bz2 \
		common-lisp.net:/project/xcvb/public_html/releases/

.PHONY: all install lisp-install tidy clean mrproper \
	xpdf doc online-doc pull push show-current-revision force \
	release release-directory release-tarball test-and-release-tarball \
	xcvb-bootstrapped-install xcvb-asdf-install \
	test fulltest

# To check out a particular revision: git fetch; git merge $commit
