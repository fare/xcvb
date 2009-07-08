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
LISP_INSTALL_FILES := driver.lisp build.xcvb

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

setup.lisp:
	( ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -B print_lisp_launcher ; \
	  ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} -i "(let ((*package* (find-package :cl-launch))) (format t \"~S~%\" \`(setf asdf:*central-registry*',asdf:*central-registry*)))" \
	) > $@

xcvb-bootstrapped: obj/xcvb.image
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} --image $$PWD/obj/xcvb.image --output $@ --init '(xcvb::main)'

define use_xcvb_mk
	${MAKE} -f xcvb.mk -j $1
endef

xcvb.mk: force
	xcvb make-makefile \
	     --build /xcvb \
	     --setup /xcvb/setup \
	     --lisp-implementation ${LISP_IMPL} \
	     --lisp-binary-path ${LISP_BIN}

obj/xcvb.image: xcvb.mk
	$(call use_xcvb_mk,$@)

## Creating executable
xcvb: ${INSTALL_BIN}/xcvb

${INSTALL_BIN}/xcvb: configure.mk ${LISP_SOURCES}
	mkdir -p ${INSTALL_BIN} ${INSTALL_IMAGE}
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} \
	--system xcvb --restart xcvb::main \
	$(call CL_LAUNCH_MODE_${CL_LAUNCH_MODE},xcvb)
# for debugging, also use --file setup.lisp and have that file do
# (asdf:oos 'asdf:load-op :cl-launch) (setf *compile-verbose* t *load-verbose* t)


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

test:
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} --system xcvb-test --restart xcvb::quit

pull:
	git pull git@github.com:fare/xcvb.git master:master
	git pull common-lisp.net:/project/xcvb/public_html/git/xcvb.git master:master

push:
	git push --tags git@github.com:fare/xcvb.git master:master
	git push --tags common-lisp.net:/project/xcvb/public_html/git/xcvb.git master:master

show-current-revision:
	git show --pretty=oneline HEAD | head -1 | cut -d' ' -f1

TMP ?= /tmp

release-tarball:
	VERSION=$$(cat version.lisp | grep version | cut -d\" -f2) ; \
	mkdir -p ${TMP}/xcvb-$$VERSION && cd ${TMP}/xcvb-$$VERSION && \
	( git clone http://common-lisp.net/project/xcvb/git/xcvb.git || \
	  echo "Already got xcvb.git" ) && \
	mkdir -p dependencies && cd dependencies && \
	( git clone http://common-lisp.net/project/asdf/asdf.git || \
	  echo "Already got asdf.git" ) && \
        echo '#+xcvb (module (:fullname "asdf" :depends-on ("asdf")))' > asdf/build.xcvb && \
	( if [ ! -f asdf/asdf.lisp.orig ] ; then mv asdf/asdf.lisp asdf/asdf.lisp.orig ; fi ) && \
        ( echo '#+xcvb (module ())' ; cat asdf/asdf.lisp.orig ) > asdf/asdf.lisp && \
	( git clone http://common-lisp.net/project/xcvb/git/asdf-dependency-grovel.git || \
	  echo "Already got asdf-dependency-grovel.git" ) && \
	( git clone http://common-lisp.net/project/qitab/git/command-line-arguments.git || \
	  echo "Already got command-line-arguments.git" ) && \
	( git clone http://common-lisp.net/project/xcvb/git/cl-launch.git || \
	  echo "Already got cl-launch.git" ) && \
	( cd cl-launch && ./cl-launch.sh -I $$PWD -B install_path ) && \
	( if [ -d closer-mop ] ; then echo "Already got closer-mop from darcs" ; else \
	  darcs get http://www.common-lisp.net/project/xcvb/darcs/closer-mop ; fi ) && \
	cd .. && \
	cp xcvb/doc/Makefile.release Makefile && \
	(read ; read ; cat ) < xcvb/doc/INSTALL.release > INSTALL && \
	cp xcvb/doc/configure.mk.example xcvb/configure.mk && \
	pwd && export XCVB_PATH=$$PWD && \
	xcvb make-makefile --xcvb-path=$$PWD --build /xcvb --lisp-implementation sbcl && \
	rm -f obj/target-properties.lisp-expr && rmdir obj && \
	cd .. && tar jcf xcvb-$$VERSION.tar.bz2 xcvb-$$VERSION/ && \
	ln -sf xcvb-$$VERSION.tar.bz2 xcvb.tar.bz2 && \
	rsync -av xcvb-$$VERSION.tar.bz2 xcvb.tar.bz2 \
		common-lisp.net:/project/xcvb/public_html/releases/

.PHONY: all install lisp-install test tidy clean mrproper \
	xpdf doc online-doc pull push show-current-revision force \
	release-tarball

# To check out a particular revision: git fetch; git merge $commit
