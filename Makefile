### Makefile for XCVB ###

## default target
all: xcvb lisp-install


## Ensure presence of configuration file
ifeq ($(wildcard configure.mk),)
  $(warning You need to create the configuration file configure.mk)
  $(warning copy it from doc/configure.mk.example and edit.)
  $(warning cp doc/configure.mk.example configure.mk ; emacs configure.mk)
  $(error Configuration file not found.)
endif

include configure.mk

export INSTALL_LISP

LISP_INSTALL_FILES := driver.lisp asdf-extensions.lisp

## cl-launch mode: standalone executable, script+image or script+fasls?
define CL_LAUNCH_MODE_standalone
	--output ${INSTALL_BIN}/$1 --dump !
endef
define CL_LAUNCH_MODE_image
	--output ${INSTALL_BIN}/$1 --dump ${IMAGE_DIR}/$1.image
endef
define CL_LAUNCH_MODE_fasls
	--output ${INSTALL_BIN}/$1
endef


## Creating executable
xcvb:
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} \
	--system xcvb --restart xcvb::main \
	$(call CL_LAUNCH_MODE_${CL_LAUNCH_MODE},xcvb)
# for debugging, also use --file setup.lisp and have that file do
# (asdf:oos 'asdf:load-op :cl-launch) (setf *compile-verbose* t *load-verbose* t)


## Installing Lisp files needed at runtime
lisp-install:
	rsync -av ${LISP_INSTALL_FILES} ${INSTALL_LISP}/

## Janitoring
clean:
	rm -f *.*fasl *.*fsl *.lib *.fas
	rm -f xcvb doc/*.html

mrproper: clean
	rm -f configure.mk


## For use on common-lisp.net
%.html: %.rest
	rst2html $< $@

doc: $(patsubst %.rest, %.html, $(wildcard doc/*.rest))

online-doc: doc
	rsync -av doc/*.html common-lisp.net:/project/xcvb/public_html/doc/
