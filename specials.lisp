(in-package :xcvb)

(defvar *lisp-implementation-type* :sbcl
  "Type of Lisp implementation for the target system")

(defvar *lisp-executable-pathname* nil
  "Path to the Lisp implementation to use for the target system")

(defvar *lisp-image-pathname* nil
  "What path to a Lisp image do we need invoke the target Lisp with?")

(defvar *lisp-flags* :default
  "What options do we need invoke the target Lisp with?")

(defvar *lisp-setup-dependencies* nil
  "What dependencies should we compile into our stage1 lisp image ?")

(defvar *use-cfasls* (eq *lisp-implementation-type* :sbcl)
  "Should we assume the target Lisp supports CFASL?")

(defvar *xcvb-setup-dependencies*
  '("driver.lisp" "asdf-extensions.lisp")
  "Dependencies in the buildee image, as required for XCVB to work properly.")

(defvar *search-path* '()
  "Path to search for XCVB modules")

;;; Note: this needs be setup before you create the binary.
;;; The variable is set in configure.mk and exported by the Makefile.
;;; Ideally, the form would be evaluated when you dump the image,
;;; not when you compile the FASL.
(defvar *xcvb-lisp-directory*
  (pathname (concatenate 'string (cl-launch:getenv "INSTALL_LISP") "/"))
  "Directory pathname for the location where XCVB Lisp files are installed")

(defvar *lisp-allow-debugger* nil
  "Should we allow interactive debugging of failed build attempts?")
