(in-package :xcvb)

;; I don't know a good way to ensure this bit gets incremented at the right time...
;; It would be better to have some version number automatically extracted from
;; the date, plus a hash obtained from git, and/or from xcvb's own digesting mechanism.
(defparameter *xcvb-version* "0.206")

(defvar *lisp-implementation-type* :sbcl
  "Type of Lisp implementation for the target system")

(defvar *lisp-executable-pathname* nil
  "Path to the Lisp implementation to use for the target system")

(defvar *lisp-image-pathname* nil
  "What path to a Lisp image do we need invoke the target Lisp with?")

(defvar *lisp-image-name* nil
  "What fullname to a Lisp image do we need invoke the target Lisp with?")

(defvar *lisp-flags* :default
  "What options do we need invoke the target Lisp with?")

(defvar *lisp-setup-dependencies* nil
  "What dependencies should we compile into our stage1 lisp image ?")

(defvar *use-cfasls* (eq *lisp-implementation-type* :sbcl)
  "Should we assume the target Lisp supports CFASL?")

(defvar *xcvb-setup-dependencies*
  '((:lisp "/xcvb/driver") (:lisp "/xcvb/asdf-extensions"))
  "Dependencies in the buildee image, as required for XCVB to work properly.")

(defvar *search-path* '()
  "Path to search for XCVB modules")

;;; Note: this needs be setup before you create the binary.
;;; The variable is set in configure.mk and exported by the Makefile.
;;; Ideally, the form would be evaluated when you dump the image,
;;; not when you compile the FASL.
;;; TODO: make that not depend on an environment variable,
;;; and/or make the dependency explicit in a way that XCVB is aware of.
(defvar *xcvb-lisp-directory*
  (pathname (strcat (cl-launch:getenv "INSTALL_LISP") "/"))
  "Directory pathname for the location where XCVB Lisp files are installed")

(defvar *lisp-allow-debugger* nil
  "Should we allow interactive debugging of failed build attempts?")
