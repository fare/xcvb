#+xcvb (module (:depends-on ("utilities")))

(in-package :xcvb)

;; User-visible special variables.

;; I don't know a good way to ensure this bit gets incremented at the right
;; time...  It would be better to have some version number automatically
;; extracted from the date, plus a hash obtained from git, and/or from xcvb's
;; own digesting mechanism.
(defparameter *xcvb-version* "0.309")

(defvar *lisp-implementation-type* nil
  "Type of Lisp implementation for the target system")

(defvar *lisp-executable-pathname* nil
  "Path to the Lisp implementation to use for the target system")

(defvar *lisp-image-pathname* nil
  "What path to a Lisp image do we need invoke the target Lisp with?")

(defvar *lisp-image-name* nil
  "What fullname to a Lisp image do we need invoke the target Lisp with?")

(defvar *lisp-flags* :default
  "What options do we need invoke the target Lisp with?")

(defvar *use-cfasls* (eq *lisp-implementation-type* :sbcl)
  "Should we assume the target Lisp supports CFASL?")

(defparameter +xcvb-setup-dependencies+
  '((:lisp "/xcvb/driver"))
  "Special Lisp dependencies to load into the initial buildee image for XCVB")

(defparameter *lisp-setup-dependencies* +xcvb-setup-dependencies+
  "Special Lisp dependencies to load into the initial buildee image")

(defvar *xcvb-verbosity* 5
  "Level of verbosity of XCVB:
0 - quiet
5 - usual warnings
9 - plenty of debug info")

(defvar *search-path* '()
  "Path to search for XCVB modules")

;;; Note: this needs be setup before you create the binary.
;;; The variable is set in configure.mk and exported by the Makefile.
;;; Ideally, the form would be evaluated when you dump the image,
;;; not when you compile the FASL.
;;; TODO: make that not depend on an environment variable,
;;; and/or make the dependency explicit in a way that XCVB is aware of.
(defvar *xcvb-lisp-directory*
  #p"/usr/share/common-lisp/xcvb/"
  ;; (pathname (strcat (cl-launch:getenv "INSTALL_XCVB") "/"))
  "Directory pathname for the location where XCVB Lisp files are installed")

(defvar *lisp-allow-debugger* nil
  "Should we allow interactive debugging of failed build attempts?")

;; *pathname-grain-cache* is used by code in lisp-grain.lisp and names.lisp.
;; lisp-grain:handle-extension-form :generate inserts lisp grains of
;; generated files into *pathname-grain-cache*.
;; names:probe-file-grain looks up lisp grains in *pathname-grain-cache*.
(defvar *pathname-grain-cache*
  (make-hash-table :test 'equal)
  "Registry of known files, indexed by namestring.
Negatives are stored as NIL. Positives as grains.")
