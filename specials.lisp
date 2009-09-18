#+xcvb (module (:depends-on ("utilities")))

(in-package :xcvb)

;; User-visible special variables.

;; Note: the statement below only declares the variable.
;; The actual version number is defined in file version.lisp.
;; TODO: find a good way to ensure this bit gets incremented at the right time.
;; e.g. have some version number automatically extracted from the date, plus
;; maybe a hash obtained from git, and/or from xcvb's own digesting mechanism.
;; MAYBE what we want is just a git hook that will ensure that a given version
;; file is modified at every commit, or else either auto-increment the number
;; in that file or plainly error out.
(defvar *xcvb-version*)

(defvar *lisp-implementation-type*
  (or #+sbcl :sbcl #+clisp :clisp #+ccl :ccl #+cmu :cmucl)
  "Type of Lisp implementation for the target system.
Default: same as XCVB itself.")

(defvar *lisp-executable-pathname* nil
  "Path to the Lisp implementation to use for the target system.
Default: what's in your PATH.")

(defvar *target-system-features* nil
  "value of *features* in the target system
Autodetected from the target Lisp system.")

(defvar *lisp-image-pathname* nil
  "What path to a Lisp image do we need invoke the target Lisp with?
Default: whatever's the default for your implementation.")

(defvar *lisp-implementation-directory*
  (or #+sbcl (namestring (sb-int:sbcl-homedir-pathname)))
  "Where is the home directory for the Lisp implementation,
in case we need it to (require ...) special features?
Default: whatever's the default for your implementation.")

(defvar *lisp-flags* :default
  "What options do we need invoke the target Lisp with?")

;; *use-cfasls* is set by main.lisp after *lisp-implementation-type* is set.
(defvar *use-cfasls* nil
  "Should we assume the target Lisp supports CFASL?
Autodetected from the target Lisp system.")

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
  #p"/usr/share/common-lisp/source/xcvb/"
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

(defvar *object-directory* "obj"
  "where to store object files")

(defvar *tmp-directory* #p"/tmp/")

(defvar *use-base-image* t
  "Should we be using a base image for all builds?")
