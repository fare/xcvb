#+xcvb (module (:depends-on ("pkgdcl")))

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

;;; We share a few variables from xcvb-master, that we inherit from its package:
#|
 *lisp-implementation-type*
 *lisp-executable-pathname*
 *lisp-image-pathname*
 *lisp-implementation-directory*
 *lisp-flags*
 *xcvb-verbosity*
 *lisp-allow-debugger*
 *object-directory*
 *tmp-directory*
 *use-base-image*
|#

(defvar *target-system-features* nil
  "value of *features* in the target system
Autodetected from the target Lisp system.")
(defvar *target-added-features* nil
  "extra user-provided *features* to push in the target system")
(defvar *target-suppressed-features* nil
  "extra user-provided *features* to remove from the target system")

;; *use-cfasls* is set by main.lisp after *lisp-implementation-type* is set.
(defvar *use-cfasls* nil
  "Should we assume the target Lisp supports CFASL?
Autodetected from the target Lisp system.")

(defparameter +fast-xcvb-setup-dependencies+
  '((:fasl "/xcvb/driver"))
  "Special Lisp dependencies to load into the initial buildee image for XCVB, fast version")

(defparameter +xcvb-setup-dependencies+
  '((:lisp "/xcvb/driver"))
  "Special Lisp dependencies to load into the initial buildee image for XCVB, slow version")

(defparameter *lisp-setup-dependencies* +fast-xcvb-setup-dependencies+
  "Special Lisp dependencies to load into the initial buildee image")

;;; Note: this needs be setup before you create the binary.
;;; The variable is set in configure.mk and exported by the Makefile.
;;; Ideally, the form would be evaluated when you dump the image,
;;; not when you compile the FASL.
;;; TODO: make that not depend on an environment variable,
;;; and/or make the dependency explicit in a way that XCVB is aware of.
(defvar *xcvb-lisp-directory*
  #p"/usr/share/common-lisp/source/xcvb/"
  ;; (pathname (strcat (asdf-utilities:getenv "INSTALL_XCVB") "/"))
  "Directory pathname for the location where XCVB Lisp files are installed")

;; *pathname-grain-cache* is used by code in names.lisp.
;; build-module extension-form :generate inserts lisp grains of
;; generated files into *pathname-grain-cache*.
;; probe-file-grain looks up lisp grains in *pathname-grain-cache*.
(defvar *pathname-grain-cache*
  (make-hash-table :test 'equal)
  "Registry of known files, indexed by namestring.
Negatives are stored as NIL. Positives as grains.")

(defvar *print-concisely* '(build-module-grain lisp-module-grain fasl-grain cfasl-grain
                            image-grain world-grain active-world lisp-file-grain lisp-generator)
  "For debugging purpose, controls the verbosity of print-object on grains")

(defvar *use-master* t
  "Should we use the XCVB master?")

(defparameter *grains*
  (make-hash-table :test 'equal)
  "A registry of known grains in the traversed build DAG,
indexed by normalized name, either fullname of a module,
nickname, or SEXP representing a computed entity.")

(defparameter *superseded-asdf*
  (make-hash-table :test 'equalp)
  "ASDF systems that have been superseded")

(defvar *computations* ()
  "A list of all the computations created")

(defvar *target-lisp-executable-pathname* nil
  "Path of the target executable")
(defvar *target-lisp-image-pathname* nil
  "Path of the target image")

(defvar *worlds* (make-hash-table :test 'equal)
  ;; TODO: either make active use of it (if *grains* is not enough), or get rid of it
  "Worlds for the standalone backend")

(defvar *arguments* nil
  "Arguments passed to the main function")

(defvar *object-directory-pathname* nil
  "Path to the object directory")

(defparameter *module-classes*
  '((module . lisp-file-grain)))
