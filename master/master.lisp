;;; XCVB master to call XCVB from a Lisp image and load the results.

#|
The current plan for in-image XCVB use (beside having users use the ASDF backend) is that
xcvb-master will run-program an XCVB process that would do all the compilation out-of-image
then tell you what to load, and how to update your configuration.
XCVB will do all the tth checksumming on its side, so you don't have to have ironclad
in your image -- or anything beside xcvb-master,
which ought to be a small standalone Lisp file, smaller than ASDF.

So xcvb-master would::
     xcvb eval '(xcvb:slave-build ...)'
where the form would including a specification of modules already loaded
(i.e. fullname and hashvalue of each included component),
and the subprocess would return a specification of modules to load:
fullname, hashvalue and current pathname.
After loading, a further call to xcvb would allow it to cleanup any
theretofore unneeded temporary file.
|#

#+xcvb
(module
 (:author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT" ;; MIT-style license. See LICENSE
  :description "XCVB Master"

(in-package :cl)

(defpackage :xcvb-master
  :nicknames (:xcvbm)
  :use (:cl)
  :export (#:build-and-load))

(in-package :master)

(defvar *lisp-implementation-type*
  (or #+sbcl :sbcl #+clisp :clisp #+ccl :ccl #+cmu :cmucl)
  "Type of Lisp implementation for the target system.
Default: same as XCVB itself.")

(defvar *lisp-executable-pathname* nil
  "Path to the Lisp implementation to use for the target system.
Default: what's in your PATH.")

(defvar *lisp-image-pathname* nil
  "What path to a Lisp image do we need invoke the target Lisp with?
Default: whatever's the default for your implementation.")

(defvar *lisp-implementation-directory*
  (or #+sbcl (namestring (sb-int:sbcl-homedir-pathname)))
  "Where is the home directory for the Lisp implementation,
in case we need it to (require ...) special features?
Default: whatever's the default for your implementation.")

(defvar *disable-cfasls* nil
  "Should we disable CFASL support when the target Lisp has it?")

(defvar *xcvb-verbosity* 5
  "Level of verbosity of XCVB:
0 - quiet
5 - usual warnings
9 - plenty of debug info")

(defvar *search-path* '()
  "Path to search for XCVB modules")

(defvar *lisp-allow-debugger* nil
  "Should we allow interactive debugging of failed build attempts?")

(defvar *object-directory* "obj"
  "where to store object files")

(defvar *tmp-directory* #p"/tmp/")

(defvar *use-base-image* t
  "Should we be using a base image for all builds?")

(defvar *xcvb-path* nil
  "XCVB Search path override")

(defun build-and-load
    (build &key
     setup
     xcvb-path
     output-path
     object-directory
     lisp-implementation
     lisp-binary-path
     disable-cfasl
     base-image
     verbosity
     profiling)
  ...)
