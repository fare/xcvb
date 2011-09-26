#+xcvb
(module
 (:description "package for XCVB"))

(in-package :xcvb-driver)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *optimization-settings*
        `((speed 2) (safety 3) (compilation-speed 0) (debug 3)
          ,@*implementation-settings*))
  (proclaim-optimization-settings))

(defpackage :xcvb
  (:use :closer-common-lisp
        :xcvb-driver :command-line-arguments :asdf
        :fare-matcher :fare-utils :interface :pure :fare-mop
        #+xcvb-farmer :quux-iolib #+xcvb-farmer :iolib.os)

  #+xcvb-farmer
  (:shadowing-import-from :quux-iolib
   #:run-program/process-output-stream)

  (:shadowing-import-from :asdf
   #:absolute-pathname-p
   #:getenv
   #:merge-pathnames*
   #:pathname-directory-pathname
   #:while-collecting)

  (:import-from :asdf
   #:*default-source-registry-exclusions*
   #:*default-source-registries*
   #:coerce-pathname
   #:inherit-source-registry
   #:probe-file*
   #:user-homedir)

  (:import-from :xcvb-driver
   #:*debugging*)

  ;;; We have stopped trying to try to export a sensible interface
  ;;; through the package system.
  ;;; No wonder why: the CL package system is not very maintainable,
  ;;; and doesn't even scale to moderate size systems.
  ;;; Instead, we only export bare essentials.
  ;;; Happily, XCVB is an end-program, not a library meant to be reused.
  (:export
   #:*xcvb-version* ;; version
   #:module ;; Defining and using modules and extensions
   #:cmd)) ;; Easy REPL access to the command-line interface

(defpackage :xcvb-user
  (:use :common-lisp :xcvb-driver :xcvb))
