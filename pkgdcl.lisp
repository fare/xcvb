#+xcvb
(module
 (:description "package for XCVB"
  :depends-on ("lisp-invocation" "version" "xcvb-utils")))

(in-package :xcvb-driver)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *optimization-settings*
        `((speed 2) (safety 3) (compilation-speed 0) (debug 3)
          ,@*implementation-settings*))
  (proclaim-optimization-settings))

(defpackage :xcvb
  (:use :closer-common-lisp
        :xcvb-utils :command-line-arguments :lisp-invocation
        :fare-matcher :interface :pure :fare-mop
        :fare-memoization :inferior-shell
        #+xcvb-farmer :quux-iolib #+xcvb-farmer :iolib.os)

  (:import-from :asdf
   #:*default-source-registry-exclusions*
   #:*default-source-registries*
   #:inherit-source-registry)

  (:import-from :xcvb-driver
   #:get-xcvb-version #:get-xcvb-directory
   #:build-xcvb)

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
