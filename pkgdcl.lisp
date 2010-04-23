#+xcvb
(module
 (:description "package for XCVB"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 2) (safety 3) (compilation-speed 0) (debug 3))))

(cl:defpackage :xcvb
  (:use :closer-common-lisp
        :xcvb-driver :command-line-arguments :xcvb-master
        :fare-utils :interface :pure)

  (:import-from :asdf
   ;;#:*default-exclusions*
   #:*default-source-registries*
   #:inherit-source-registry)

  ;;; We have stopped trying to try to export a sensible interface
  ;;; through the package system.
  ;;; No wonder why: the CL package system is not very maintainable,
  ;;; and doesn't even scale to moderate size systems.
  ;;; Instead, we only export bare essentials.
  ;;; Happily, XCVB is an end-program, not a library meant to be reused.
  (:export
     ;; version
     #:*xcvb-version*

     ;; Defining and using modules and extensions
     #:module

     ;; Easy REPL access to the command-line interface
     #:cmd))

(cl:defpackage :xcvb-user
  (:use :common-lisp :xcvb-driver :xcvb))
