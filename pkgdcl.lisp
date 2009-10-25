#+xcvb
(module
 (:description "package for XCVB"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 2) (safety 3) (compilation-speed 0) (debug 3))))

(cl:defpackage :xcvb
  (:use :closer-common-lisp :xcvb-driver :command-line-arguments :xcvb-master)
  (:import-from :cl-launch #:DBG)

  ;;; THIS EXPORT LIST IS MASSIVELY OUT-OF-DATE.
  ;;; No wonder why: the CL package system is not very maintainable,
  ;;; and doesn't even scale to moderate size systems.
  (:export

     ;; version
     #:*xcvb-version*

     ;; Defining and using modules and extensions
     #:parse-module #:module-form-p
     #:module
     #:version
     #:author
     #:maintainer
     #:description
     #:long-description
     #:licence
     #:fullname
     #:build-module
     #:build-depends-on
     #:compile-depends-on
     #:cload-depends-on
     #:load-depends-on
     #:depends-on
     #:defextension
     #:extension-form

     ;; Utilities
     #:cmd
     #:DBG
     #:quit
     #:filepath
     #:strcat
     #:join-strings
     #:coerce-asdf-system-name
     #:asdf-systems-are-up-to-date-p

     ;; portablish-pathnames
     #:pathname-directory-pathname
     #:subpathname

     ;; Grains
     #:grain
     #:lisp-grain
     #:build-grain
     #:fasl-grain
     #:cfasl-grain

     #:build-pre-image-name
     #:build-image-name
     #:grain-computation
     #:grain-dependencies
     #:make-grain

     ;; Computations
     #:computation
     #:computation-inputs
     #:computation-outputs
     #:computation-command
     #:make-computation
     #:make-nop-computation
     #:make-phony-grain

     ;; registry
     #:registered-grain
     #:build-registry-conflict

     ;; names
     #:resolve-module-name

     ;; search-path
     #:*search-path*
     #:*search-path-searched-p*
     #:*archive-directory-names*
     #:default-search-path
     #:initialize-search-path
     #:set-search-path!
     #:finalize-search-path
     #:search-search-path

     ;; Main functions for using xcvb
     #:write-makefile
     #:write-asd-file

     ;; Compiler options
     #:*lisp-implementation*
     #:*lisp-executable-pathname*
     #:*lisp-image-pathname*
     #:*lisp-flags*
     #:*lisp-implementation-directory*

     ;; Target system configuration
     #:*use-cfasls*
     #:*target-system-features*

     ;; interpreting dependencies
     #:cfasl-for-fasl
     #:build-command-for

     ;; Converting ASDF systems.
     #:asdf-to-xcvb
     ))

(cl:defpackage :xcvb-user
  (:use :common-lisp :xcvb-driver :xcvb))
