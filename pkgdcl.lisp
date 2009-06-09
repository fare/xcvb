#+xcvb
(module
 (:description "package for XCVB"))

(cl:defpackage :xcvb
  (:use :cl :xcvb-driver :closer-mop :command-line-arguments)
  (:import-from :cl-launch #:DBG)
  (:export

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
     #:build-requires
     #:compile-depends-on
     #:load-depends-on
     #:defextension
     #:extension-form

     ;; Utilities
     #:DBG
     #:quit
     #:define-combinator-interpreter
     #:filepath
     #:strcat
     #:join-strings
     #:all-descendents-f
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

     ;; Lisp Grains
     #:depends-on
     #:compile-depends-on
     #:load-depends-on

     ;; Create graphs
     #:create-dependency-graph
     #:create-dump-image-graph

     ;; Main functions for using xcvb
     #:write-makefile
     #:write-asd-file

     ;; Compiler options
     #:*lisp-implementation*
     #:*lisp-executable-pathname*
     #:*lisp-image-pathname*
     #:*lisp-image-name* ;; fullname instead of pathname
     #:*lisp-options*
     #:*use-cfasls*

     ;; interpreting dependencies
     #:compile-time-fasl-type
     #:cfasl-for-fasl
     #:load-command-for

     ;; Converting ASDF systems.
     #:asdf-to-xcvb

     ;; Conditions
     #:no-build-file-found
     #:dependency-cycle))

(cl:defpackage :xcvb-user
  (:use :common-lisp :xcvb-driver :xcvb))
