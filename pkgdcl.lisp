#+xcvb
(xcvb:module
  :description "package for XCVB")

(cl:defpackage :xcvb
  (:use :cl :xcvb-driver)
  (:import-from :xcvb-driver #:quit)
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
     #:coerce-asdf-system-name
     #:filepath
     #:strcat
     #:quit
     #:asdf-systems-are-up-to-date-p

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
     #:*lisp-options*
     #:*use-cfasls*

     ;; Converting ASDF systems.
     #:asdf-to-xcvb

     ;; Conditions
     #:no-build-file-found
     #:dependency-cycle))
