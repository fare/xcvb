#|(xcvb:module
  :description "package for XCVB")|#

(cl:defpackage :xcvb
  (:use :cl :xcvb-driver)
  (:import-from :xcvb-driver #:quit)
  (:export
     #:module
     #:create-dependency-graph
     #:create-dump-image-graph
     #:defextension
     #:asdf-systems-are-up-to-date-p
     #:quit
     #:quit-form
     ;;Main functions for using xcvb
     #:write-makefile
     #:write-asd-file
     #:convert-asdf-system-to-xcvb
     ;;compiler options
     #:*lisp-implementation*
     #:*lisp-executable-pathname*
     #:*lisp-image-pathname*
     #:*lisp-options*
     #:*use-cfasls*
     ;;Conditions
     #:no-build-file-found
     #:dependency-cycle))
