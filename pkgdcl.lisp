#|(xcvb:module
  :description "package for XCVB")|#

(cl:defpackage :xcvb
  (:use :cl)
  (:export
     #:module
     #:module-declaration
     #:compile-dependencies
     #:load-dependencies
     #:compile-module
     #:load-module
     #:run-tests))
