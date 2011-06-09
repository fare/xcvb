#+xcvb (module ())

(cl:defpackage :xcvb-hello
  (:use :common-lisp :xcvb-driver :xcvb-hello-lib)
  (:export #:main #:main*)) ;; from lib/
