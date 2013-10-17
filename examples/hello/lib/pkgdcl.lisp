;;; Package declaration for trivial library

#+xcvb (module ())

(cl:defpackage :xcvb-hello-lib
  (:use :common-lisp :command-line-arguments :uiop)
  (:export #:main #:main* #:exit ;; actually defined
           #:interpret-command-line)) ;; to be defined by your application
