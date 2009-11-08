;;; Package declaration for trivial library

#+xcvb (module ())

(cl:defpackage :xcvb-hello-lib
  (:use :common-lisp :command-line-arguments :cl-launch)
  (:export #:main #:exit ;; defined
           #:interpret-command-line ;; to be defined by your application
           ))
