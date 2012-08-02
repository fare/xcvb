#+xcvb (module (:description "Unit test package for XCVB"
                :depends-on ("driver-test")))

(in-package #:cl)

(defpackage #:xcvb-test
  (:use :hu.dwim.stefil :xcvb :closer-common-lisp :xcvb-utils
        :xcvb-driver-test :inferior-shell :lisp-invocation)
  (:import-from :xcvb-driver
   #:lisp-present-p
   #:+xcvb-lisps+)
  (:import-from :xcvb
   #:module-form-p #:read-module-declaration #:cmdize*
   #:repl-command #:eval-command #:errexit
   #:grain-pathname #:handle-global-options
   #:define-command #:define-option-spec #:*program*)
  (:export
   #:test-xcvb
   #:run-program/*
   #:run-program-backend/*))

(in-package #:xcvb-test)

(defsuite* (xcvb-test
            :in root-suite
            :documentation "All XCVB unit tests"))
