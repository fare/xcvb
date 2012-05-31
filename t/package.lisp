#+xcvb (module (:description "Unit test package for XCVB"
                :depends-on ("driver-test")))

(in-package #:cl)

(defpackage #:xcvb-test
  (:use :hu.dwim.stefil :xcvb :xcvb-driver :closer-common-lisp
        :fare-utils :xcvb-driver-test :inferior-shell :lisp-invocation)
  (:shadowing-import-from :asdf
   #:subpathname)
  (:import-from :asdf
   #:coerce-pathname #:probe-file*
   #:ensure-directory-pathname #:directory-pathname-p)
  (:import-from :alexandria
   #:ensure-list)
  (:shadowing-import-from :xcvb-driver
   #:with-output #:lisp-present-p)
  (:import-from :xcvb-driver
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
