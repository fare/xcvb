#+xcvb (module (:description "Unit test package for XCVB"))

(in-package #:cl)

(defpackage #:xcvb-unit-tests
  (:use :hu.dwim.stefil :xcvb :xcvb-driver :closer-common-lisp
        :fare-utils)
  (:import-from :asdf
                #:coerce-pathname #:probe-file*
                #:ensure-directory-pathname #:directory-pathname-p)
  (:import-from :xcvb
                #:module-form-p #:read-module-declaration #:cmdize*)
  (:export
   #:test-xcvb
   #:run-program/*
   #:run-program-backend/*))

(in-package #:xcvb-unit-tests)

(defsuite* (test-xcvb
            :in root-suite
            :documentation "All XCVB unit tests"))
