#+xcvb (module (:description "Unit test package for XCVB"
                :depends-on ("driver-test")))

(uiop/package:define-package #:xcvb-test
  (:mix :hu.dwim.stefil :xcvb :fare-utils :uiop
        :inferior-shell :lisp-invocation :alexandria :xcvb-driver)
  (:use :closer-common-lisp)
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
   #:run-program*
   #:run-program-backend*))

(in-package #:xcvb-test)

(defsuite* (xcvb-test
            :in root-suite
            :documentation "All XCVB unit tests"))
