;;; -*- mode: lisp -*-

(defsystem :xcvb-unit-tests
  :author "Peter Keller & Francois-Rene Rideau"
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "Tests for XCVB"
  :depends-on (:xcvb :hu.dwim.stefil :cl-ppcre)
  :components
  ((:file "package")
   (:file "helpers" :depends-on ("package"))
   (:file "specials" :depends-on ("package"))
   (:file "run-program" :depends-on ("helpers"))
   (:file "run-program-backend" :depends-on ("helpers" "specials"))
   (:file "sub-xcvb" :depends-on ("helpers" "specials"))))
