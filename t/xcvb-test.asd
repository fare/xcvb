;;; -*- mode: lisp -*-

(defsystem :xcvb-test
  :author "Peter Keller & Francois-Rene Rideau"
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "Tests for XCVB"
  :depends-on (:xcvb :hu.dwim.stefil :cl-ppcre :xcvb-driver-test :inferior-shell)
  :components
  ((:file "package")
   (:file "helpers" :depends-on ("package"))
   (:file "specials" :depends-on ("package"))
   (:file "run-program-backend" :depends-on ("helpers" "specials"))
   (:file "sub-xcvb" :depends-on ("helpers" "specials"))
   (:file "release" :depends-on ("sub-xcvb"))
   (:file "main" :depends-on ("package"))))
