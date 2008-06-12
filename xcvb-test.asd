;;; -*- mode: lisp -*-
(asdf:defsystem :xcvb-test
    :name "xcvb-test"
    :author "Spencer Brody"
    ;; :version "0.1"
    :maintainer '("Spencer Brody" "Francois-Rene Rideau")
    :licence "MIT"
    :description      "Tests for XCVB"
    :long-description "Tests for XCVB: an eXtensible Component Verifier and Builder for Lisp"
    :depends-on (:xcvb)
    :serial t
    :components ((:file "xcvb-test")))
