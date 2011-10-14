;;; -*- mode: lisp -*-

;; I would have liked to use :hu.dwim.stefil, but
;; it's not nearly portable to all the platforms
;; xcvb-driver is meant to support.

(defsystem :xcvb-driver-test
  :author "Peter Keller & Francois-Rene Rideau"
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "Tests for XCVB-Driver"
  :depends-on (:xcvb-driver)
  :components
  ((:file "driver-test")))
