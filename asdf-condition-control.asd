;;; -*- mode: lisp -*-
;;; superseded by asdf-driver
(in-package :asdf)

(defsystem :asdf-condition-control
  :defsystem-depends-on (:asdf)
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "Condition control for ASDF"
  :long-description "Using XCVB's condition control for ASDF"
  :depends-on (:asdf #-asdf3 :asdf-driver))
