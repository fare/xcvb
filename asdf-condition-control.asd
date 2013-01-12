;;; -*- mode: lisp -*-
(in-package :asdf)

(defsystem :asdf-condition-control
  :defsystem-depends-on (:asdf)
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "Condition control for ASDF"
  :long-description "Using XCVB's condition control for ASDF"
  :depends-on (:asdf :xcvb-driver)
  :components ((:file "asdf-condition-control")))
