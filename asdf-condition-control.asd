;;; -*- mode: lisp -*-
(in-package :asdf)

(defsystem :asdf-condition-control
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "Condition control for ASDF"
  :long-description "Using XCVB's condition control for ASDF"
  :depends-on ((:version :asdf "2.019") :xcvb-driver)
  :components ((:file "asdf-condition-control")))
