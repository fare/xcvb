;;; -*- mode: lisp -*-
(in-package :asdf)

(defsystem :xcvb-bridge
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "XCVB bridge for ASDF"
  :long-description "A module to integrate XCVB builds into ASDF"
  :depends-on ((:version :asdf "2.019") :xcvb-driver)
  :components ((:file "bridge")))
