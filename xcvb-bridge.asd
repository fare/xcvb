;;; -*- mode: lisp -*-
(in-package :asdf)

(defsystem :xcvb-bridge
  :defsystem-depends-on (:asdf)
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "XCVB bridge for ASDF"
  :long-description "A module to integrate XCVB builds into ASDF"
  :depends-on (:asdf :xcvb-driver)
  :components ((:file "bridge")))
