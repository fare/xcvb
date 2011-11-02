;;; -*- mode: lisp -*-
(in-package :asdf)

(unless (or #+asdf2 (version-satisfies (asdf-version) "2.018"))
  (error "ASDF 2.018 or later required for XCVB bridge"))

(defsystem :xcvb-bridge
    :author ("Francois-Rene Rideau")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "XCVB bridge for ASDF"
    :long-description "A module to integrate XCVB builds into ASDF"
    :depends-on ((:version :asdf "2.018") :xcvb-driver)
    :components ((:file "bridge")))
