;;; -*- mode: lisp -*-
(in-package :asdf)

(unless (or #+asdf2 (version-satisfies (asdf-version) "2.016"))
  (error "ASDF 2.016 or later required for XCVB bridge"))

(defsystem :xcvb-bridge
    :author ("Francois-Rene Rideau")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "XCVB bridge for ASDF"
    :long-description "A module to integrate XCVB builds into ASDF"
    :depends-on (:asdf :xcvb-driver)
    :components ((:file "bridge")))
