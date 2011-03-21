;;; -*- mode: lisp -*-

(asdf:defsystem :xcvb-bridge
    :author ("Francois-Rene Rideau")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "XCVB bridge for ASDF"
    :long-description "A module to integrate XCVB builds into ASDF"
    :depends-on (:asdf :xcvb-master)
    :components ((:file "asdf-xcvb-bridge")))
