;;; -*- mode: lisp -*-

(asdf:defsystem :xcvb-bridge
    :author ("Francois-Rene Rideau")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "XCVB bridge for ASDF"
    :long-description "A module to integrate XCVB builds into ASDF"
    :depends-on (;; :asdf ; not safe unless everyone uses ASDF >= 2.014.8
                 :xcvb-master)
    :components ((:file "bridge")))
