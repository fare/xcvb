;;; -*- mode: lisp -*-

(asdf:defsystem :xcvb-master
    :author ("Francois-Rene Rideau")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "XCVB master"
    :long-description "A module to run XCVB as an external process, and use its results
in the current process"
    :depends-on (:xcvb-driver)
    :components ((:file "master")))
