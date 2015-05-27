(defsystem "xcvb-bridge"
  :version (:read-file-line "version.text")
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "XCVB bridge for ASDF"
  :long-description "A module to integrate XCVB builds into ASDF"
  :depends-on ((:version "asdf" "3.1.4") "xcvb-driver")
  :components ((:file "bridge")))
