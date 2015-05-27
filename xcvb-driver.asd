(defsystem "xcvb-driver"
  :version (:read-file-line "version.text")
  :author ("Francois-Rene Rideau" "Spencer Brody")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "XCVB Driver"
  :long-description
  "a minimal driver to be loaded in target system when building software with XCVB"
  :depends-on () ; The very idea of it is that it should be standalone, without dependencies
  :components ((:file "driver")))
