(defsystem "xcvb-bootstrap"
  :version (:read-file-line "version.text")
  :author ("Francois-Rene Rideau" "Peter Keller")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "Bootstrapping an XCVB binary from ASDF"
  :long-description "This system allows you to create an XCVB binary
using an automatically detected supported host implementation and
install it in a known location, from an arbitrary current Lisp implementation."
  :depends-on ("lisp-invocation" "xcvb-driver")
  :components ((:file "bootstrap")))
