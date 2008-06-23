(xcvb:module
  (:fullname "com.itasoftware.xcvb.test"
   :nickname "xcvb-test"
   :licence "BSD"
   :description "testing the XCVB framework"
   :long-description "blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah"
   :compile-depends-on ("macros")
   :load-depends-on ("$tart"))
  (:add-load-depends-on "foo")
  (:remove-load-depends-on (:C "foo")))

(xcvb-test:start-xcvb-test)


;;:load-depends-on ((:S "imported") "start" "foo"))
