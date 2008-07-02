(xcvb:module
  :fullname "com.itasoftware.xcvb.test"
   :nickname "xcvb-test"
   :licence "BSD"
   :description "testing the XCVB framework"
   :long-description "blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah"
   :compile-depends-on ("macros" "pkgdcl" (:asdf "foo"))
   :load-depends-on ((:asdf "bar") "$tart")
   :build-depends-on ((:asdf "foobar")))

(xcvb-test:start-xcvb-test)


;;:load-depends-on ((:S "imported") "start" "foo"))
