(xcvb:module (
   :fullname "com.itasoftware.xcvb.test"
   :nickname "xcvb-test"
   :licence "BSD"
   :description "testing the XCVB framework"
   :long-description "blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah"
   ;:compile-depends-on ( )
   :load-depends-on ("start" (:asdf "foo")))

   (:set :this-module :build-requires ("sub/lib" (:asdf "foo")))

   (:add :this-module :load-depends-on ("macros"))
   (:set :this-module :load-depends-on ("start" "macros"))
  
)
