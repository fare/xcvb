(asdf:defsystem :/com.itasoftware.xcvb.test
 :licence "BSD"
 :description "testing the XCVB framework"
 :long-description "blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah"
 :components
 ((:file "pkgdcl")
  (:file "macros" :depends-on ("pkgdcl"))
  (:file "BUILD" :depends-on ("macros"))
  )
 :depends-on(:foo :foo))

(cl:pushnew :/com.itasoftware.xcvb.test *features*)