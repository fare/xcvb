(xcvb:module
  :description "macros for XCVB test"
  :origin "/home/sbrody/xcvb/test/"
  :build-depends-on ("pkgdcl" (:asdf "foo"))
  
  )




(in-package :xcvb-test)

(defmacro foo (x)
  `(format t "~S ~S~%" ',x ,x))

:compile-depends-on ("BUILD")
