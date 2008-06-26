(xcvb:module
  :description "macros for XCVB test"
  :origin "/home/sbrody/xcvb/test/"
  :build-depends-on ("pkgdcl" (:asdf "foo"))
  
  )
;(:asdf "foo")
;:build-depends-on ("pkgdcl" (:asdf "foo"))



(in-package :xcvb-test)


(defmacro incf (x)
   '(setf x (+ x 1)))

