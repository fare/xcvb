(xcvb:module
  :description "macros for XCVB test"
  :origin "/home/sbrody/xcvb/test/sub/"
  :build-depends-on ("pkgdcl")
  )



(in-package :xcvb-test)

(defun sum (x y)
 (+ x y))

;;  :compile-depends-on ("macros.lisp")
;;  :load-depens-on ("sub/macros.lisp"))

;;  :compile-depends-on ("start")
