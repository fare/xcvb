(xcvb:module
  :description "macros for XCVB test"
  :origin "/home/sbrody/xcvb/test/"
  :load-depends-on ("sub/lib")
  :build-depends-on ("pkgdcl"))

(in-package :xcvb-test)

(defun start-xcvb-test ()
   (format nil (sum 1 2)))
