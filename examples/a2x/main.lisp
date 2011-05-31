;; This is a comment. Testing whether rmx and a2x DTRT wrt this comment...

#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :a2x-test)

(defun main ()
  (if cl-launch:*arguments*
    (format t "arguments: ~S~%" cl-launch:*arguments*)
    (display-help))
  (cl-launch:quit 0))
