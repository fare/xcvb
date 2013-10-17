;; This is a comment. Testing whether rmx and a2x DTRT wrt this comment...

#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :a2x-test)

(defun main ()
  (if *command-line-arguments*
    (format t "arguments: ~S~%" *command-line-arguments*)
    (display-help))
  (quit 0))
