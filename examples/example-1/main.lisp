#+xcvb (module (:depends-on ("package")))

(in-package :xcvb-example-1)

(defun main (&rest arguments)
  (declare (ignore arguments))
  (format t "Hello World~%"))
