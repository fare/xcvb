#+xcvb (module (:depends-on ("specials")))

(in-package :a2x-test)

(defun display-help (&key (stream *standard-output*))
  (format stream "~&a2x-test version ~A~%~%Usage: a2x-test [ARGUMENTS]*~%~%" +version+))
