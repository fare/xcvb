#+xcvb (module (:depends-on ("specials")))

(in-package :xcvb-hello)

(defun display-help (&key (stream *standard-output*) (code 0))
  (format stream "~&Usage: hello [OPTIONS]*~%~%")
  (command-line-arguments:show-option-help
   +hello-option-spec+
   :stream stream :sort-names t)
  (exit code))

(defun error-help ()
  (display-help :stream *error-output* :code 2))

(defun display-version ()
  (cond
    (*version*
     (format t "XCVB hello version ~A~%" *version*)
     (exit 0))
    (t
     (format *error-output* "XCVB hello version not defined!~%")
     (exit 2))))
