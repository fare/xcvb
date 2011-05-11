#+xcvb (module (:depends-on ("specials")))

(in-package :xcvb)

(defun log-format (required-verbosity format &rest args)
  (when (>= *xcvb-verbosity* required-verbosity)
    (fresh-line *error-output*)
    (apply #'format *error-output* format args)
    (fresh-line *error-output*))
  (values))

(defun log-format-pp (require-verbosity format &rest args)
  (let ((*print-pretty* t))
    (apply 'log-format require-verbosity format args)))