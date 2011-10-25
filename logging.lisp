#+xcvb (module (:depends-on ("specials")))

(in-package :xcvb)

(defun log-format (required-verbosity format &rest args)
  (when (>= (or *xcvb-verbosity* 5) required-verbosity)
    (fresh-line *error-output*)
    (apply #'format *error-output* format args)
    (fresh-line *error-output*)
    (force-output *error-output*)) ; more than useful on single-threaded-ccl
  (values))

(defun log-format-pp (require-verbosity format &rest args)
  (let ((*print-pretty* t))
    (apply 'log-format require-verbosity format args)))
