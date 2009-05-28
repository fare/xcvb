(in-package :xcvb)

(defun log-format (required-verbosity format &rest args)
  (when (>= *xcvb-verbosity* required-verbosity)
    (apply #'format format *error-output* args)))
