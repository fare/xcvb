#+xcvb (module (:depends-on ("specials")))

(in-package :xcvb-hello)

(defun hello (&key traditional name)
  (format t (if traditional "hello, ~(~A~)~%" "Hello, ~A!~%") name))

(defun hello-command (arguments &key traditional name version help)
  (when help
    (display-help))
  (when version
    (display-version))
  (when arguments
    (error "Invalid arguments to hello: ~S~%" arguments))
  (hello :traditional traditional :name name))

(defun interpret-command-line (args)
  (multiple-value-bind (options arguments)
      ;;(handler-bind ((error (lambda (x) (declare (ignore x)) (error-help))))
        (command-line-arguments:process-command-line-options +hello-option-spec+ args);)
    (apply #'hello-command arguments options)))
