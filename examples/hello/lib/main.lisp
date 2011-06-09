;;; This trivial library shows how to write a main function
;;; that consumes and interprets command-line-arguments,
;;; then returns an error code.

#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :xcvb-hello-lib)

(defun main* ()
  (apply 'main (get-command-line-arguments)))

(defun main (&rest arguments)
  (quit
   (catch :exit
     (handler-bind
         ((error (lambda (error) (format *error-output* "~A~%" error) (exit 10))))
       (interpret-command-line arguments)
       0))))

(defun exit (&optional (code 0) &rest r)
  (declare (ignore r))
  (throw :exit code))
