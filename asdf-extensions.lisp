(in-package :asdf)

(defun asdf-system-is-up-to-date-p (operation-class system &rest args)
  "Takes a name of an asdf system (or the system itself) and a asdf operation
and returns a boolean indicating whether or not anything needs to be done
in order to perform the given operation on the given system.
This returns whether or not the operation has already been performed,
and none of the source files in the system have changed since then"
  (let* ((op (apply #'make-instance operation-class
		    :original-initargs args args))
	 (*verbose-out*
	  (if (getf args :verbose t)
            *trace-output*
            (make-broadcast-stream)))
	 (system (if (typep system 'component) system (find-system system)))
	 (steps (traverse op system)))
    ;(format T "~%that system is ~:[out-of-date~;up-to-date~]" (null steps))
    (null steps)))
 
(defun asdf-systems-are-up-to-date-p (&rest systems)
  "Takes a list of names of asdf systems, and
exits lisp with a status code indicating
whether or not all of those systems were up-to-date or not."
  (xcvb:quit
   :exit-status (if (every (lambda (system) (asdf-system-is-up-to-date-p 'asdf:load-op system))
			   systems)
		    0 1)))
