;;; Shell command-line interface for XCVB

(in-package :xcvb)

(defun usage (&optional (out *standard-output*))
  (format out "~&Usage: xcvb FILE.lisp~%  ~
	where FILE.lisp configures xcvb and starts computation.~%  ~
	see xcvb/doc/README.rest (when it is completed).~%~%"))

(defun main ()
  (cond
    ((eql 1 (length cl-launch:*arguments*))
     (let ((x (car cl-launch:*arguments*)))
       (if (member x '("-?" "--help" "-h") :test 'equal)
	   (usage)
	   (let ((*package* (find-package :xcvb)))
	     (load x)))
       (quit :exit-status 0)))
    (t
     (usage *error-output*)
     (quit :exit-status 1))))
