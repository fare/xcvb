;;; Shell command-line interface for XCVB

(in-package :xcvb)

(defun reset-variables ()
  (setf *grains* (make-hash-table :test 'equal)
        *computations* nil
        *target-system-features* nil
        *search-path-searched-p* nil
        *pathname-grain-cache* (make-hash-table :test 'equal))
  (values))


(defun usage (&optional (out *standard-output*))
  (format out "~&Usage: xcvb FILE.lisp~%  ~
	where FILE.lisp configures xcvb and starts computation.~%  ~
	see xcvb/doc/README.rest (when it is completed).~%~%"))

(defun main ()
  (cond
    ((eql 1 (length cl-launch:*arguments*))
     (let ((x (car cl-launch:*arguments*)))
       (if (member x '("-?" "--help" "-h") :test #'equal)
	   (usage)
	   (let ((*package* (find-package :xcvb-user)))
	     (load x)))
       (quit 0)))
    (t
     (usage *error-output*)
     (quit 1))))
