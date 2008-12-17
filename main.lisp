;;; Shell command-line interface for XCVB

(in-package :xcvb)


(defun asdf-to-xcvb ()
#|
  (setf *systems-to-preload* ...) ;; get it from some argument
  (setf *systems-to-convert* ...) ;; get it from some argument
  ... have some mechanism to specify overrides...
  ... use ADG to compute dependencies into migrated-system-components.lisp-expr...
  ... Splice migrated-system-components.lisp-expr into a new migrated-system.asd...
  ... (xcvb::convert-asdf-system-to-xcvb :quux) ...
  ... make clean ...
  ...
|#
  nil)

(defun build ()
#|
  ... Create an image with the setup loaded ... (see load-setup-and-die)
  ... (setf xcvb:*lisp-executable-pathname* ...) ... the path to the image you dumped in
  ... (xcvb::write-makefile "/ita/devel/qres/lisp/quux/BUILD.lisp" "/ita/devel/qres/lisp/quux/") ...
  ... make -f /ita/devel/qres/lisp/quux/Makefile.xcvb lisp-image.core ...
|#
  nil)

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
	   (load x))
       (quit :exit-status 0)))
    (t
     (usage *error-output*)
     (quit :exit-status 1))))
