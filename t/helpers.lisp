#+xcvb (module (:depends-on ("package")))

(in-package #:xcvb-unit-tests)

;; We add a newline to the end of a string and return it.
;; We do it in this specific manner so that under unix, windows and macos,
;; format will choose the correct type of newline delimiters
(defun nl (str)
  (format nil "~A~%" str))

(defun in-dir (dir sub)
  (merge-pathnames* (coerce-pathname sub) dir))

(defun ns-in-dir (dir sub)
  (namestring (is (in-dir dir sub))))

(defun run-cmd (&rest args)
  (run-program/echo-output (cmdize* args)))

(defun run-cmd/string (&rest args)
  (run-program/read-output-string (cmdize* args)))

(defun run-cmd/lines (&rest args)
  (run-program/read-output-lines (cmdize* args)))

(defun run-make (dir &rest args)
  (apply 'run-cmd "make" "-C" dir args))

(defun rm-rfv (x)
  (let* ((p (pathname x)))
    (assert (not (wild-pathname-p p)))
    (assert (absolute-pathname-p p))
    (assert (consp (pathname-directory p)))
    (assert (<= 4 (length (pathname-directory p))))
    (run-program/echo-output
     `("/bin/rm" "-rfv" (namestring p))
     :ignore-error-status t)))

(defun rsync (&rest args)
  (apply 'run-cmd "rsync" args))
