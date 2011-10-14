#+xcvb (module (:depends-on ("package")))

(in-package #:xcvb-test)

(declaim (optimize (debug 3) (safety 3)))

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

(defun rm-rfv (x)
  (let* ((p (pathname x))
         (d (pathname-directory p))
         (n (namestring p)))
    (assert (not (wild-pathname-p p)))
    (assert (consp d))
    (assert (eq :absolute (first d)))
    (assert (<= 4 (length d)))
    (assert (directory-pathname-p p))
    (unless (search "xcvb" n)
      (break "Do you really want to rm -rfv ~A ???" n))
    (run-program/echo-output
     `("/bin/rm" "-rfv" ,n)
     :ignore-error-status t)))

(defun rsync (&rest args)
  (apply 'run-cmd "rsync" args))
