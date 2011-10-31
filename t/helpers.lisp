#+xcvb (module (:depends-on ("package")))

(in-package #:xcvb-test)

(declaim (optimize (debug 3) (safety 3)))

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

(defvar *driver* nil
  "path to the driver")

(defun find-driver ()
  (or *driver*
      (setf *driver* (first
                      (run-cmd/lines
                       xcvb 'find-module :name "/xcvb/driver" :short)))))

(defvar *asdf* nil
  "path to asdf")

(defun find-asdf ()
  (or *asdf*
      (setf *asdf* (first
                    (run-cmd/lines
                     xcvb 'find-module :name "/asdf/asdf" :short)))))
