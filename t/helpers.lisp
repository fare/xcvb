#+xcvb (module (:depends-on ("package")))

(in-package #:xcvb-test)

(declaim (optimize (debug 3) (safety 3)))

(defun rm-rfv (x)
  (let* ((p (pathname x))
         (d (pathname-directory p))
         (n (namestring p)))
    (assert (not (wild-pathname-p p)))
    (assert (consp d))
    (assert (eq :absolute (first d)))
    (assert (<= 2 (length d)))
    (assert (directory-pathname-p p))
    (unless (search "xcvb" n)
      (break "Do you really want to rm -rfv ~A ???" n))
    (run-program/
     `("/bin/rm" "-rfv" ,n)
     :ignore-error-status t)))

(defun rsync (&rest args)
  (run `("rsync" ,@args)))

(defvar *driver* nil
  "path to the driver")

(defun find-driver ()
  (or *driver*
      (setf *driver* (asdf:system-relative-pathname :xcvb "driver.lisp"))))

(defvar *asdf* nil
  "path to asdf")

(defun find-asdf ()
  (or *asdf*
      (setf *asdf* (asdf:system-relative-pathname :asdf "asdf.lisp"))))
