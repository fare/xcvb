#+xcvb (module (:description "Unit test package for XCVB"))

(in-package #:cl)

(defpackage #:xcvb-unit-tests
  (:use :hu.dwim.stefil :xcvb :xcvb-driver :xcvb-master :closer-common-lisp
        :fare-utils)
  (:export
   #:run-program/*
   #:run-program-backend/*))

(in-package #:xcvb-unit-tests)

;; We add a newline to the end of a string and return it. We do it in
;; this specific manner so that under unix and windows format will
;; choose the correct type of newline delimiters
(defun nl (str)
  (format nil "~A~%" str))

;; Sometimes we only want forms to be present when we've been compiled
;; on a unix machine of any kind
(defmacro using-unix (&body body)
  (declare (ignorable body))
  (or #+os-unix `(progn ,@body)))

;; Sometimes we only want forms to be present when we've been compiled
;; on a windows machine of any kind
(defmacro using-windows (&body body)
  (declare (ignorable body))
  (or #+os-windows `(progn ,@body)))
