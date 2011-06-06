#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :xcvb)

;;; Conditions

(define-condition user-error (simple-error)
  ;; This condition is for user-errors. A user error does NOT trigger a backtrace.
  ())

(define-condition dependency-cycle (user-error)
  ;; This condition is signaled if the dependency graph has any cycles in it.
  ())

(define-condition syntax-error (user-error)
  ;; Condition is signaled if there is some syntax error in some user-specified data
  ())

(define-condition grain-not-found (user-error)
  ;; Condition is signaled if there is some syntax error in some user-specified data
  ())

;; This dumps a backtrace
(defun simply-error (simple-error control &rest args)
  (error (or simple-error 'simple-error)
         :format-control control :format-arguments args))

(defun user-error (control &rest args)
  (apply 'simply-error 'user-error
	 (concatenate 'string "Error: " control)
	 args))
