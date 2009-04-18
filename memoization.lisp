;;; Initially taken from http://fare.tunes.org/files/fun/fibonacci.lisp

(cl:defpackage #:memoization
  (:use #:common-lisp)
  (:export #:memoize #:unmemoize
	   #:define-memo-function
	   #:memoizing #:memo-lambda))

(in-package :memoization)

(defun compute-memoized-function (f h args)
  "the basic helper for computing with a memoized function F,
with a hash-table H, being called with arguments ARGS"
  (multiple-value-bind (results foundp) (gethash args h)
    (if foundp (apply #'values results)
	(let ((results (multiple-value-list (apply f args))))
	  (setf (gethash args h) results)
	  (apply #'values results)))))
(declaim (inline compute-memoized-function))

(defun unmemoize (sym)
  "undoing the memoizing function, return the hash of memoized things so far"
  (let ((r (get sym :original-memoized-function)))
    (when r
      (setf (symbol-function sym) (car r))
      (remprop sym :original-memoized-function)
      (cdr r))))

(defun memoize (sym &optional (h (make-hash-table :test 'equal)))
  "a pretty generic memoizing function to speed things up"
  (unmemoize sym)
  (let ((f (symbol-function sym)))
    (setf (symbol-function sym)
	  #'(lambda (&rest args)
	      (compute-memoized-function f h args))
	  (get sym :original-memoized-function)
	  (cons f h))))

(defun memoizing (f)
  (let ((h (make-hash-table :test 'equal)))
    #'(lambda (&rest args)
	(compute-memoized-function f h args))))

(defmacro memo-lambda (formals &body body)
  `(memoizing #'(lambda ,formals ,@body)))

;;(defmacro define-memo-function (name formals &body body)
;;  `(progn (defun ,name ,formals ,@body) (memoize ',name)))
(defmacro define-memo-function (name formals &body body)
  (let ((args (gensym "ARGS"))
        (h (gensym "HASH"))
        (fun (gensym (symbol-name name))))
    `(let ((,h (make-hash-table :test 'equal)))
       (defun ,name (&rest ,args)
         (labels ((,fun ,formals (block ,name ,@body))
                  (,name (&rest ,args) (compute-memoized-function #',fun ,h ,args)))
           (apply #',name ,args))))))
