#+xcvb (module (:depends-on ("pkgdcl")))

;;; Macros for XCVB

(in-package :xcvb)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fintern (package format &rest rest)
    (intern (apply #'format nil format rest)
            (find-package
             (cond
               ((null package) :keyword)
               ((eql package t) *package*)
               (t package)))))
  (defun kintern (format &rest rest)
    (apply #'fintern nil format rest))
  (defun keywordify (x)
    (with-safe-io-syntax () (kintern "~A" x))))

;;; Simple Dispatcher

(defun simple-dispatcher (debug-name atom-processor function-hash environment expression)
  (if (consp expression)
    (let* ((head (car expression))
           (arguments (cdr expression))
           (function (or (gethash head function-hash)
                         (error "Simple Dispatcher[~A]: Error: No associated dispatch function for the keyword in car position of expression ~S" debug-name expression))))
      (apply function environment arguments))
    (if atom-processor
        (funcall atom-processor environment expression)
        (error "Simple Dispatcher[~A]: Error: Invalid atom ~S" debug-name expression))))

(defmacro define-simple-dispatcher (name atom-interpreter &key generic (environment t))
  (let ((hash-name (fintern t "*~A-FUNCTIONS*" name))
        (registrar-name (fintern t "REGISTER-~A" name))
        (definer-name (fintern t "DEFINE-~A" name))
        (dispatcher-name (fintern t "~A-DISPATCHER" name))
	(debug-name (format nil "~(~S~)" name))
        (env (gensym "ENVIRONMENT")))
    `(progn
       (defvar ,hash-name (make-hash-table :test 'eql))
       (defun ,registrar-name (symbol function)
         (setf (gethash symbol ,hash-name) function))
       (defmacro ,definer-name (symbol formals &body body)
         (let ((fname (fintern t "~A-~A" ',name symbol)))
           `(progn
              (,',(if generic 'defmethod 'defun)
                ,(fintern t "~A-~A" ',name symbol)
                (,@',(unless environment `(,env)) ,@formals)
                ,@',(unless environment `((declare (ignore ,env))))
                ,@body)
              (,',registrar-name ',symbol ',fname))))
       (defun ,dispatcher-name (,@(when environment `(,env)) expression)
         (simple-dispatcher
	  ,debug-name
          ,atom-interpreter
          ,hash-name
          ,(if environment env nil) expression)))))



;;; Create a local context for cmd

(defun all-xcvb-vars ()
  (remove-duplicates
   (loop :for pkg-name :in '(:xcvb :xcvb-driver)
     :for pkg = (find-package pkg-name) :append
     (loop :for sym :being :the :present-symbols :of pkg
       :when (and (boundp sym) (not (constantp sym)))
       :collect sym))))

(defun call-with-local-xcvb-vars (thunk)
  (let* ((vars (all-xcvb-vars))
         (vals (mapcar 'symbol-value vars)))
    (progv vars vals (funcall thunk))))

(defmacro with-local-xcvb-vars (() &body body)
  `(call-with-local-xcvb-vars (lambda () ,@body)))
