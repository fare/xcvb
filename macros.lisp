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

(defun simple-dispatcher (atom-processor function-hash environment expression)
  (if (consp expression)
    (let* ((head (car expression))
           (arguments (cdr expression))
           (function (or (gethash head function-hash)
                         (error "undefined function in ~S" expression))))
      (apply function environment arguments))
    (funcall atom-processor environment expression)))

(defmacro define-simple-dispatcher (name atom-interpreter &optional gf)
  (let ((hash-name (fintern t "*~A-FUNCTIONS*" name))
        (registrar-name (fintern t "REGISTER-~A" name))
        (definer-name (fintern t "DEFINE-~A" name))
        (dispatcher-name (fintern t "~A-DISPATCHER" name)))
    `(progn
       (defvar ,hash-name (make-hash-table :test 'eql))
       (defun ,registrar-name (symbol function)
         (setf (gethash symbol ,hash-name) function))
       (defmacro ,definer-name (symbol formals &body body)
         (let ((fname (fintern t "~A-~A" ',name symbol)))
           `(progn
              (,(if ',gf 'defmethod 'defun)
                ,(fintern t "~A-~A" ',name symbol) ,formals ,@body)
              (,',registrar-name ',symbol ',fname))))
       (defun ,dispatcher-name (environment expression)
         (simple-dispatcher
          ,atom-interpreter
          ,hash-name
          environment expression)))))
