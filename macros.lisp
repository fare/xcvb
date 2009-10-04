#+xcvb (module (:depends-on ("pkgdcl")))

;;; Macros for XCVB

(in-package :xcvb)

(defmacro with-gensyms (syms &body body)
  "Replaces given symbols with gensyms. Useful for creating macros.
This version by Paul Graham in On Lisp.
Mostly the same as cliki's WITH-UNIQUE-NAMES."
  ;; Note: we probably should be using it from alexandria or something
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s)))) syms) ,@body))

(define-modify-macro funcallf (f &rest args) xfuncall)

(defmacro with-output ((out &optional (obj out)) &body body)
  `(call-with-output ,obj #'(lambda (,out) ,@body)))

(defun call-with-output (obj fun)
  "Calls FUN with an actual stream argument, behaving like FORMAT with respect to stream'ing:
If OBJ is a stream, use it as the stream.
If OBJ is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OBJ is T, use *STANDARD-OUTPUT* as the stream.
If OBJ is a string with a fill-pointer, use it as a string-output-stream.
Otherwise, signal an error."
  (etypecase obj
    (stream
     (funcall fun obj))
    ((eql t)
     (funcall fun *standard-output*))
    (null
     (with-output-to-string (s) (funcall fun s)))
    ((and string (satisfies array-has-fill-pointer-p))
     (with-output-to-string (s obj) (funcall fun s)))))

(defmacro with-safe-io-syntax ((&key (package :cl-user)) &body body)
  `(call-with-safe-read-io-syntax (lambda () ,@body) :package ,package))

(defun call-with-safe-io-syntax (thunk &key (package :cl-user))
  (with-standard-io-syntax ()
    (let ((*package* (find-package package))
	  (*read-eval* nil))
      (funcall thunk))))

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
    (with-standard-io-syntax (kintern "~A" x))))

;;; Nesting binding forms (from a suggestion by marco baringer)
(defmacro with-nesting (() &rest things)
  (reduce #'(lambda (outer inner) (append outer (list inner)))
          things :from-end t))

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
