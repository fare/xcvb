;;; Macros for XCVB

(in-package :xcvb)

(defmacro with-gensyms (syms &body body)
  "Replaces given symbols with gensyms. Useful for creating macros.
This version by Paul Graham in On Lisp.
Mostly the same as cliki's WITH-UNIQUE-NAMES."
  ;; Note: we probably should be using it from alexandria or something
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms) ,@body))


(defmacro with-output ((out &optional (obj out)) &body body)
  `(call-with-output ,obj (lambda (,out) ,@body)))

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
     (with-output-to-string (s obj)
       (funcall fun s)))))


(eval-when (:compile-toplevel :load-toplevel)
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
    (kintern "~A" x)))

;;; Collecting data

(defmacro while-collecting ((&rest collectors) &body body)
  (let ((vars (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
        (initial-values (mapcar (constantly nil) collectors)))
    `(let ,(mapcar #'list vars initial-values)
       (flet ,(mapcar #'(lambda (c v) `(,c (x) (push x ,v))) collectors vars)
         ,@body
         (values ,@(mapcar #'(lambda (v) `(nreverse ,v)) vars))))))
