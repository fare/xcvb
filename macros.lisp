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
  (cond
    ((null obj)
     (with-output-to-string (s) (funcall fun s)))
    ((eq obj t)
     (funcall fun *standard-output*))
    ((streamp obj)
     (funcall fun obj))
    (t
     (error "Invalid output stream specifier ~S" obj))))
