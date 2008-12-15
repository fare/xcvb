;;; Macros for XCVB

; we should be using it from alexandria or something

(defmacro with-gensyms (syms &body body)
  "Replaces given symbols with gensyms. Useful for creating macros.
This version by Paul Graham in On Lisp.
Mostly the same as cliki's WITH-UNIQUE-NAMES."
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms) ,@body))
