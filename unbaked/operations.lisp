;;; THIS FILE IS UNUSED, PROBABLY OBSOLETE, AND OUGHT TO DIE.
;;; Maybe recycle half-baked ideas from it before to delete it -- prior to v1.0

;;;; How I'd like to be able to write the program.

;;; using AP5-like techniques to generate the graph node classes
;;; from something more declarative that can generate indexes and back-pointers
;;; from a relational description

(define-grain-transformer :c
  :inputs ((x lisp-grain)) ;;; method will be applied to the evaluation of the argument expressions
  :outputs ((f fasl-grain) (c cfasl-grain)) ;;; can override the grain names (?)
  :body (...))
