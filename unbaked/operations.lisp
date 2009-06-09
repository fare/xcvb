;;; THIS FILE IS UNUSED, PROBABLY OBSOLETE, AND OUGHT TO DIE.
;;; Maybe recycle half-baked ideas from it before to delete it -- prior to v1.0

;;; If we end up with ASDF-like "operations", here's what we'll do...

(in-package :xcvb)

(defclass operation ()
  ())

(defclass build (operation))
(defclass compile (operation))

(defparameter $build (make-the 'build))
(defparameter $compile (make-the 'compile))




;;;; How I'd like to be able to write the program.

;;; using AP5-like techniques to generate the graph node classes
;;; from something more declarative that can generate indexes and back-pointers
;;; from a relational description

#+nil (progn

(define-grain-transformer :c
  :inputs ((x lisp-grain)) ;;; method will be applied to the evaluation of the argument expressions
  :outputs ((f fasl-grain) (c cfasl-grain)) ;;; can override the grain names (?)
  :body (...))
