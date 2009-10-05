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

(def grain lisp-grain (file-grain)
  :filename "%.lisp"
  :documentation "LISP file grain")

(def grain fasl-grain (file-grain)
  :filename "%.fasl"
  :documentation "Lisp FASL file grain")

(def grain cfasl-grain (file-grain)
  :filename "%.cfasl"
  :documentation "Lisp CFASL file grain")

(def rule lisp-compile/cleanly
  :input ((lisp lisp-grain) &key
          (implementation lisp-implementation))
  :output ((fasl fasl-grain) &key
           (cfasl cfasl-grain))
  :binding ((dependencies (compile-dependencies lisp)))
  :dependencies (compile-dependencies lisp)
  :execute (in-process
            (lisp-process
             :implementation implementation
             :loaded dependencies)
            (compile-module (filename lisp) :fasl (filename fasl) :cfasl (filename cfasl))))
