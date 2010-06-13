;;; THIS FILE HAS UNBAKED IDEAS, THAT NEED TO BE COOKED OR TRASHED
;;; How I'd like to be able to write the program.

;;; using AP5-like techniques to generate the graph node classes
;;; from something more declarative that can generate indexes and back-pointers
;;; from a relational description?

(define-grain-transformer :c
  :inputs ((x lisp-grain)) ;;; method will be applied to the evaluation of the argument expressions
  :outputs ((f fasl-grain) (c cfasl-grain)) ;;; can override the grain names (?)
  :body (...))

(def grain lisp-grain (file-grain)
  :filename "%.lisp"
  :documentation "LISP file grain")


; load a grain into an image
; load a grain's compilation data into an image



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

(defun src-name (name &optional type)
  `(:src ,(if type (strcat name "." type) name)))
(defun obj-name (name &optional type)
  `(:obj ,(if type (strcat name "." type) name)))


(defparameter *exe-extension* #-windows nil #+windows "exe") ;; from cffi/grovel/grovel.lisp

(define-dependency ((:cffi-grovel-lisp (build-relative-name name))
                    &environment env)
  "This XCVB dependency represents files
operations that take care of calling PROCESS-GROVEL-FILE in order
to generate a Lisp file that is subsequently compiled and/or
loaded."))
  :build
  (with-grains-from-virtual-pathname
      ((grovel-lisp (src-name name "lisp"))
       (grovel-c (obj-name name "c"))
       (grovel-exe (obj-name *exe-extension*))
       (grovel-generated-lisp (obj-name "-tmp.lisp"))
       (grovel-fasl (obj-name "fasl")))
    (make-computation
     env
     :inputs (list grovel-lisp)
     :outputs (list grovel-c grovel-exe grovel-generated-lisp)
     :context (lisp-context :include '((:build "cffi")))
     :command `(:call :process-grovel-file :cffi-grovel
                      ,(node-vp grovel-lisp)
                      ,(node-vp grovel-c)))
    (make-fasl-computation
     env
     grovel-generated-lisp
     grovel-fasl
     '((:build "cffi")))))

(define-dependency ((:cffi-grovel-wrapper (build-relative-name name)
                                          &key cc-flags soname)
                    &environment env)
  :build
  (with-grains-from-virtual-pathname
      ((grovel-lisp (src-name name "lisp"))
       (grovel-c (obj-name name "c"))
       (grovel-exe (obj-name "exe"))
       (grovel-generated-lisp (obj-name "-tmp.lisp")))
       (grovel-fasl (obj-name "fasl")))


(defclass wrapper-file (asdf:cl-source-file cc-flags-mixin)
  ((soname :initform nil :initarg :soname :accessor soname-of))
  (:documentation
   "This ASDF component defines COMPILE-OP and LOAD-SOURCE-OP
operations that take care of calling PROCESS-WRAPPER-FILE in
order to generate a foreign library and matching CFFI bindings
that are subsequently compiled and/or loaded."))

(defun %perform-process-wrapper-file (op c)
  (let ((fasl-file (ensure-pathname (car (asdf:output-files op c)))))
    (values (process-wrapper-file (asdf:component-pathname c)
                                  fasl-file
                                  (or (soname-of c)
                                      (asdf:component-name c)))
            fasl-file)))

(defmethod asdf:perform ((op asdf:compile-op) (c wrapper-file))
  (multiple-value-bind (generated-source-file fasl-file)
      (%perform-process-wrapper-file op c)
    (compile-file generated-source-file
                  :output-file fasl-file
                  #+ecl :system-p #+ecl t)))

(defmethod asdf:perform ((op asdf:load-source-op) (c wrapper-file))
  (load (%perform-process-wrapper-file op c)))

(defun process-wrapper-file (input-file output-defaults lib-soname)
  (with-standard-io-syntax
    (let ((lib-file
           (lib-filename (make-pathname :name lib-soname
                                        :defaults output-defaults))))
      (multiple-value-bind (c-file lisp-forms)
          (generate-c-lib-file input-file output-defaults)
        (cc-compile-and-link c-file lib-file :library t)
        ;; FIXME: hardcoded library path.
        (values (generate-bindings-file lib-file lib-soname lisp-forms output-defaults)
                lib-file)))))


#|
Have a hierarchy of rules.
1- matching constants
2- matching a cons -> bind, matcher for the head, if yes, matcher for the rest
...

Meh, as a first approximation, have a table for things with rules,
(the same as the build table?)
and another table for actual nodes of the computation graph.

|#

#|
NEW PLAN FOR LANGUAGES VS CLOS OBJECTS.
sexp advantages:
* less infrastructure to debug
* more infrastructure to use
object advantages:
* CLOS objects make it easier to cache information instead of recomputing it
  (hash, paths, etc.)

(normalize env '(:lisp "foo"))
=>
(make-instance 'lisp-grain ...)


* Data structures
  * a hash-table of stuff with a provided hashing function,
    built on top of hash-tables for integers (or some basic type),
    manually managing buckets?
  * a balanced sorted tree? - the default thing for everything(!)
  * a trie?


* in a VPN, cache the pathname, namestring (?)

1- a rule store, prioritized by specificity, indexed by whatever works.

So...
1- 


|#


(defgeneric computation-category (computation))
(declaim (ftype (function (computation) category) computation-category))

(defgeneric computation-source (computation))
(declaim (ftype (function (computation) object) computation-source))
(defgeneric computation-target (computation))
(declaim (ftype (function (computation) object) computation-target))
(defgeneric computation-label (computation))
(declaim (ftype (function (computation) label) computation-label))



