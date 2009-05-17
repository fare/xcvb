(in-package :xcvb)

;;; Recognizer for current trivial dependency language

(defun coerce-asdf-system-name (name)
  "This function take the name of an asdf-system, and
converts it to a string representation that can universally be used to refer to that system.
Modeled after the asdf function coerce-name"
  (string-downcase
   (typecase name
     #+asdf (asdf:component (asdf:component-name name))
     (symbol (symbol-name name))
     (string name)
     (t (simply-error 'syntax-error "~@<invalid asdf system designator ~A~@:>" name)))))

(defun normalize-dependency (dep grain)
  (flet ((n (x) (let ((grain (resolve-module-name x grain)))
                  (unless (typep grain 'lisp-grain)
                    (error "Couldn't resolve ~S to a valid module from grain ~S"
                           x (fullname grain)))
                  (fullname grain)))
         (err () (error "unrecognized dependency ~S" dep)))
    (cond
      ((stringp dep)
       (list :fasl (n dep)))
      ((and (list-of-length-p 2 dep)
            (stringp (second dep)))
       (let ((f (first dep))
             (x (second dep)))
         (case f
           ((:lisp :fasl :cfasl) (list f (n x)))
           (:compile (list (compile-time-fasl-type) (n x)))
           (:asdf (list :asdf (coerce-asdf-system-name x)))
           (t (err)))))
      (t (err)))))


;;; Matcher for the normalized dependency language
(defvar +dependency-type+
  '((:lisp . lisp-grain)
    (:fasl . fasl-grain)
    (:cfasl . cfasl-grain)
    (:asdf . asdf-grain))
  "what type for grains corresponding to a given dependency tag")

(defun deconstruct-dependency (dep k)
  (flet ((err () (error "malformed dependency ~S" dep)))
    (unless (and (list-of-length-p 2 dep)
                 (stringp (second dep)))
      (err))
    (let* ((head (first dep))
           (name (second dep))
           (type (cdr (assoc head +dependency-type+))))
      (unless type
        (err))
      (funcall k head name type))))

(defmacro with-dependency ((&key head name type) expr &body body)
  (loop :for v :in (list head name type)
        :for var = (or v (gensym))
        :collect var :into vars
        :unless v :collect var :into ignored
        :finally (return
                   `(deconstruct-dependency
                     ,expr
                     (lambda ,vars
                       ,@(when ignored `((declare (ignore ,@ignored))))
                       ,@body)))))

(defun compiled-dependency (dep)
  "Go from a load-time dependency to the corresponding compile-time dependency,
in the normalized dependency mini-language"
  (with-dependency (:head h :name x) dep
    (ecase h
      (:fasl (list (compile-time-fasl-type) x))
      ((:lisp :cfasl :asdf) dep))))

(defun compile-time-fasl-type ()
  (if *use-cfasls* :cfasl :fasl))

(defun fasl-grains-for-name (fullname)
  (cons (make-grain 'fasl-grain :fullname `(:fasl ,fullname))
        (if *use-cfasls*
            (list (make-grain 'cfasl-grain :fullname `(:cfasl ,fullname)))
            nil)))

(defun cfasl-for-fasl (fasl-grain)
  (check-type fasl-grain fasl-grain)
  (if *use-cfasls*
    (second (computation-outputs (grain-computation fasl-grain)))
    fasl-grain))

(defun load-command-for (grapher spec)
  ;; What to do to load a given dependency -
  ;; returns a command in the command language, and a grain.
  (with-dependency (:type type) spec
     (let ((grain (funcall grapher spec)))
       (unless (typep grain type)
         (error "Expected a grain of type ~S for ~S, instead got ~S"
                type spec grain))
       (values `(:load ,spec) grain))))


#|
In more complex cases, we probably want to
(1) output to a stream of commands, while
(2) sending fullnames for grains, and getting back grains that have been
 already output to a duplicates-removed, type-checked stream of grains.
|#
