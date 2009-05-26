(in-package :xcvb)

;;; Recognizer for current trivial dependency language

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

(defun fasl-grains-for-name (fullname load-dependencies compile-dependencies)
  (cons (make-grain 'fasl-grain
                    :fullname `(:fasl ,fullname)
                    :load-dependencies load-dependencies)
        (if *use-cfasls*
            (list (make-grain 'cfasl-grain
                              :fullname `(:cfasl ,fullname)
                              :load-dependencies compile-dependencies))
            nil)))

(defun cfasl-for-fasl (fasl-grain)
  (check-type fasl-grain fasl-grain)
  (if *use-cfasls*
    (second (computation-outputs (grain-computation fasl-grain)))
    fasl-grain))

(define-simple-dispatcher load-command-for #'load-command-for-atom)

(defun load-command-for-atom (env spec)
  (declare (ignore env))
  (error "Invalid dependency ~S" spec))

(defun load-command-for (env spec)
  (load-command-for-dispatcher env spec))

(define-load-command-for :lisp (env name)
  (simple-load-command-for
   env `(:load-file ,name) name))
(define-load-command-for :fasl (env name)
  (simple-load-command-for
   env `(:load-file (:fasl ,name)) `(:fasl ,name)))
(define-load-command-for :cfasl (env name)
  (simple-load-command-for
   env `(:load-file (:cfasl ,name)) `(:cfasl ,name)))
(define-load-command-for :asdf (env name)
  (simple-load-command-for
   env `(:load-asdf ,name) `(:asdf ,name)))

(defun call-with-dependency-grain (environment dep fun)
  (let* ((grain (graph-for environment dep)))
    (with-dependency (:type type) dep
      (unless (typep grain type)
        (error "Expected a grain of type ~S for ~S, instead got ~S"
               type dep grain))
      (funcall fun grain))))

;;; TODO: We probably need a better interface, so that
;;; the following aspects be handled in a generic way
;;; * the fact that we don't want to load the same dependency twice
;;; * the fact that we may want to "upgrade" some cfasl's to fasl's
;;;   -- or not, because that gets tricky since we want to preserve
;;;   the order of loads, and trickier still if for whatever reason
;;;   the dependencies of the fasl are not an upgrade from the
;;;   dependencies of the cfasl
;;;   -- that condition may be tested and an error issued otherwise.
(defgeneric issue-dependency (env grain))
(defgeneric issue-load-command (env command))

(defun simple-load-command-for (env command fullname)
  (call-with-dependency-grain
   env fullname
   (lambda (grain)
     (load-commands-for-dependencies env grain)
     (issue-dependency env grain)
     (issue-load-command env command))))

(define-load-command-for :build (env name)
  (call-with-dependency-grain
   env `(:build ,name)
   (lambda (grain)
     (load-commands-for-dependencies env grain))))

(defun load-commands-for-dependencies (env grain)
  (dolist (dep (load-dependencies grain))
    (load-command-for env dep)))

#|
In more complex cases, we probably want to
(1) output to a stream of commands, while
(2) sending fullnames for grains, and getting back grains that have been
 already output to a duplicates-removed, type-checked stream of grains.
|#
