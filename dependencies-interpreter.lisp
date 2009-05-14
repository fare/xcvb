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

(defun normalize-dependency (dep build)
  (cond
    ((stringp dep)
     (resolve-module-name dep build))
    ((and (list-of-length-p 2 dep)
          (member (first dep) '(:lisp :compile))
          (stringp (second dep)))
     (list (first dep) (resolve-module-name (second dep) build)))
    ((and (list-of-length-p 2 dep)
          (eq (first dep) ':asdf))
     (list :asdf (coerce-asdf-system-name (second dep))))
    (t
     ;; Other forms are not recognized
     (error "unrecognized dependency ~S" dep))))


;; Q: What is the best place / time to do that?
;; Note: the parents must already be setup!
(defun normalize-grain-dependencies! (grain)
  (check-type grain lisp-grain)
  (let ((build (or (grain-parent grain) grain)))
    (flet ((normalize (dep)
             (normalize-dependency dep build)))
      (with-slots (depends-on load-depends-on compile-depends-on) grain
        (setf depends-on (mapcar #'normalize depends-on)
              load-depends-on (mapcar #'normalize load-depends-on)
              compile-depends-on (mapcar #'normalize compile-depends-on))))))

;;; Go from a load-time dependency to the corresponding compile-time dependency

(defun compiled-dependency (dep)
  (cond
    ((stringp dep)
     ;; Lisp modules are to be compiled
     (list :compile dep))
    ((and (list-of-length-p 2 dep)
          (member (first dep) '(:lisp :compiled :asdf))
          ;; (stringp (second dep)) ;--- should we assume it's been normalized yet?
          )
     ;; verbatim Lisp source, compiled stuff, asdf systems stay same
     dep)
    (t
     ;; Other forms are not recognized
     (error "unrecognized dependency ~S" dep))))


;;; Basics of Lisp compilation
(defun compile-time-fasl-type ()
  (if *use-cfasls* :cfasl :fasl))

(defun ensure-graph-for-grain-compilation (grapher grain)
  (check-type grain lisp-grain)
  (funcall grapher `(:fasl ,(fullname grain))))

(defun cfasl-for-fasl (fasl-grain)
  (check-type fasl-grain fasl-grain)
  (second (computation-outputs (grain-computation fasl-grain))))


;;; What to do to load a given dependency
(define-combinator-interpreter load-command-for #'load-command-for-atom)

(defun load-command-for (environment spec)
  (load-command-for-interpreter environment spec))

(defun load-command-for-atom (grapher atom)
  (let* ((grain (funcall grapher atom)))
    (check-type grain lisp-grain)
    (ensure-graph-for-grain-compilation grapher grain)
    `(:load (:fasl ,(fullname grain)))))

(define-load-command-for :lisp (grapher name)
  (let* ((grain (funcall grapher name)))
    (check-type grain lisp-grain)
    `(:load (:lisp ,(fullname grain)))))

(define-load-command-for :compile (grapher name)
  (let* ((grain (funcall grapher name)))
    (check-type grain lisp-grain)
    (ensure-graph-for-grain-compilation grapher grain)
    `(:load (,(compile-time-fasl-type) ,(fullname grain)))))
