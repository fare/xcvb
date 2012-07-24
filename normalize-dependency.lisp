#+xcvb (module (:depends-on ("names" "grain-interface")))

(in-package :xcvb)

;;; Recognizer for current trivial dependency language

(defvar *asdf-systems-warned* ()
  ;; This is a bit of a kluge, but oh well.
  "the names of ASDF systems for which we have already issued a warning that
a reference to the system was superseded by a build.xcvb file.")

(defun lisp-module-grain-from (name grain)
  (let ((lisp-module-grain (resolve-module-name name grain)))
    (unless (lisp-module-grain-p lisp-module-grain)
      (error "Couldn't resolve ~S to a valid module from grain ~S~@[ pathname ~S~]"
             name (fullname grain) (grain-pathname grain)))
    lisp-module-grain))

(defun lisp-fullname-from (name grain)
  (second (fullname (lisp-module-grain-from name grain))))

(defun unrecognized-dependency (dep)
  (error "unrecognized dependency ~S" dep))

(defun normalize-dependencies (grain deps type)
  (unless (listp deps)
    (error "In module ~S~@[ pathname ~S~], ~S dependencies are not a list but ~S"
           (fullname grain) (grain-pathname grain) type deps))
  (mapcar/ #'normalize-dependency grain deps))

(defun normalize-dependency (grain dep)
  (normalize-dependency-dispatcher grain dep))

(define-simple-dispatcher normalize-dependency #'normalize-dependency-atom)

(defun normalize-dependency-atom (grain name)
  (check-type grain grain)
  (check-type name string)
  (let ((g (resolve-module-name name grain)))
    (etypecase g
      (null (error "~@<Failed to resolve name ~S from grain ~S~@[ pathname ~S~]~:>"
		   name (fullname grain) (grain-pathname grain)))
      (executable-grain `(:executable ,(fullname g)))
      (build-module-grain `(:build ,(fullname g)))
      (lisp-file-grain `(:fasl ,(second (fullname g)))))))

(define-normalize-dependency :when (grain expression &rest dependencies)
  ;; TODO: parse and make sure that expression is well-formed, which
  ;; should issue an error message early if there user-provided code is wrong.
  `(:when ,expression ,@(normalize-dependencies grain dependencies :when)))

(define-normalize-dependency :cond (grain &rest cond-expressions)
  ;; TODO: parse and make sure that expression is well-formed, which
  ;; should issue an error message early if there user-provided code is wrong.
  `(:cond ,@(mapcar (lambda (x) (cons (car x) (normalize-dependencies grain (cdr x) :cond)))
                    cond-expressions)))

(defun normalize-dependency-lisp* (type grain name)
  `(,type ,(lisp-fullname-from name grain)))
(define-normalize-dependency :lisp (grain name)
  (normalize-dependency-lisp* :lisp grain name))
(define-normalize-dependency :fasl (grain name)
  (normalize-dependency-lisp* :fasl grain name))
(define-normalize-dependency :cfasl (grain name)
  (normalize-dependency-lisp* :cfasl grain name))
(define-normalize-dependency :lisp-object (grain name)
  (normalize-dependency-lisp* :lisp-object grain name))

(defun normalize-dependency-build* (type grain name)
  (let ((g (lisp-module-grain-from name grain)))
    (check-type g lisp-module-grain)
    `(,type ,(fullname g))))

(define-normalize-dependency :build (grain name)
  (normalize-dependency-build* :build grain name))
(define-normalize-dependency :compile-build (grain name)
  (normalize-dependency-build* :compile-build grain name))

(define-normalize-dependency :compile (grain name)
  (let ((g (lisp-module-grain-from name grain)))
    (check-type g lisp-module-grain)
    (let ((n (fullname g)))
      (if (build-module-grain-p g)
        `(:compile-build ,n)
        `(,(compile-time-fasl-type) ,n)))))

(define-normalize-dependency :asdf (grain name)
  (declare (ignore grain))
  (let* ((n (coerce-asdf-system-name name))
         (superseding (registered-build `(:supersedes-asdf ,n))))
    (etypecase superseding
      (null
       `(:asdf ,n))
      (build-module-grain
       (finalize-grain superseding)
       (let ((nn (second (assoc n (asdf-supersessions superseding) :test 'equal))))
         (unless (member n *asdf-systems-warned* :test 'equal)
           (push n *asdf-systems-warned*))
         (normalize-dependency-atom superseding nn)))
      (require-grain
       (let ((nn (fullname superseding)))
         (unless (member nn *asdf-systems-warned* :test 'equal)
           (push nn *asdf-systems-warned*))
         nn))
      (invalid-build-registry-entry
       (error "Trying to use ASDF system :~A claimed by invalid build ~S"
              n superseding)))))

(define-normalize-dependency :require (grain name)
  (declare (ignore grain))
  (check-type name (or string symbol))
  `(:require ,(intern (string name) :keyword)))

(define-normalize-dependency :source (grain name &key in)
  "File named relatively to a build"
  (let ((path (portable-pathname-from-string name)))
    (if (absolute-pathname-p path)
        (multiple-value-bind (build suffix)
            (resolve-build-relative-name name)
          (if build
              `(:source ,suffix :in ,(fullname build))
              (error "Couldn't find in a build to which ~S is relative" name)))
        (let ((build (if in
                         (registered-build (canonicalize-fullname in) :ensure-build t)
                         (build-module-grain-for grain))))
        `(:source ,name :in ,(fullname build))))))

(define-normalize-dependency :object (grain name)
  "File named relatively to the object directory"
  `(:object
    ,(portable-namestring
      (merge-pathnames*
       (portable-pathname-from-string name)
       (portable-pathname-from-string (fullname grain))))))

(define-normalize-dependency :file (grain name)
  "File named relatively to the filesystem"
  `(:file ,(namestring
            (ensure-pathname-absolute
             (merge-pathnames*
              name
              (pathname-directory-pathname
               (grain-pathname
                (build-module-grain-for grain))))))))

;;; Matcher for the normalized dependency language
(defparameter +dependency-type+
  '((:lisp . lisp-module-grain)
    (:fasl . fasl-grain)
    (:cfasl . cfasl-grain)
    (:lisp-object . lisp-object-grain)
    (:executable . executable-grain)
    (:dynamic-library . dynamic-library-grain)
    (:static-library . static-library-grain)
    (:image . image-grain)
    (:asdf . asdf-grain)
    (:require . t)
    (:build . t)
    (:compile-build . t)
    (:source . t)
    (:object . t)
    (:file . t))
  "what type for grains corresponding to a given dependency tag")

(defparameter +computing-dependencies+
  '(:when :cond :source :require))

(defun deconstruct-dependency (dep k)
  (flet ((err () (error "malformed dependency ~S" dep)))
    (typecase dep
      (cons
         (let* ((head (first dep))
                (computing (and (member head +computing-dependencies+) t)))
           (if computing
             (funcall k head nil t)
             (progn
               (unless (and (list-of-length-p 2 dep)
                            (stringp (second dep)))
                 (err))
               (let* ((name (second dep))
                      (type (or computing (cdr (assoc head +dependency-type+)))))
                 (unless (or computing type)
                   (err))
                 (funcall k head name type))))))
      (string
         (funcall k :lisp dep 'lisp-file-grain))
      (t
         (err)))))

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
  (compiled-dependency-dispatcher dep))
(define-simple-dispatcher compiled-dependency #'unrecognized-dependency :environment nil)
(define-compiled-dependency :fasl (x)
  (list (compile-time-fasl-type) x))
(define-compiled-dependency :build (x)
  `(:build ,x))
(macrolet ((d (k) `(define-compiled-dependency ,k (&rest r) (cons ,k r)))
           (self-compiled-dependency (&rest r) `(progn ,@(loop :for k :in r :collect `(d ,k)))))
  (self-compiled-dependency :source :lisp :cfasl :lisp-object :asdf :require
			    :compile-build :static-library :dynamic-library :executable))
(define-compiled-dependency :when (c &rest deps)
  `(:when ,c ,@(mapcar #'compiled-dependency deps)))
(define-compiled-dependency :cond (&rest clauses)
  `(:cond ,@(loop :for (c . deps) :in clauses
              :collect (cons c (mapcar #'compiled-dependency deps)))))

(defun linkable-dependency (dep)
  "Go from a load-time dependency to the corresponding compile-time dependency,
in the normalized dependency mini-language"
  (linkable-dependency-dispatcher dep))
(define-simple-dispatcher linkable-dependency #'unrecognized-dependency :environment nil)
(define-linkable-dependency :cfasl (x) `(:lisp-object ,x))
(define-linkable-dependency :fasl (x) `(:lisp-object ,x))
(define-linkable-dependency :lisp-object (x) `(:lisp-object ,x))
(define-linkable-dependency :dynamic-library (x) `(:static-library ,x))
(define-linkable-dependency :compile-build (x) `(:build ,x))
(macrolet ((d (k) `(define-linkable-dependency ,k (&rest r) (cons ,k r)))
           (self-linkable-dependency (&rest r) `(progn ,@(loop :for k :in r :collect `(d ,k)))))
  (self-linkable-dependency :asdf :require :build :static-library))
(define-linkable-dependency :when (c &rest deps)
  `(:when ,c ,@(mapcar #'linkable-dependency deps)))
(define-linkable-dependency :cond (&rest clauses)
  `(:cond ,@(loop :for (c . deps) :in clauses
              :collect (cons c (mapcar #'linkable-dependency deps)))))

(defun compile-time-fasl-type ()
  (if *use-cfasls* :cfasl :fasl))

(defun fasl-grains-for-name (env fullname
                             load-dependencies cload-dependencies build-dependencies)
  (flet ((m (class kw name deps &aux (fullname `(,kw ,(second name))))
           (make-grain
            class
            :fullname fullname
            :vp (default-vp-for-fullname env fullname)
            :load-dependencies deps)))
    `(,(m 'fasl-grain :fasl fullname (append build-dependencies load-dependencies))
      ,@(cond
	 ((target-ecl-p)
	  `(,(m 'lisp-object-grain :lisp-object fullname
			      (append build-dependencies cload-dependencies))))
         (*use-cfasls*
	  `(,(m 'cfasl-grain :cfasl fullname
		(append build-dependencies cload-dependencies))))))))

(defun cfasl-for-fasl (fasl-grain)
  (check-type fasl-grain fasl-grain)
  (if *use-cfasls*
    (second (computation-outputs (grain-computation fasl-grain)))
    fasl-grain))

(defun grain-source (grain)
  (typecase grain
    ((or fasl-grain cfasl-grain lisp-object-grain)
     (registered-grain `(:lisp (second (fullname grain)))))
    (t
     nil)))

(defun fullname-source (fullname)
  (let ((g (grain-source (registered-grain fullname))))
    (when g (fullname g))))
