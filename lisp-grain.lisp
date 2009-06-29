;;;;; Syntax and Semantics of Lisp grains, including BUILD.lisp files
#+xcvb (module (:depends-on ("registry" "specials")))
(in-package :xcvb)

;;; Module forms

(defmacro module (&rest options)
  ;; Make sure that module declarations don't have an effect when compiled,
  ;; only when read by XCVB.
  (declare (ignore options))
  nil)

(defun parse-module-declaration (form &key path build-p)
  "Takes a module declaration FORM and returns a grain object for that module."
  (unless (module-form-p form)
    (error "Invalid or missing module declaration"))
  (destructuring-bind ((&rest keys) &rest extension-forms) (cdr form)
    (apply #'make-instance (if build-p 'build-grain 'lisp-grain)
           :pathname path :extension-forms extension-forms
           :computation nil
           keys)))

(defun target-system-features ()
  (unless *target-system-features*
    (setf *target-system-features* (compute-target-system-features)))
  *target-system-features*)

(defun compute-target-system-features ()
  (read-first-file-form "obj/target-features.lisp-expr"))

(defun read-first-file-form (filepath)
  "Reads the first form from the top of a file"
  (with-standard-io-syntax ()
    (let ((*package* (find-package :xcvb-user))
	  (*read-eval* nil))
      (with-open-file (in filepath)
        (read in)))))

(defun grain-from-file-declaration (path &key build-p)
  (parse-module-declaration
   (let ((*features* (list :xcvb)))
     (read-first-file-form path))
   :path path :build-p build-p))

(defun module-form-p (form)
  "Returns whether or not the given form is a valid xcvb:module form"
  (and (consp form)
       (eq (car form) 'xcvb:module)
       (listp (cdr form))
       (listp (cadr form))))

(defun generate-form-p (form)
  (eq :generate (car form)))

;;; Lisp Grains

(defgeneric handle-lisp-dependencies (grain))

(defmethod handle-lisp-dependencies ((grain lisp-grain))
  (unless (slot-boundp grain 'load-dependencies) ;; Only do it once
    (handle-extension-forms grain)
    (flet ((normalize (deps)
	     (normalize-dependencies deps grain)))
      (with-slots (compile-depends-on load-depends-on depends-on
                   compile-dependencies load-dependencies) grain
        (let ((common-dependencies (normalize depends-on)))
          (setf compile-dependencies
                (append (mapcar #'compiled-dependency common-dependencies)
                        (normalize compile-depends-on)))
          (setf load-dependencies
                (append common-dependencies
                        (normalize load-depends-on)))))
      (when (build-grain-p grain)
        (with-slots (build-requires build-dependencies) grain
          (setf build-dependencies (normalize build-requires))))))
  (values))

;; Lisp grain extension form for generating Lisp files.

(defclass generate-extension-form ()
  ;; List of Lisp files to be generated.
  ((generate
    :initform nil
    :initarg :generate)
   ;; List of dependencies in order to generate Lisp files.
   (generate-depends-on
    :initform nil
    :initarg :generate-depends-on)))

(define-simple-dispatcher handle-extension-form #'handle-extension-form-atom)

(defun handle-extension-form (grain extension-form)
  (handle-extension-form-dispatcher grain extension-form))

(defun handle-extension-form-atom (grain extension-form)
  (declare (ignore grain))
  (error "handle-extension-form-atom: Extension form ~a is invalid.  Only currently support :generate extension form."
	 extension-form))

(defparameter *generators* (make-hash-table :test 'equal))

(defclass generator (simple-print-object-mixin)
  ((target-grain :initarg :targets :reader generator-targets) ;; generated-grains
   (dependencies :initarg :dependencies :reader generator-dependencies) ;; generated-to-generator
   (computation :initform nil :accessor generator-computation))) ;; generated-computation
  

(define-handle-extension-form :generate (grain generate &key generate-depends-on)
  (unless generate
    (error "Files to be generated not specified."))
  (unless generate-depends-on
    (error "Generators not specified."))
  (let* ((targets
	  (mapcar (lambda (target)
		    (destructuring-bind (type name &rest keys &key &allow-other-keys) target
		      (unless (eq type :gen-lisp)
			(error "Only know how to generate lisp modules."))
		      (parse-module-declaration `(module ,keys)
						:path (module-subpathname
						       (grain-pathname grain) name))))
		  generate))
	 (generator
	  (make-instance 'generator
	    :targets targets
	    :dependencies (normalize-dependencies generate-depends-on grain))))
    (dolist (target targets)
      (slot-makunbound target 'computation)
      (setf (gethash (fullname target) *generators*) generator)
      (setf (gethash (namestring (grain-pathname target)) *pathname-grain-cache*) target))))

(defun handle-extension-forms (grain)
  (let ((extension-forms  (grain-extension-forms grain)))
    (when extension-forms
      (dolist (extension extension-forms)
	(handle-extension-form grain extension)))))

(defun make-grain-from-file (path &key build-p)
  "Takes a PATH to a lisp file, and returns the corresponding grain."
  (let ((grain (grain-from-file-declaration path :build-p build-p)))
    (compute-fullname grain)
    grain))

(defun %grain-from-relative-name (name build)
  (probe-file-grain
   (make-pathname
    :type "lisp"
    :defaults (merge-pathnames
               (portable-pathname-from-string name :allow-absolute nil)
               (grain-pathname build)))))


;;; Ain't this superseded by resolve-module-name???
(defun grain-from-fullname (name)
  (setf name (canonicalize-fullname name))
  (or (registered-grain name)
      (loop :for prev = nil :then pos
            :for pos = (position #\/ name :from-end t :end prev)
            :while pos :do
              (let* ((prefix (subseq name 0 pos))
                     (build (registered-grain prefix)))
                (when (build-grain-p build)
                  (let ((it (%grain-from-relative-name (subseq name (1+ pos)) build)))
                    (when it
                      (setf (registered-grain name) it)
                      (return it)))))
            :finally (return nil))))

(defun build-pre-image-name (build-grain)
  (check-type build-grain build-grain)
  (handle-lisp-dependencies build-grain)
  (let ((dependencies (build-dependencies build-grain)))
    (cond
      ((null dependencies) "/_")
      ((and (consp dependencies)
            (null (cdr dependencies))
            (consp (car dependencies))
            (eq :build (caar dependencies)))
       ;; requiring exactly one other build... make its post image our pre-image
       (cadar dependencies))
      (t (strcat "/_pre" (fullname build-grain))))))

(defun build-image-name (build-grain)
  (check-type build-grain build-grain)
  (let ((image (build-image build-grain)))
    (etypecase image
      (null nil)
      ;;(string image)
      ((eql t) (fullname build-grain)))))

(defun make-asdf-grain (&key name implementation)
  (make-instance
   'asdf-grain
   :implementation implementation
   :name name
   :fullname `(:asdf ,name)))

(defmethod load-dependencies ((grain asdf-grain))
  nil)
(defmethod compile-dependencies ((grain asdf-grain))
  nil)

(defun lisp-grain-p (x)
  (typep x 'lisp-grain))

(defun build-grain-p (x)
  (typep x 'build-grain))

(defun asdf-grain-p (x)
  (typep x 'asdf-grain))

(defun image-grain-p (x)
  (typep x 'image-grain))
