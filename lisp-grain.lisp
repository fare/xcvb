;;;;; Syntax and Semantics of Lisp grains, including build.xcvb files
#+xcvb (module (:depends-on ("registry" "specials" "extract-target-properties")))
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
  (get-target-properties)
  *target-system-features*)

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
        (with-slots (build-depends-on build-dependencies supersedes-asdf) grain
          (setf build-dependencies (normalize build-depends-on)
                supersedes-asdf (mapcar #'coerce-asdf-system-name supersedes-asdf))))))
  (values))

;; Lisp grain extension form for generating Lisp files.

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


(define-handle-extension-form :generate (grain generate &key depends-on)
  (unless generate
    (error "Files to be generated not specified."))
  (unless depends-on
    (error "Generators not specified."))
  (let* ((targets
	  (mapcar (lambda (target)
		    (destructuring-bind (type name &rest keys &key &allow-other-keys) target
		      (unless (eq type :lisp)
			(error "Only know how to generate lisp modules."))
		      (parse-module-declaration `(module ,keys)
						:path (module-subpathname
						       (grain-pathname grain) name))))
		  generate))
	 (generator
	  (make-instance 'generator
	    :targets targets
	    :dependencies (normalize-dependencies depends-on grain))))
    (dolist (target targets)
      (slot-makunbound target 'computation)
      (setf (gethash (fullname target) *generators*) generator)
      (setf (gethash (namestring (grain-pathname target)) *pathname-grain-cache*) target))))

;;(define-handle-extension-form :in-package (grain files &key package) ...)


(defun handle-extension-forms (grain)
  (let ((extension-forms (grain-extension-forms grain)))
    (dolist (extension extension-forms)
      (handle-extension-form grain extension))))

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


(defun build-grain-for (grain)
  (etypecase grain
    (build-grain grain)
    (lisp-grain (grain-parent grain))))

(defmethod load-dependencies :before ((grain lisp-grain))
  (handle-lisp-dependencies grain))
(defmethod compile-dependencies :before ((grain lisp-grain))
  (handle-lisp-dependencies grain))
(defmethod build-dependencies :before ((grain lisp-grain))
  (handle-lisp-dependencies grain))

(defmethod build-dependencies ((grain lisp-grain))
  (build-dependencies (grain-parent grain)))

(defun build-starting-dependencies-p (dependencies)
  (and (consp dependencies)
       (consp (car dependencies))
       (eq :build (caar dependencies))
       (cadar dependencies)))

(defun base-image-name ()
  (when (or *use-base-image* (registered-grain `(:image "/_")))
    "/_"))

(defun build-pre-image-name (build-grain &optional traversed)
  (check-type build-grain build-grain)
  (when (member build-grain traversed)
    (error "Circular build dependency ~S"
           (member build-grain (reverse traversed))))
  (handle-lisp-dependencies build-grain)
  (let* ((dependencies (build-dependencies build-grain))
         (pre-image-p (build-pre-image build-grain))
         (starting-build-name (build-starting-dependencies-p dependencies))
         (starting-build
          (when starting-build-name
            (registered-build starting-build-name)))
         (starting-build-image-name
          (when starting-build
            (build-image-name starting-build))))
    (cond
      ((null dependencies)
       (if pre-image-p "/_" (base-image-name)))
      ((and starting-build-image-name (null (cdr dependencies)))
       starting-build-image-name)
      (pre-image-p
       (strcat "/_pre" (fullname build-grain)))
      (starting-build-image-name ; and (not pre-image-p)
       starting-build-image-name)
      (starting-build
       (build-pre-image-name starting-build (cons build-grain traversed)))
      (t ; (not pre-image-p)
       (base-image-name)))))

(defun build-post-image-name (build-grain)
  ;; The closest build on top of which to load files to reach the state post loading the build.
  ;; If the build has an image, that's it. Otherwise, it's its pre-image.
  (check-type build-grain build-grain)
  (handle-lisp-dependencies build-grain)
  (if (build-image build-grain)
      (build-image-name build-grain)
      (build-pre-image-name build-grain)))

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

