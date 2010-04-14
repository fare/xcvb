;;;;; Syntax and Semantics of Lisp grains, including build.xcvb files
#+xcvb (module (:depends-on ("registry" "extract-target-properties")))
(in-package :xcvb)

(defun parse-module-declaration (form &key path build-p)
  "Takes a module declaration FORM and returns a grain object for that module."
  (unless (module-form-p form)
    (error "Invalid or missing module declaration~@[ in ~S~]" path))
  (destructuring-bind ((&rest keys &key &allow-other-keys) &rest extension-forms)
      (cdr form)
    (apply #'make-instance (if build-p 'build-module-grain 'lisp-file-grain)
           :pathname path :extension-forms extension-forms
           :computation nil
           keys)))

(defun grain-from-file-declaration (path &key build-p)
  (parse-module-declaration
   (let ((*features* (list :xcvb)))
     (read-first-file-form path :package :xcvb-user))
   :path path :build-p build-p))

(defun module-form-p (form)
  "Returns whether or not the given form is a valid xcvb:module form"
  (and (consp form)
       (eq (car form) 'xcvb:module)
       (listp (cdr form))
       (listp (cadr form))))

(defmethod shared-initialize :after
    ((grain lisp-module-grain) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (compute-fullname grain)
  (let ((fullname (slot-value grain 'fullname)))
    (validate-fullname fullname))
  (values))

(defmethod grain-vp :before ((grain file-grain))
  (unless (slot-boundp grain 'vp)
    (setf (slot-value grain 'vp) (default-vp-for grain))))


;;; Lisp Grains

(defmethod finalize-grain :around (grain)
  (unless (grain-finalized-p grain) ;; only do it once per grain
    (setf (grain-finalized-p grain) :in-progress)
    (call-next-method)
    (setf (grain-finalized-p grain) t))
  (values))

(defmethod finalize-grain ((grain lisp-module-grain))
  (handle-extension-forms grain)
  (macrolet ((normalize (deps)
               `(normalize-dependencies grain ,deps ,(keywordify deps))))
    (with-slots (build-depends-on compile-depends-on load-depends-on
                 cload-depends-on depends-on
                 build-dependencies compile-dependencies cload-dependencies load-dependencies)
        grain
      (let ((common-dependencies (normalize depends-on)))
        (setf compile-dependencies
              (append (mapcar #'compiled-dependency common-dependencies)
                      (normalize compile-depends-on))
              cload-dependencies
              (if (slot-boundp grain 'cload-depends-on)
                  (append (mapcar #'compiled-dependency common-dependencies)
                          (normalize cload-depends-on))
                  compile-dependencies)
              load-dependencies
              (append common-dependencies
                      (normalize load-depends-on))
              build-dependencies
              (if (slot-boundp grain 'build-depends-on)
                  (normalize build-depends-on)
                  (build-dependencies (grain-parent grain))))))))

(defmethod finalize-grain :after ((grain build-module-grain))
  (with-slots (supersedes-asdf) grain
    (setf supersedes-asdf (mapcar #'coerce-asdf-system-name supersedes-asdf))))

;; Lisp grain extension form for generating Lisp files.

(define-simple-dispatcher handle-extension-form #'handle-extension-form-atom)

(defun handle-extension-form (grain extension-form)
  (handle-extension-form-dispatcher grain extension-form))

(defun handle-extension-form-atom (grain extension-form)
  (declare (ignore grain))
  (error "handle-extension-form-atom: Extension form ~a is invalid.  Only currently support :generate extension form."
	 extension-form))

(defmethod print-object ((g lisp-generator) stream)
  (with-output (stream)
    (print-unreadable-object (g stream :type t)
      (format stream ":build ~A :targets ~A :dependencies ~A ~@[:computation ~S~]"
              (fullname (generator-build g))
              (mapcar #'fullname (generator-targets g))
              (generator-dependencies g)
              (when (slot-boundp g 'computation)
                (generator-computation g))))))

(define-handle-extension-form :generate (build generate &key depends-on)
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
						       (grain-pathname build) name))))
		  generate))
	 (generator
	  (make-instance 'lisp-generator
            :build build
	    :targets targets
	    :dependencies (normalize-dependencies build depends-on :depends-on))))
    (loop :for target :in targets :do
      (setf (registered-grain (fullname target)) target)
      (setf (grain-generator target) generator))))

;;(defmethod run-generator (env (fun function)) (funcall fun env))
(defmethod run-generator (env (generator lisp-generator))
  (let* ((dependencies (generator-dependencies generator))
         (targets (generator-targets generator))
         (grain (first targets)))
    (unless targets
      (error "no targets"))
    (unless dependencies
      (error "run-generator: Need dependencies to generate files ~S.~%"
             (mapcar #'fullname targets)))
    (dolist (target targets)
      (slot-makunbound target 'computation))
    (pre-image-for env grain)
    (build-command-for* env dependencies)
    (setf (generator-computation generator)
          (make-computation
           env
           :outputs targets
           :inputs (traversed-dependencies env)
           :command
           `(:xcvb-driver-command
             ,(image-setup env)
             ,@(traversed-build-commands env))))))

;;(define-handle-extension-form :in-package (grain files &key package) ...)


(defun handle-extension-forms (grain)
  (let ((extension-forms (grain-extension-forms grain)))
    (dolist (extension extension-forms)
      (handle-extension-form grain extension))))

(defun make-grain-from-file (path &key build-p)
  "Takes a PATH to a lisp file, and returns the corresponding grain."
  (grain-from-file-declaration path :build-p build-p))

(defun build-module-grain-for (grain)
  (etypecase grain
    (build-module-grain grain)
    (lisp-module-grain (grain-parent grain))))

(defmethod load-dependencies :before ((grain lisp-module-grain))
  (finalize-grain grain))
(defmethod cload-dependencies :before ((grain lisp-module-grain))
  (finalize-grain grain))
(defmethod compile-dependencies :before ((grain lisp-module-grain))
  (finalize-grain grain))
(defmethod build-dependencies :before ((grain lisp-module-grain))
  (finalize-grain grain))

(defun build-starting-dependencies-p (dependencies)
  (and (consp dependencies)
       (consp (car dependencies))
       (eq :build (caar dependencies))
       (cadar dependencies)))

(defun base-image-name ()
  (when (or *use-base-image* (registered-grain `(:image "/_")))
    "/_"))

(defun build-pre-image-name (grain &optional traversed)
  (check-type grain lisp-module-grain)
  (when (member grain traversed)
    (error "Circular build dependency ~S"
           (member grain (reverse traversed))))
  (finalize-grain grain)
  (let* ((dependencies (build-dependencies grain))
         (build-module-grain
          (cond
            ((build-module-grain-p grain) grain)
            ((or (not (slot-boundp grain 'build-depends-on))
                 (equal dependencies
                        (build-dependencies (grain-parent grain))))
             (grain-parent grain))
            (t nil)))
         (pre-image-p (when build-module-grain (build-pre-image build-module-grain)))
         (starting-build-name (build-starting-dependencies-p dependencies))
         (starting-build
          (when starting-build-name
            (registered-build starting-build-name :ensure-build t)))
         (starting-build-image-name
          (when starting-build
            (build-image-name starting-build))))
    (cond
      ((null dependencies)
       (if pre-image-p "/_" (base-image-name)))
      ((and starting-build-image-name (null (cdr dependencies)))
       starting-build-image-name)
      (pre-image-p
       (strcat "/_pre" (fullname build-module-grain)))
      (starting-build-image-name ; and (not pre-image-p)
       starting-build-image-name)
      (starting-build
       (build-pre-image-name starting-build (cons build-module-grain traversed)))
      (t ; (not pre-image-p)
       (base-image-name)))))

(defun build-post-image-name (build-module-grain)
  ;; The closest build on top of which to load files to reach the state post loading the build.
  ;; If the build has an image, that's it. Otherwise, it's its pre-image.
  (check-type build-module-grain build-module-grain)
  (finalize-grain build-module-grain)
  (if (build-image build-module-grain)
      (build-image-name build-module-grain)
      (build-pre-image-name build-module-grain)))

(defun build-image-name (build-module-grain)
  (check-type build-module-grain build-module-grain)
  (let ((image (build-image build-module-grain)))
    (etypecase image
      (null nil)
      ;;(string image)
      ((eql t) (fullname build-module-grain)))))

(defun make-asdf-grain (&key name implementation)
  (make-instance
   'asdf-grain
   :implementation implementation
   :name name
   :fullname `(:asdf ,name)))

(defun make-require-grain (&key name)
  (make-instance
   'require-grain
   :name name
   :fullname `(:require ,name)))

(defmethod build-dependencies ((grain asdf-grain))
  nil)
(defmethod load-dependencies ((grain asdf-grain))
  nil)
(defmethod grain-computation ((grain asdf-grain))
  nil)
(defmethod build-dependencies ((grain require-grain))
  nil)
(defmethod load-dependencies ((grain require-grain))
  nil)
(defmethod grain-computation ((grain require-grain))
  nil)

;;
(defmethod build-dependencies ((grain fasl-grain))
  nil)
(defmethod build-dependencies ((grain cfasl-grain))
  nil)

(defun lisp-module-grain-p (x)
  (typep x 'lisp-module-grain))

(defun build-module-grain-p (x)
  (typep x 'build-module-grain))

(defun lisp-file-grain-p (x)
  (typep x 'lisp-file-grain))

(defun asdf-grain-p (x)
  (typep x 'asdf-grain))

(defun require-grain-p (x)
  (typep x 'require-grain))

(defun image-grain-p (x)
  (typep x 'image-grain))

(defun world-grain-p (x)
  (typep x 'world-grain))

(defun coerce-asdf-system-name (name)
  "This function take the name of an asdf-system, and
converts it to a string representation that can universally be used to refer to that system.
Modeled after the asdf function coerce-name"
  (string-downcase
   (typecase name
     #+asdf (asdf:component (asdf:component-name name))
     (symbol (symbol-name name))
     (string name)
     (asdf-grain (asdf-grain-system-name name))
     (t (simply-error 'syntax-error "~@<invalid asdf system designator ~A~@:>" name)))))

(defmethod print-object ((x grain) stream)
  (if (member (type-of x) *print-concisely*)
    (print-unreadable-object (x stream :type t :identity nil)
      (when (slot-boundp x 'fullname)
        (format stream "~S" (slot-value x 'fullname))))
    (call-next-method)))

(defmethod included-dependencies ((image image-grain))
  (included-dependencies (image-world image)))
(defmethod (setf included-dependencies) (hashset (image image-grain))
  (setf (included-dependencies (image-world image)) hashset))
(defmethod image-setup ((image image-grain))
  (image-setup (image-world image)))
(defmethod build-commands-r ((image image-grain))
  (build-commands-r (image-world image)))
(defmethod image-setup ((world world-grain))
  (getf (cdr (fullname world)) :setup))
(defmethod build-commands-r ((world world-grain))
  (getf (cdr (fullname world)) :commands-r))

(defun canonicalize-image-setup (setup)
  (destructuring-bind (&key image load) setup
    (append
     (when image `(:image ,image))
     (when load `(:load ,load)))))

(defun make-world-name (setup commands-r)
  `(:world :setup ,(canonicalize-image-setup setup)
           :commands-r ,commands-r))

(defun fullname-pathname (fullname)
  (grain-pathname (registered-grain fullname)))

(defgeneric default-file-extension (x))
(defmethod default-file-extension ((x (eql 'lisp-file-grain)))
  "lisp")
(defmethod default-file-extension ((x (eql 'fasl-grain)))
  "fasl")
(defmethod default-file-extension ((x (eql 'cfasl-grain)))
  "cfasl")
(defmethod default-file-extension ((x (eql 'image-grain)))
  "image")


(defgeneric default-vp-for (x))
(defmethod default-vp-for ((x build-module-grain))
  (make-vp :src (fullname x) "/build.xcvb"))
(defmethod default-vp-for ((x lisp-file-grain))
  (let* ((build (grain-parent x))
         (bname (fullname build))
         (bpath (grain-pathname build))
         (fullname (fullname x))
         (pathname (grain-pathname x))
         (suffix (progn
                   (assert (string-prefix-p (strcat bname "/") fullname))
                   (subseq fullname (1+ (length bname))))))
    (assert (equal pathname (subpathname bpath (strcat suffix ".lisp"))))
    (make-vp :src bname "/" suffix "." "lisp")))

(defun vp-for-type-name (type name)
  (make-vp :obj name  "." (string-downcase type)))
(defun default-vp-for-type (type grain)
  (destructuring-bind (i n) (fullname grain)
    (assert (eq i type))
    (vp-for-type-name type n)))
(defmethod default-vp-for ((x image-grain))
  (default-vp-for-type :image x))
(defmethod default-vp-for ((x fasl-grain))
  (default-vp-for-type :fasl x))
(defmethod default-vp-for ((x cfasl-grain))
  (default-vp-for-type :cfasl x))
