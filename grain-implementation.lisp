;;;;; Syntax and Semantics of Lisp grains, including build.xcvb files
#+xcvb (module (:depends-on ("grain-registry" "extract-target-properties")))
(in-package :xcvb)

(declaim (optimize (speed 2) (safety 3) (debug 3) (compilation-speed 0)))

(defun create-module-from-declaration (form &key keys build-p)
  "Takes a module declaration FORM and returns a grain object for that module."
  (apply #'make-instance (parse-module-declaration form :keys keys :build-p build-p)))

(defun parse-module-declaration (form &key keys build-p)
  "Takes a module declaration FORM and returns a grain object for that module."
  (let ((class (module-form-p form))
        (pathname (getf keys :pathname)))
    (unless class
      (error "Invalid or missing module declaration~@[ in ~S~]" pathname))
    (when build-p
      (unless (eq class 'lisp-file-grain)
        (error "Invalid build module declaration~@[ in ~S~]" pathname))
      (setf class 'build-module-grain))
    (destructuring-bind ((&rest form-keys &key &allow-other-keys) &rest extension-forms)
        (cdr form)
      (loop :for (key nil rest-keys) :on form-keys :by #'cddr :do
        (cond
          ((getf keys key)
           (error "While parsing module form ~S~@[ in ~S~], invalid key ~S provided"
                  form pathname key))
          ((getf keys rest-keys)
           (error "While parsing module form ~S~@[ in ~S~], duplicate key ~S provided"
                  form pathname key))))
      (log-format-pp 10
		     "      Constructing grain of class ~A with~%        ~S~%" class
		  `(:extension-forms ,extension-forms :computation nil
				     ,(append keys form-keys)))
      (list* class
             :extension-forms extension-forms
             :computation nil
             (append keys form-keys)))))

(defun read-module-declaration (path)
  (let ((*features* (list :xcvb)))
    (read-first-file-form path :package :xcvb-user)))

(defun grain-from-file-declaration (path &key build-p)
  (log-format 10 "    Creating grain from declarations in file at ~S~%" path)
  (create-module-from-declaration
   (read-module-declaration path)
   :keys `(:pathname ,path) :build-p build-p))

(defun module-form-p (form)
  "Returns whether or not the given form is a valid xcvb:module form"
  (and (consp form)
       (listp (cdr form))
       (listp (cadr form))
       (cdr (assoc (car form) *module-classes*))))

(defmethod shared-initialize :after
    ((grain lisp-module-grain) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (compute-fullname grain)
  (validate-fullname grain)
  (values))

(defmethod shared-initialize :after
    ((grain loadable-file-grain) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (validate-fullname grain)
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
  grain)

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
	      (append (unless (member (effective-encoding grain) '(:utf-8 nil))
			(normalize '((:build "/asdf-encodings"))))
		      (if (slot-boundp grain 'build-depends-on)
			  (normalize build-depends-on)
			(build-dependencies (grain-parent grain)))))))))

(defun normalize-supersedes-asdf (n x)
  (etypecase x
    (string
     (list (coerce-asdf-system-name x) n))
    ((cons string (cons string null))
     (portable-pathname-from-string (second x) :allow-absolute nil) ; check validity
     (list (coerce-asdf-system-name (first x)) (strcat n "/" (second x))))))

(defmethod finalize-grain :after ((grain build-module-grain))
  (with-slots (supersedes-asdf asdf-supersessions) grain
    (setf asdf-supersessions
          (mapcar/ #'normalize-supersedes-asdf (fullname grain) supersedes-asdf))))

;; Lisp grain extension form for generating Lisp files.

(define-simple-dispatcher handle-extension-form #'handle-extension-form-atom)

(defun handle-extension-form (grain extension-form)
  (handle-extension-form-dispatcher grain extension-form))

(defun handle-extension-form-atom (grain extension-form)
  (declare (ignore grain))
  (error "handle-extension-form-atom: Extension form ~a is invalid.
Only currently support :generate and :executable extension form."
	 extension-form))

(defmethod print-object ((g lisp-generator) stream)
  (with-output (stream)
    (print-unreadable-object (g stream :type t)
      (format stream ":build ~A :targets ~A :dependencies ~A"
              (fullname (generator-build g))
              (mapcar #'fullname (generator-targets g))
              (generator-dependencies g)))))

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
		      (create-module-from-declaration
                       `(module ,keys)
                       :keys `(:fullname (:lisp ,(strcat (fullname build) "/" name))
                               :parent ,build
                               ;;; TODO: use :obj rather than :src, after mapping file is defined
                               :vp ,(make-vp :src (fullname build) "/" name "." "lisp")))))
		  generate))
	 (generator
	  (make-instance 'lisp-generator
            :build build
	    :targets targets
	    :dependencies (normalize-dependencies build depends-on :depends-on))))
    (loop :for target :in targets :do
      (setf (registered-grain (fullname target)) target)
      (setf (grain-generator target) generator))))

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
    (make-computation
     env
     :outputs targets
     :inputs (traversed-dependencies env)
     :command
     `(:xcvb-driver-command
       ,(image-setup env)
       ,@(traversed-build-commands env)))))

(define-handle-extension-form :executable (build name &key depends-on pre-image-dump post-image-restart entry-point)
  (let* ((target (make-instance 'executable-grain
		   :parent build
		   :fullname `(:executable ,(strcat (fullname build) "/" name))))
	 (generator
	  (make-instance 'executable-generator
	    :build build
            :target target
	    :pre-image-dump pre-image-dump
	    :post-image-restart post-image-restart
	    :entry-point entry-point
	    :depends-on depends-on)))
    (setf (registered-grain (fullname target)) target)
    (setf (grain-generator target) generator))
  (values))

(defmethod run-generator (env (generator executable-generator))
  (let* ((build (generator-build generator))
         (target (generator-target generator))
	 (fullname (fullname target))
	 (name (progn
		 (assert (single-arg-form-p :executable fullname))
		 (second fullname)))
         (depends-on (generator-depends-on generator))
         (dependencies
          (if (eq depends-on :build)
              (load-dependencies build)
              (normalize-dependencies build depends-on :depends-on))))
    (graph-for-image-grain
     env name (build-pre-image-name build) dependencies
     :executable t
     :pre-image-dump (pre-image-dump generator)
     :post-image-restart (post-image-restart generator)
     :entry-point (entry-point generator))))

;;(define-handle-extension-form :in-package (grain files &key package) ...)

(defun handle-extension-forms (grain)
  (let ((extension-forms (grain-extension-forms grain)))
    (dolist (extension extension-forms)
      (handle-extension-form grain extension))))

(defun make-grain-from-file (path &key build-p)
  "Takes a PATH to a lisp file, and returns the corresponding grain."
  (grain-from-file-declaration path :build-p build-p))


(defmethod build-module-grain-for ((grain build-module-grain))
  grain)
(defmethod build-module-grain-for ((grain executable-grain))
  (grain-parent grain))
(defmethod build-module-grain-for ((grain lisp-file-grain))
  (grain-parent grain))
(defmethod build-module-grain-for ((grain source-grain))
  (registered-build (source-grain-in grain)))

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
  (setf name (string name))
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

(defmethod build-dependencies ((grain loadable-file-grain))
  (declare (ignorable grain))
  nil)

(defun lisp-module-grain-p (x)
  (typep x 'lisp-module-grain))

(defun build-module-grain-p (x)
  (typep x 'build-module-grain))

(defun file-grain-p (x)
  (typep x 'file-grain))

(defun lisp-file-grain-p (x)
  (typep x 'lisp-file-grain))

(defun asdf-grain-p (x)
  (typep x 'asdf-grain))

(defun require-grain-p (x)
  (typep x 'require-grain))

(defun image-grain-p (x)
  (typep x 'image-grain))

(defun executable-grain-p (x)
  (typep x 'executable-grain))

(defun world-grain-p (x)
  (typep x 'world-grain))

(defmethod print-object ((g file-grain) stream)
  (with-output (stream)
    (print-unreadable-object (g stream :type t)
      (format stream "~@<~S~@[ ~S~]~>"
              (grain-vp g) (when (slot-boundp g 'pathname) (grain-pathname g))))))

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
(defmethod (setf included-dependencies) (grain-set (image image-grain))
  (setf (included-dependencies (image-world image)) grain-set))
(defmethod image-setup ((image image-grain))
  (image-setup (image-world image)))
(defmethod image-setup ((world world-grain))
  (getf (cdr (fullname world)) :setup))
(defmethod build-commands-r ((world world-grain))
  (getf (cdr (fullname world)) :commands-r))

(defgeneric all-build-commands-r (env grain))
(defmethod all-build-commands-r (env (image image-grain))
  (all-build-commands-r env (image-world image)))
(defmethod all-build-commands-r (env (world world-grain))
  (destructuring-bind (&key image load) (image-setup world)
    (remove-duplicates
     (append
      (build-commands-r world)
      (when load
        (loop :for l :in (reverse load)
          :for dep = (tweak-dependency env l)
          ;;:do (grain-for env dep)
          :collect `(:load-file ,dep)))
      (when image
        (all-build-commands-r env (registered-grain image))))
     :test 'equal)))

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

(defgeneric default-vp-for (x))
(defmethod default-vp-for (grain)
  (default-vp-for-fullname nil (fullname grain)))

(defmethod default-vp-for ((x lisp-file-grain))
  (let* ((build (grain-parent x))
         (bname (fullname build))
         (bpath (grain-pathname build))
         (lname (second (fullname x)))
         (pathname (grain-pathname x))
         (suffix (progn
                   (assert (string-prefix-p (strcat bname "/") lname))
                   (subseq lname (1+ (length bname))))))
    (assert (equal pathname (subpathname bpath (strcat suffix ".lisp"))))
    (make-vp :src bname "/" suffix "." "lisp")))

(define-simple-dispatcher default-vp-for-fullname #'default-vp-for-fullname-atom)

(defun default-vp-for-fullname (env name)
  (default-vp-for-fullname-dispatcher env name))

(defun default-vp-for-fullname-atom (env name)
  (declare (ignore env))
  (assert (registered-build name))
  (make-vp :src name "/build.xcvb"))

(defun vp-for-name-extension (name extension &optional (zone :obj))
  (make-vp zone name  "." extension))

(define-default-vp-for-fullname :image (env name)
  (declare (ignore env))
  (vp-for-name-extension name "image"))
(define-default-vp-for-fullname :executable (env name)
  (declare (ignore env))
  #+os-unix (make-vp :obj name) ;; TODO: create a zone :install for end products?
  #+os-windows (vp-for-name-extension name "exe"))
(define-default-vp-for-fullname :static-library (env name)
  (declare (ignore env))
  (vp-for-name-extension name "a"))
(define-default-vp-for-fullname :dynamic-library (env name)
  (declare (ignore env))
  #+os-unix (vp-for-name-extension name "so") ; or should we use "fas" at the risk of clashes?
  #+os-windows (vp-for-name-extension name "dll"))
(define-default-vp-for-fullname :manifest (env name)
  (declare (ignore env))
  (vp-for-name-extension name "manifest"))
(define-default-vp-for-fullname :fasl (env name)
  (declare (ignore env))
  ;; Note: at least ecl and lispworks recognize files based on the type,
  ;; which at least on lispworks varies depending on the target platform.
  (vp-for-name-extension name *fasl-type*))
(define-default-vp-for-fullname :cfasl (env name)
  (declare (ignore env))
  (vp-for-name-extension name (strcat "c" *fasl-type*)))
(define-default-vp-for-fullname :lisp-object (env name)
  (declare (ignore env))
  (vp-for-name-extension name "o"))
(define-default-vp-for-fullname :source (env sub &key in)
  (declare (ignore env))
  (assert (equal in (fullname (registered-build in :ensure-build t))))
  (make-vp :src in "/" sub))

(defmethod effective-around-compile ((lisp lisp-file-grain))
  (if (slot-boundp lisp 'around-compile)
      (around-compile lisp)
      (let ((build (build-module-grain-for lisp)))
        (if (slot-boundp build 'around-compile)
            (around-compile build)
            nil))))

(defmethod effective-encoding ((lisp lisp-module-grain))
  (or (specified-encoding lisp)
      (specified-encoding (build-module-grain-for lisp))
      #|:utf-8|# ;; default
      ))

(defmethod fullname ((grain asdf-grain))
  `(:asdf ,(asdf-grain-system-name grain)))
