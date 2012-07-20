#+xcvb (module (:depends-on ("grain-interface" "dependencies-interpreter")))

(in-package :xcvb)

(declaim (optimize (speed 2) (safety 3) (debug 3) (compilation-speed 0)))

(defclass enforcing-traversal (traversal)
  ())

(defclass static-traversal (enforcing-traversal)
  ((included-dependencies
    :initform (make-hashset :test 'equal)
    :accessor included-dependencies
    :documentation "dependencies included in the current world, as a set")
   (linking-traversal-p
    :initform nil
    :accessor linking-traversal-p
    :documentation "is this traversal for linking")
   (issued-build-commands
    :initform (make-hashset :test 'equal)
    :accessor issued-build-commands
    :documentation "load commands issued so far to build the current world, as a set")
   (build-commands-r
    :initform nil
    :accessor traversed-build-commands-r
    :documentation "load commands issued so far to build the current world, in reverse order")))

(defmethod print-object ((x static-traversal) stream)
  (print-unreadable-object (x stream :type t :identity nil)
    (format stream ":target ~S :depth ~A :setup ~S :build-commands ~S :dependencies ~S :linking ~A"
            (first (traversed-grain-names-r x))
            (length (traversed-grain-names-r x))
            (if (slot-boundp x 'image-setup) (image-setup x) :no-image-setup)
            (reverse (traversed-build-commands-r x))
            (mapcar 'fullname (reverse (traversed-dependencies-r x)))
            (linking-traversal-p x))))

(defmethod tweak-dependency ((env static-traversal) dep)
  (if (linking-traversal-p env)
      (linkable-dependency dep)
      dep))

(defun tweak-dependencies (env deps)
  (mapcar/ #'tweak-dependency env deps))

(defmethod dependency-already-included-p ((env static-traversal) grain)
  (or (gethash grain (included-dependencies env))
      (call-next-method)))

(defmethod issue-build-command ((env static-traversal) command)
  (unless (gethash command (issued-build-commands env))
    (setf (gethash command (issued-build-commands env)) t)
    (push command (traversed-build-commands-r env))))

(defun graph-for-compiled (env spec)
  (graph-for env (compiled-dependency spec)))

(defmethod graph-for-atom (env name)
  (graph-for env `(:lisp ,name)))

(defun include-image-dependencies (env image)
  (when image
    (check-type image image-grain)
    (when (linking-traversal-p env)
      (assert (null (traversed-build-commands-r env)))
      (setf (traversed-build-commands-r env)
            (all-build-commands-r env image)))
    (setf (included-dependencies env)
          (make-hashset :test 'equal :set (included-dependencies image)))))

(define-graph-for :lisp ((env static-traversal) name)
  (let* ((grain (resolve-absolute-module-name name)))
    (unless (typep grain 'lisp-file-grain)
      (error "Couldn't resolve ~S to a lisp module" name))
    grain))

(define-graph-for :build (env name)
  (graph-for-build-named env name))

(define-graph-for :compile-build (env name)
  (graph-for-build-named env name))

(defmethod graph-for-build-named (env name)
  (graph-for-build-module-grain env (registered-build name :ensure-build t)))

(defmethod graph-for-build-module-grain :before (env (grain build-module-grain))
  (declare (ignore env))
  (finalize-grain grain))

(defmethod graph-for-build-module-grain ((env enforcing-traversal) (build build-module-grain))
  (cond
    ((target-ecl-p)
     (graph-for env `(:dynamic-library ,(fullname build))))
    ((and *target-can-dump-image-p*
	  (let ((post-image-name (build-image-name build)))
	    (and post-image-name
		 (graph-for env `(:image ,post-image-name))))))
    (t
     (make-phony-grain
      :name `(:build ,(fullname build))
      :dependencies
      (progn
	(build-command-for* env (load-dependencies build))
	(traversed-dependencies env))))))

(define-graph-for :dynamic-library ((env enforcing-traversal) name)
  (assert (target-ecl-p))
  (second (graph-for-build-libraries env name)))

(define-graph-for :static-library ((env enforcing-traversal) name)
  (assert (target-ecl-p))
  (first (graph-for-build-libraries env name)))

(defmethod graph-for-build-libraries ((env static-traversal) name)
  (check-type name string)
  (assert (target-ecl-p))
  (setf (linking-traversal-p env) t)
  ;; We want to compute the *difference* between the build-commands-for
  ;; the build dependencies of the library and build-commands-for the library and its dependencies.
  ;; i.e. what does the build include that's new?
  ;; then we package that into a nice static library (for linking) and dynamic library (for loading)
  (let* ((build (registered-build name :ensure-build t))
	 (build-dependencies
	  (progn
	    (pre-image-for env build)
	    (build-dependencies build)))
	 (traversed
	  (progn
	    (build-command-for* env build-dependencies)
	    (setf (traversed-build-commands-r env) nil) ;; but keep issued-build-commands as it is!
	    (build-command-for* env (load-dependencies build))
	    (traversed-dependencies env)))
	 (image-setup (image-setup env))
	 (build-commands (traversed-build-commands env)) ;; only the new ones!
	 (issued-build-commands (issued-build-commands env)) ;; including the old ones!
	 (included-dependencies (included-dependencies env))) ;; including the old ones!
    (flet ((make-library (class keyword)
	     (make-grain
	      class :fullname `(,keyword ,name)
	      :load-dependencies build-dependencies
	      :issued-build-commands issued-build-commands
	      :included-dependencies included-dependencies)))
      (let* ((static-library-grain
	      (make-library 'static-library-grain :static-library))
	     (dynamic-library-grain
	      (make-library 'dynamic-library-grain :dynamic-library))
	     (grains (list static-library-grain dynamic-library-grain)))
	(make-computation env
	  :outputs grains
	  :inputs traversed
	  :command
	  `(:xcvb-driver-command
	    ,image-setup
	    (:create-bundle
	     (:bundle (:static-library ,name) :kind :static-library)
	     ,@build-commands)
	    (:create-bundle
	     (:bundle (:dynamic-library ,name) :kind :shared-library)
	     ,@build-commands)))
	grains))))

(define-graph-for :image ((env static-traversal) name)
  (cond
    ((null name) ; special: no image
     nil)
    ((equal name "/_") ;; special: initial image
     (graph-for-image-grain env name nil nil))
    ((string-prefix-p "/_pre/" name)
     (let* ((build-name (subseq name 5))
            (build (registered-build build-name :ensure-build t)))
       (finalize-grain build)
       (let* ((dependencies (build-dependencies build))
              (starting-build-name (build-starting-dependencies-p dependencies))
              (starting-build-image-name
               (when starting-build-name
                 (build-post-image-name
                  (registered-build starting-build-name :ensure-build t)))))
         (graph-for-image-grain env name (or starting-build-image-name "/_") dependencies))))
    (t
     (let* ((build (registered-build name :ensure-build t)))
       (graph-for-image-grain
        env name (build-pre-image-name build) (load-dependencies build))))))

(define-graph-for :executable ((env static-traversal) name)
  (registered-grain `(:executable ,name)))

(defun pre-image-for (env grain)
  (issue-image-named env (build-pre-image-name grain)))

(defun issue-image-named (env name)
  (if name
    (let ((image (graph-for env `(:image ,name))))
      (issue-dependency env image)
      (include-image-dependencies env image)
      (setf (image-setup env) `(:image ,(fullname image)))
            image)
    (progn
      ;; TODO: issue dependency for
      ;; lisp executable/wrapper (in $PATH),
      ;; actual executable/driver (self/exe),
      ;; base image/core
      ;; if one is not found (or even if it is?),
      ;; use lisp-implementation-version and *features* as a proxy.
      (when (linking-traversal-p env)
        (assert (null (traversed-build-commands-r env)))
        (loop :for dep :in *lisp-setup-dependencies* :do
          (build-command-for env dep)))
      (setf (image-setup env)
            `(:load ,(loop
                       :for dep :in *lisp-setup-dependencies*
                       :for grain = (graph-for env dep)
                       :do (issue-dependency env grain)
                       :collect (fullname grain))))
      nil)))

(defun make-load-file-command (fullname)
  `(:load-file ,fullname))
(defun unwrap-load-file-command (x)
  (when (single-arg-form-p :load-file x)
    (second x)))
(defun remove-load-file (x)
  (or (unwrap-load-file-command x)
      (error "cannot remove :load-file from ~S" x)))

(defun require-command-p (x)
  (and (list-of-length-p 2 x) (eq (first x) :require)))


;;; TODO: have an actual grain for the manifest!
;;; TODO: have a better language for describing computations!
(defun manifest-and-build-commands (name image-setup build-commands)
  (cond
    ((and *use-master* (target-ecl-p))
     (values
      `((:make-manifest
         (:manifest ,name)
         ,@build-commands))
      `((:load-manifest (:manifest ,name)))))
    (*use-master*
     (let ((initial-loads (getf image-setup :load))
           (initial-name (strcat name "__initial")))
       (values
        (append
         (when initial-loads
           `((:make-manifest
              (:manifest ,initial-name)
              ,@(mapcar 'make-load-file-command initial-loads))))
         (when build-commands
           `((:make-manifest
              (:manifest ,name)
              ,@build-commands))))
        (append
         (when initial-loads
           `((:initialize-manifest (:manifest ,initial-name))))
         (when build-commands
           `((:load-manifest (:manifest ,name))))))))
    (t
     (values nil build-commands))))

(defmethod graph-for-image-grain ((env static-traversal) name pre-image-name dependencies
				  &key executable pre-image-dump post-image-restart entry-point)
  (declare (optimize (debug 3) (safety 3)))
  (setf (linking-traversal-p env) (target-ecl-p))
  (let ((pre-image (issue-image-named env pre-image-name)))
    (build-command-for* env dependencies)
    (let* ((traversed (traversed-dependencies env))
           (image-setup (image-setup env))
           (build-commands-r (traversed-build-commands-r env))
           (build-commands (reverse build-commands-r))
           (manifest-and-build-commands
            (multiple-value-list
             (manifest-and-build-commands name image-setup build-commands)))
           (manifest-maker (first manifest-and-build-commands))
           (build-commands-spec (second manifest-and-build-commands))
           (world (make-instance
                   'world-grain
                   :fullname (make-world-name image-setup build-commands-r)
                   :issued-build-commands
                   (make-hashset :test 'equal :list build-commands-r)
                   :included-dependencies
                   (make-hashset :test 'equal
                                 :set (when pre-image (included-dependencies pre-image))
                                 :list traversed)))
           (grain
	    (if executable
		(registered-grain `(:executable ,name))
		(make-grain 'image-grain
			    :fullname `(:image ,name)
			    :world world)))
	   (fullname (fullname grain)))
      (make-computation env
	:outputs (list grain)
	:inputs traversed
	:command
	`(:progn
	   ,@manifest-maker
	   (:xcvb-driver-command
	    ,image-setup
	    (:create-image
	     (:image ,fullname
	      ,@(when executable
		  `(:executable t :pre-image-dump ,pre-image-dump
		    :post-image-restart ,post-image-restart
		    :entry-point ,entry-point)))
	     ,@build-commands-spec))))
      grain)))

(define-graph-for :source (env name &key in)
  (declare (ignore env))
  (make-source-grain :name name :in in))

(defun make-source-grain (&key name in)
  (make-instance
   'source-grain
   :computation nil
   :name name
   :in in
   :fullname `(:source ,name :in ,in)))

(define-graph-for :fasl ((env enforcing-traversal) lisp-name)
  (first (graph-for-fasls env lisp-name)))

(define-graph-for :cfasl ((env enforcing-traversal) lisp-name)
  (assert *use-cfasls*)
  (second (graph-for-fasls env lisp-name)))

(define-graph-for :lisp-object ((env enforcing-traversal) lisp-name)
  (assert (target-ecl-p))
  (second (graph-for-fasls env lisp-name)))

(defun setup-dependencies-before-fasl (fullname)
  (assert (equal '(:fasl "/xcvb/driver") (car *lisp-setup-dependencies*)))
  (reverse ; put back in order
   (cdr ; skip the current dependency itself
    (member `(:fasl ,(second fullname)) ; what is up to the current dependency
            (reverse *lisp-setup-dependencies*)
            :test #'equal))))

(define-graph-for :asdf ((env static-traversal) system-name)
  (declare (ignorable env))
  (let* ((phony (make-instance 'phony-grain
                               :fullname `(:build-asdf ,system-name))))
    (issue-image-named env nil)
    (build-command-for env '(:build "/asdf"))
    (make-computation
     env :outputs (list phony) :inputs (traversed-dependencies env) :command
     `(:xcvb-driver-command ,(image-setup env) (:initialize-asdf) (:load-asdf ,system-name)))
    (call-next-method)))

(defmethod make-computation ((env static-traversal) &rest keys &key &allow-other-keys)
  (apply #'make-computation () keys))

(defmethod graph-for-fasls ((env static-traversal) fullname)
  (check-type fullname string)
  (let* ((lisp (graph-for env fullname))
         (fullname (fullname lisp)) ;; canonicalize the fullname
         (driverp (equal fullname '(:lisp "/xcvb/driver")))
         (specialp (member `(:fasl ,(second fullname)) *lisp-setup-dependencies* :test #'equal)))
    (check-type lisp lisp-file-grain)
    (finalize-grain lisp)
    (let ((build-dependencies (if specialp
                                  (setup-dependencies-before-fasl fullname)
                                  (build-dependencies lisp)))
          (compile-dependencies (compile-dependencies lisp))
          (cload-dependencies (cload-dependencies lisp))
          (load-dependencies (load-dependencies lisp))
          (around-compile (effective-around-compile lisp))
          (encoding (effective-encoding lisp)))
      (issue-dependency env lisp)
      (unless specialp
        (pre-image-for env lisp))
      (build-command-for* env build-dependencies)
      (build-command-for* env compile-dependencies)
      (let* ((outputs (fasl-grains-for-name
                       env fullname
                       load-dependencies cload-dependencies
                       build-dependencies))
             (cfasl (when *use-cfasls* (fullname (second outputs))))
             (lisp-object (when (target-ecl-p) (fullname (second outputs)))))
        (make-computation
         env
         :outputs outputs
         :inputs (traversed-dependencies env)
         :command
         (if driverp
           `(:compile-file-directly ,fullname :cfasl ,cfasl :lisp-object ,lisp-object)
           `(:xcvb-driver-command
             ,(if specialp `(:load '(:fasl "/xcvb/driver"))
                  (image-setup env))
             (:compile-lisp
              (,fullname
               ,@(when around-compile `(:around-compile ,around-compile))
               ,@(when encoding `(:encoding ,encoding)))
              ,@(traversed-build-commands env)))))
        outputs))))
