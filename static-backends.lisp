#+xcvb (module (:depends-on
		("specials" "lisp-grain" "dependencies-interpreter" "logging")))
(in-package :xcvb)

(defclass static-traversal (simple-print-object-mixin)
  ((grain-names ;;; grain names in the stack of things we try to create -- to avoid circularities
    :initform nil
    :initarg :grain-names
    :reader traversed-grain-names-r)
   (dependencies-r ;;; dependencies discovered so far while examining the current computation
    :initform nil
    :accessor traversed-dependencies-r)
   (included-dependencies ;;; dependencies included in the current image
    :accessor included-dependencies)
   (load-commands-r ;;; load commands issued so far to run the current compilation.
    :initform nil
    :accessor traversed-load-commands-r)))

(defmethod dependency-already-included-p ((env static-traversal) grain)
  (or (member grain (included-dependencies env))
      (member grain (traversed-dependencies-r env))))

(defmethod issue-dependency ((env static-traversal) grain)
  ;;; TODO: avoid dependencies that are already in the base image!
  (push grain (traversed-dependencies-r env)))

(defmethod issue-load-command ((env static-traversal) command)
  ;;; TODO: avoid dependencies that are already in the base image!
  (pushnew command (traversed-load-commands-r env) :test 'equal))

(defmethod traversed-dependencies ((env static-traversal))
  (reverse (traversed-dependencies-r env)))

(defmethod traversed-load-commands ((env static-traversal))
  (reverse (traversed-load-commands-r env)))

(defmethod load-command-issued-p ((env static-traversal) command)
  (and (member command (traversed-load-commands-r env) :test 'equal)
       t))

(define-simple-dispatcher graph-for #'graph-for-atom)

(define-graph-for :when (env expression &rest dependencies)
  (when (evaluate-condition env expression)
    (graph-for* env dependencies)))

(define-graph-for :cond (env &rest cond-expressions)
  (loop :for cond-expression :in cond-expressions
        :when (evaluate-condition env (car cond-expression))
        :do (return (graph-for env (cdr cond-expression)))))

(defmethod graph-for ((env static-traversal) spec)
  (let* ((current-grains-r (traversed-grain-names-r env))
         (current-grains (reverse current-grains-r))
         (circularity (member spec current-grains :test 'equal)))
    (when circularity
      (error "circularity in the dependencies:~%~{ ~S~%~}" circularity))
    (call-with-grain-registration
     spec
     #'(lambda ()
         (graph-for-dispatcher
          (make-instance
           'static-traversal
           :grain-names (cons spec current-grains-r))
          spec)))))

;; :WHEN and :COND can return 0 or multiple grains.
;; Allow graph-for to return a list of grains for :WHEN and :COND.
(defun graph-for* (env specs)
  (remove-duplicates
   (mapcan #'(lambda (spec)
	       (let ((grain (graph-for env spec)))
		 (assert (or grain
			     (equal :when (car spec))
			     (equal :cond (car spec)))
			 "Couldn't find grain for spec: ~S" spec)
		 (if (not (listp grain))
		     (list grain)
		     grain)))
	   (remove-duplicates specs :from-end t :test 'equal))
   :from-end t))

(defun graph-for-compiled (env spec)
  (graph-for env (compiled-dependency spec)))

(defun load-command-for* (env specs)
  (dolist (spec specs)
    (load-command-for env spec)))

(defmethod graph-for-atom (env (name string))
  (graph-for-lisp-module env name))

(define-graph-for :lisp (env name)
  (graph-for-atom env name))

(defun graph-for-lisp-module (env name)
  (let* ((grain (resolve-absolute-module-name name))
	 (fullname (fullname grain))
	 (generator (gethash fullname *generators*)))
    (check-type grain lisp-grain)
    (when (and generator (not (generator-computation generator)))
      (let ((dependencies (append (build-dependencies grain) (generator-dependencies generator)))
	    (targets (generator-targets generator)))
	(unless dependencies
	  (error "graph-for-lisp-module: Need dependencies to generate file ~S.~%" fullname))
	(mapcar (lambda (target) (slot-makunbound target 'computation)) targets)
	(let ((pre-image (pre-image-for env grain)))
          (setf (included-dependencies env) (image-included pre-image))
	  (load-command-for* env dependencies)
          (setf (generator-computation generator)
                (make-computation
                 ()
                 :outputs targets
                 :inputs (traversed-dependencies env)
                 :command
                 `(:xcvb-driver-command (:image ,(fullname pre-image))
                   ,@(traversed-load-commands env)))))))
    grain))

(define-graph-for :build (env name)
  (graph-for-build-named env name))

(define-graph-for :compile-build (env name)
  (graph-for-build-named env name))

(defun graph-for-build-named (env name)
  (graph-for-build-grain env (registered-build name)))

(defmethod graph-for-build-grain ((env static-traversal) (grain build-grain))
  (handle-lisp-dependencies grain)
  (let ((post-image-name (build-image-name grain)))
    (if post-image-name
        (graph-for env `(:image ,post-image-name))
        (make-phony-grain
         :name `(:build ,(fullname grain))
         :dependencies (graph-for*
                        env (append (compile-dependencies grain) (load-dependencies grain)))))))

(define-graph-for :image (env name)
  (cond
    ((equal name "/_") ;; special: initial image
     (graph-for-image-grain env name nil nil))
    ((string-prefix<= "/_pre/" name)
     (let* ((build-name (subseq name 5))
            (build (registered-build build-name)))
       (check-type build build-grain)
       (handle-lisp-dependencies build)
       (let* ((dependencies (build-dependencies build))
              (starting-build (imaged-build-starting-dependencies-p dependencies)))
         (if starting-build
             ;; if the build dependency is a build AND has a post-image, use it as pre-image!
             (graph-for-image-grain env name starting-build (cdr dependencies))
             ;; otherwise, start from the common pre-image
             (graph-for-image-grain env name "/_" dependencies)))))
    (t
     (let* ((build (registered-build name)))
       (graph-for-image-grain
        env name (build-pre-image-name build) (load-dependencies build))))))

(defun pre-image-for (env grain)
  (let ((build (build-grain-for grain)))
    (check-type build build-grain)
    (graph-for env `(:image ,(build-pre-image-name build)))))

(defun graph-for-image-grain (env name pre-image-name dependencies)
  (with-nesting ()
    (let ((pre-image
           (when pre-image-name
             (graph-for env `(:image ,pre-image-name))))))
    (progn
      (when pre-image-name
        (check-type pre-image image-grain)
        (setf (included-dependencies env) (image-included pre-image))))
    (let ((pre-dependencies
           (if pre-image
             (list pre-image)
             (graph-for* env *lisp-setup-dependencies*)))))
    (progn
      (load-command-for* env dependencies))
    (let* ((dependencies
            (append pre-dependencies (traversed-dependencies env)))
           (included
            (if pre-image
                (append (image-included pre-image) dependencies)
                dependencies))
           (grain
            (make-grain 'image-grain
                        :fullname `(:image ,name)
                        :included included)))
      (or (not pre-image-name)
	  pre-image
	  (error "pre-image is nil"))
      (make-computation ()
        :outputs (list grain)
        :inputs dependencies
        :command
        `(:xcvb-driver-command
          ,(if pre-image-name
               `(:image ,(fullname pre-image))
               `(:load ,(mapcar #'fullname pre-dependencies)))
          (:create-image
           (:image ,name)
           ,@(traversed-load-commands env))))
      grain)))

(define-graph-for :source (env name &key in)
  (declare (ignore env))
  (make-source-grain :name name :in in))

(defun make-source-grain (&key name in)
  (make-instance
   'source-grain
   :name name
   :in in
   :fullname `(:source ,name :in ,in)))

(define-graph-for :asdf (env system-name)
  (declare (ignore env))
  (make-asdf-grain :name system-name
                   :implementation *lisp-implementation-type*))

(define-graph-for :fasl (env lisp-name)
  (first (graph-for-fasls env lisp-name)))

(define-graph-for :cfasl (env lisp-name)
  (second (graph-for-fasls env lisp-name)))

(defmethod graph-for-fasls ((env static-traversal) fullname)
  (check-type fullname string)
  (let ((lisp (graph-for env fullname)))
    (check-type lisp lisp-grain)
    (handle-lisp-dependencies lisp)
    (issue-dependency env lisp)
    (let ((load-dependencies (load-dependencies lisp))
          (compile-dependencies (compile-dependencies lisp))
          (build-dependencies (build-dependencies lisp))
          (pre-image (pre-image-for env lisp)))
      (setf (included-dependencies env) (image-included pre-image))
      (load-command-for* env build-dependencies)
      (load-command-for* env compile-dependencies)
      (let ((outputs (fasl-grains-for-name fullname load-dependencies compile-dependencies
                                           build-dependencies)))
        (make-computation
         ()
         :outputs outputs
         :inputs (cons pre-image (traversed-dependencies env))
         :command
         `(:xcvb-driver-command
           (:image ,(fullname pre-image))
           ,@(traversed-load-commands env)
           (:compile-lisp ,fullname)))
        outputs))))
