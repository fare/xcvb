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
   (lisp-commands-r ;;; lisp commands issued so far to run the current compilation.
    :initform nil
    :accessor traversed-lisp-commands-r)))

(defmethod dependency-already-included-p ((env static-traversal) grain)
  (or (member grain (included-dependencies env))
      (member grain (traversed-dependencies-r env))))

(defmethod issue-dependency ((env static-traversal) grain)
  ;;; TODO: avoid dependencies that are already in the base image!
  (push grain (traversed-dependencies-r env)))

(defmethod issue-load-command ((env static-traversal) command)
  ;;; TODO: avoid dependencies that are already in the base image!
  (pushnew command (traversed-lisp-commands-r env) :test 'equal))

(defmethod traversed-dependencies ((env static-traversal))
  (reverse (traversed-dependencies-r env)))

(defmethod traversed-lisp-commands ((env static-traversal))
  (reverse (traversed-lisp-commands-r env)))

(defmethod lisp-command-issued-p ((env static-traversal) command)
  (and (member command (traversed-lisp-commands-r env) :test 'equal)
       t))

(define-simple-dispatcher graph-for #'graph-for-atom)

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

(defun graph-for* (env specs)
  (remove-duplicates
   (mapcar #'(lambda (spec) (graph-for env spec))
           (remove-duplicates specs :from-end t :test 'equal))
   :from-end t))

(defun graph-for-compiled (env spec)
  (graph-for env (compiled-dependency spec)))

(defun load-command-for* (env specs)
  (dolist (spec specs)
    (load-command-for env spec)))

(defmethod graph-for-atom (env (name string))
  (declare (ignore env))
  (graph-for-lisp-module name))

(define-graph-for :lisp (env name)
  (graph-for-atom env name))

(defun graph-for-lisp-module (name)
  (resolve-absolute-module-name name))

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
       (handle-lisp-dependencies build)
       (let ((dependencies (build-dependencies build)))
         (if (and (consp dependencies)
                  (consp (car dependencies))
                  (eq :build (caar dependencies)))
             ;; if the build dependency is a build, use its post-image as pre-image!
             (graph-for-image-grain env name (cadar dependencies) (cdr (build-dependencies build)))
             ;; otherwise, start from the common pre-image
             (graph-for-image-grain env name "/_" (build-dependencies build))))))
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
      (make-computation 'concrete-computation
        :outputs (list grain)
        :inputs dependencies
        :command
        `(:lisp
          ,(if pre-image-name
               `(:image ,(fullname pre-image))
               `(:load ,(mapcar #'fullname pre-dependencies)))
          (:create-image
           (:image ,name)
           ,@(traversed-lisp-commands env))))
      grain)))

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
          (pre-image (pre-image-for env lisp)))
      (setf (included-dependencies env) (image-included pre-image))
      (load-command-for* env compile-dependencies)
      (let ((outputs (fasl-grains-for-name fullname load-dependencies compile-dependencies)))
        (make-computation
         'concrete-computation
         :outputs outputs
         :inputs (cons pre-image (traversed-dependencies env))
         :command
         `(:lisp
           (:image ,(fullname pre-image))
           ,@(traversed-lisp-commands env)
           (:compile-lisp ,fullname)))
        outputs))))
