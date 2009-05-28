(in-package :xcvb)

(defclass static-traversal (simple-print-object-mixin)
  ((grain-names ;;; grain names in the stack of things we try to create -- to avoid circularities
    :initform nil
    :initarg :grain-names
    :reader traversed-grain-names-r)
   (dependencies-r ;;; dependencies discovered so far while examining the current computation
    :initform nil
    :accessor traversed-dependencies-r)
   (lisp-commands-r ;;; lisp commands issued so far to run the current compilation.
    :initform nil
    :accessor traversed-lisp-commands-r)))


(defmethod issue-dependency ((env static-traversal) grain)
  (pushnew grain (traversed-dependencies-r env)))

(defmethod issue-load-command ((env static-traversal) command)
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
  (mapcar #'(lambda (spec) (graph-for env spec)) specs))

(defun graph-for-compiled (env spec)
  (graph-for env (compiled-dependency spec)))

(defun load-command-for* (env specs)
  (dolist (spec specs)
    (load-command-for env spec)))

(defgeneric graph-for-atom (env atom))

(defmethod graph-for-atom (env (name string))
  (declare (ignore env))
  (graph-for-lisp-module name))

(define-graph-for :lisp (env name)
  (graph-for-atom env name))

(defun graph-for-lisp-module (name)
  (resolve-absolute-module-name name))

(define-graph-for :build (env name)
  (graph-for-build-named env name))

(defun graph-for-build-named (env name)
  (graph-for-build env (registered-grain name)))

(defmethod graph-for-build ((env static-traversal) (grain build-grain))
  (handle-lisp-dependencies grain)
  (let ((post-image-name (build-image-name grain)))
    (if post-image-name
        (graph-for env `(:image ,post-image-name))
        (make-phony-grain
         :name `(:build ,(fullname grain))
         :dependencies (graph-for*
                        env
                        (remove-duplicates
                         (append
                          (compile-dependencies grain)
                          (load-dependencies grain))
                         :test #'equal))))))

(define-graph-for :image (env name)
  (cond
    ((equal name "/_") ;; special: initial image
     (graph-for-image-grain env name nil nil))
    ((string-prefix<= "/_pre/" name)
     (let* ((build-name (subseq name 5))
            (build (registered-grain build-name)))
       (check-type build build-grain)
       (graph-for-image-grain
        env name "/_" (build-requires build))))
    (t
     (let* ((build (registered-grain name)))
       (check-type build build-grain)
       (graph-for-image-grain
        env name (build-pre-image-name build) (load-dependencies build))))))

(defun pre-image-for (env grain)
  (let ((build (build-grain-for grain)))
    (check-type build build-grain)
    (graph-for env `(:image ,(build-pre-image-name build)))))

(defun graph-for-image-grain (env name pre-image-name dependencies)
  (let* ((grain
          (make-grain 'image-grain :fullname `(:image ,name)))
         (pre-image
          (when pre-image-name
            (graph-for env `(:image ,pre-image-name))))
         (pre-dependencies
          (if pre-image
              (list pre-image)
              (graph-for* env *lisp-setup-dependencies*))))
    (DBG :gfig name pre-image-name dependencies pre-image pre-dependencies)
    (load-command-for* env dependencies)
    (make-computation 'concrete-computation
      :outputs (list grain)
      :inputs (append pre-dependencies (traversed-dependencies env))
      :command
      `(:lisp
        ,(if pre-image-name
             `(:image ,(fullname pre-image))
             `(:load ,(mapcar #'fullname pre-dependencies)))
        (:create-image
         (:image ,name)
         ,@(traversed-lisp-commands env))))
    grain))

(define-graph-for :asdf (env system-name)
  (let ((superseding (registered-grain `(:supersedes-asdf ,system-name))))
    (etypecase superseding
      (null
       (make-asdf-grain :name system-name
                        :implementation *lisp-implementation-type*))
      (build-grain
       (log-format 5 "Grain ~S declared a dependency on ASDF system :~A but superseding it with ~S"
                   (fullname (first (traversed-grain-names-r env))) system-name (fullname superseding))
       (graph-for-build env superseding))
      (build-registry-conflict
       (error "Trying to use ASDF system :~A claimed by conflicting builds ~S"
              system-name superseding)))))

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
