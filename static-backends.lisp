#+xcvb (module (:depends-on
		("specials" "lisp-grain" "dependencies-interpreter" "logging")))
(in-package :xcvb)

(defclass static-traversal (xcvb-traversal)
  ((image-setup
    :accessor image-setup
    :documentation "xcvb-driver-command options to setup the image for the current computation")
   (included-dependencies
    :initform (make-hashset :test 'equal)
    :accessor included-dependencies
    :documentation "dependencies included in the image used by current computation, as a set")
   (issued-dependencies
    :initform (make-hashset :test 'equal)
    :accessor issued-dependencies
    :documentation "dependencies issued as part of current computation, as a set")
   (dependencies-r
    :initform nil
    :accessor traversed-dependencies-r
    :documentation "dependencies issued as part of the current computation, in reverse order")
   (issued-load-commands
    :initform (make-hashset :test 'equal)
    :accessor issued-load-commands
    :documentation "load commands issued so far to run the current compilation, as a set")
   (load-commands-r
    :initform nil
    :accessor traversed-load-commands-r
    :documentation "load commands issued so far to run the current compilation, in reverse order")))

(defmethod dependency-already-included-p ((env static-traversal) grain)
  (or (gethash grain (included-dependencies env))
      (call-next-method)))

(defmethod issue-load-command ((env static-traversal) command)
  (unless (gethash command (issued-load-commands env))
    (setf (gethash command (issued-load-commands env)) t)
    (push command (traversed-load-commands-r env))))

(defmethod traversed-load-commands ((env static-traversal))
  (reverse (traversed-load-commands-r env)))

(defmethod load-command-issued-p ((env static-traversal) command)
  (values (gethash command (issued-load-commands env))))

(defun graph-for-compiled (env spec)
  (graph-for env (compiled-dependency spec)))

(defmethod graph-for-atom (env name)
  (error "invalid normalized dependency ~S" name))

(defun include-image-dependencies (env image)
  (when image
    (check-type image image-grain)
    (setf (included-dependencies env) (image-included image))))

(define-graph-for :lisp ((env static-traversal) name)
  (let* ((grain (resolve-absolute-module-name name))
	 (fullname (if grain (fullname grain) (error "Couldn't resolve ~S to a lisp module" name)))
	 (generator (gethash fullname *generators*)))
    (check-type grain lisp-grain)
    (when (and generator (not (generator-computation generator)))
      (let ((dependencies (append (build-dependencies grain) (generator-dependencies generator)))
	    (targets (generator-targets generator)))
	(unless dependencies
	  (error "graph-for-lisp: Need dependencies to generate file ~S.~%" fullname))
	(dolist (target targets)
          (slot-makunbound target 'computation))
        (pre-image-for env grain)
        (load-command-for* env dependencies)
        (setf (generator-computation generator)
              (make-computation
               ()
               :outputs targets
               :inputs (traversed-dependencies env)
               :command
               `(:xcvb-driver-command
                 ,(image-setup env)
                 ,@(traversed-load-commands env))))))
    grain))

(define-graph-for :build (env name)
  (graph-for-build-named env name))

(define-graph-for :compile-build (env name)
  (graph-for-build-named env name))

(defmethod graph-for-build-named (env name)
  (graph-for-build-grain env (registered-build name)))

(defmethod graph-for-build-grain :before (env (grain build-grain))
  (handle-lisp-dependencies grain))

(defmethod graph-for-build-grain ((env static-traversal) (grain build-grain))
  (let ((post-image-name (build-image-name grain)))
    (if post-image-name
        (graph-for env `(:image ,post-image-name))
        (make-phony-grain
         :name `(:build ,(fullname grain))
	 :dependencies
	 (progn (load-command-for* env (compile-dependencies grain))
                (load-command-for* env (load-dependencies grain))
		(traversed-dependencies env))))))

(define-graph-for :image ((env static-traversal) name)
  (cond
    ((null name) ; special: no image
     nil)
    ((equal name "/_") ;; special: initial image
     (graph-for-image-grain env name nil nil))
    ((string-prefix<= "/_pre/" name)
     (let* ((build-name (subseq name 5))
            (build (registered-build build-name)))
       (check-type build build-grain)
       (handle-lisp-dependencies build)
       (let* ((dependencies (build-dependencies build))
              (starting-build-name (build-starting-dependencies-p dependencies))
              (starting-build-image-name
               (when starting-build-name
                 (build-post-image-name (registered-build starting-build-name)))))
         (graph-for-image-grain env name (or starting-build-image-name "/_") dependencies))))
    (t
     (let* ((build (registered-build name)))
       (graph-for-image-grain
        env name (build-pre-image-name build) (load-dependencies build))))))

(defun pre-image-for (env grain)
  (let ((build (build-grain-for grain)))
    (check-type build build-grain)
    (issue-image-named env (build-pre-image-name build))))

(defun issue-image-named (env name)
  (if name
    (let ((image (graph-for env `(:image ,name))))
      (issue-dependency env image)
      (include-image-dependencies env image)
      (setf (image-setup env) `(:image ,(fullname image)))
      image)
    (progn
      (setf (image-setup env)
            `(:load ,(loop
                       :for dep :in *lisp-setup-dependencies*
                       :for grain = (graph-for env dep)
                       :do (issue-dependency env grain)
                       :collect (fullname grain))))
      nil)))

(defun graph-for-image-grain (env name pre-image-name dependencies)
  (let ((pre-image (issue-image-named env pre-image-name)))
    (load-command-for* env dependencies)
    (let* ((traversed (traversed-dependencies env))
           (grain
            (make-grain 'image-grain
                        :fullname `(:image ,name)
                        :included (make-hashset :test 'equal
                                                :list traversed
                                                :set (when pre-image (image-included pre-image))))))
      (make-computation ()
        :outputs (list grain)
        :inputs traversed
        :command
        `(:xcvb-driver-command ,(image-setup env)
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

(define-graph-for :asdf ((env static-traversal) system-name)
  (declare (ignore env))
  (make-asdf-grain :name system-name
                   :implementation *lisp-implementation-type*))

(define-graph-for :fasl ((env static-traversal) lisp-name)
  (first (graph-for-fasls env lisp-name)))

(define-graph-for :cfasl ((env static-traversal) lisp-name)
  (second (graph-for-fasls env lisp-name)))

(defmethod graph-for-fasls ((env static-traversal) fullname)
  (check-type fullname string)
  (let ((lisp (graph-for-lisp env fullname))
        (specialp (member `(:fasl ,fullname) *lisp-setup-dependencies* :test #'equal)))
    (check-type lisp lisp-grain)
    (handle-lisp-dependencies lisp)
    (let ((build-dependencies (unless specialp (build-dependencies lisp)))
          (compile-dependencies (compile-dependencies lisp))
          (load-dependencies (load-dependencies lisp)))
      (issue-dependency env lisp)
      (unless specialp
        (pre-image-for env lisp)
        (load-command-for* env build-dependencies))
      (load-command-for* env compile-dependencies)
      (let* ((outputs (fasl-grains-for-name fullname load-dependencies compile-dependencies
                                            build-dependencies)))
        (when specialp
          (setf outputs (list (car outputs))))
        (make-computation
         ()
         :outputs outputs
         :inputs (traversed-dependencies env)
         :command
         (if (not specialp)
           `(:xcvb-driver-command
             ,(image-setup env)
             (:compile-lisp (,fullname) ,@(traversed-load-commands env)))
           `(:xcvb-driver-command
             (:load ,(mapcar #'remove-load-file (traversed-load-commands env)))
             (:compile-file-directly ,fullname))))
        outputs))))

(defun remove-load-file (x)
  (assert (and (list-of-length-p 2 x) (eq (first x) :load-file)))
  (second x))
