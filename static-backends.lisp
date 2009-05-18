(in-package :xcvb)

#|
TODO: handle the fact that to load a BUILD,
you don't want to load an image (unless we implement support for something like SB-HEAPDUMP),
but a collection of fasls (or cfasls, depending) and dependencies.

This in turn will necessitate refactoring of the load-command-for infrastructure.
See dependencies-interpreter for details.

Good news is, we can start implementing the Makefile backend even before we support that,
and it will "just work". The limitation is that at this point,
you can't directly depend on a build file --
you HAVE to depend on individual lisp files.
I should add some explicit error message about that --
or bite the bullet and do the right thing.

Add checks to detect and report circular dependencies!

|#

(defclass static-traversal ()
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
  (push command (traversed-lisp-commands-r env)))

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
      (error "Circularity in the dependencies:~%~{ ~S~%~}" circularity))
    (call-with-grain-registration
     spec
     #'(lambda ()
         (graph-for-dispatcher
          (make-instance 'static-traversal
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

(defun graph-for-lisp-module (name)
  (resolve-absolute-module-name name))

(define-graph-for :build (env name)
  (graph-for-build env name))

(defmethod graph-for-build ((env static-traversal) name)
  (let ((grain (registered-grain name)))
    (check-type grain build-grain)
    (handle-lisp-dependencies grain)
    (let* (;;; Build pre-image if needed
           (pre-image
            (when (build-requires grain)
              (graph-for-image-grain
               env
               (build-pre-image-name grain)
               (build-requires grain))))
           (*lisp-image-name*
            (if pre-image
                (fullname pre-image)
                *lisp-image-name*)))
      ;; build post-image if needed
      (let* ((post-image-name (build-image-name grain)))
        (if post-image-name
            (graph-for-image-grain
             env
             post-image-name
             (load-dependencies grain))
            (make-phony-grain
             :name `(:build ,(fullname grain))
             :dependencies (graph-for*
                            env
                            (remove-duplicates
                             (append (compile-dependencies grain)
                                     (load-dependencies grain))
                             :test #'equal))))))))


(defun graph-for-image-grain (env name dependencies)
  (let ((grain (make-grain 'image-grain :fullname `(:image ,name))))
    (load-command-for* env dependencies)
    (make-computation 'concrete-computation
      :outputs (list grain)
      :inputs (traversed-dependencies env)
      :command
      `(:lisp
        (:image ,*lisp-image-name*)
        ,@(traversed-lisp-commands env)
        (:save-image ,name)))
    grain))

(define-graph-for :asdf (env system-name)
  (declare (ignore env))
  (make-phony-grain
   :name `(:asdf ,system-name)
   :dependencies nil)) ;;TODO: make corresponding computation?

(define-graph-for :fasl (env lisp-name)
  (first (graph-for-fasls env lisp-name)))

(define-graph-for :cfasl (env lisp-name)
  (second (graph-for-fasls env lisp-name)))

(defmethod graph-for-fasls ((env static-traversal) fullname)
  (check-type fullname string)
  (let ((lisp (resolve-absolute-module-name fullname)))
    (check-type lisp lisp-grain)
    (handle-lisp-dependencies lisp)
    (let ((load-dependencies (load-dependencies lisp))
          (compile-dependencies (compile-dependencies lisp)))
      (load-command-for* env compile-dependencies)
      (let ((outputs (fasl-grains-for-name fullname load-dependencies compile-dependencies)))
        (make-computation
         'concrete-computation
         :outputs outputs
         :inputs (traversed-dependencies env)
         :command
         `(:lisp
           (:image ,*lisp-image-name*)
           ,@(traversed-lisp-commands env)
           (:compile-lisp ,fullname)))
        outputs))))

#|
Note:
* Whenever we build an image, it is implicit that we load the driver first.
* :IMAGE NIL might mean that we use the standard image then load the driver,
 or that we first dump a simple image with just the driver loaded.
|#
