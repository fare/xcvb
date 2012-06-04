#+xcvb (module (:depends-on ("names" "computations")))

(in-package :xcvb)

(declaim (optimize (speed 2) (safety 3) (debug 3) (compilation-speed 0)))

(defgeneric next-traversal (env spec))
(defgeneric dependency-already-included-p (env grain))
(defgeneric issue-dependency (env grain))
(defgeneric issue-build-command (env command))
(defgeneric issue-link-element (env element))
(defgeneric traversed-dependencies (env))
(defgeneric traversed-build-commands (env))
(defgeneric traversed-link-elements (env command))
(defgeneric build-command-issued-p (env command))
(defgeneric link-element-issued-p (env element))
(defgeneric graph-for (env spec)
  (:documentation "Build the dependency graph for given dependency, return the node for it"))
(defgeneric graph-for-atom (env atom))
(defgeneric graph-for-build-module-grain (env grain))
(defgeneric graph-for-lisp (env name))
(defgeneric graph-for-fasls (env name))
(defgeneric graph-for-fasl (env name))
(defgeneric graph-for-cfasl (env name))
(defgeneric graph-for-lisp-object (env name))
(defgeneric graph-for-dynamic-library (env name))
(defgeneric graph-for-static-library (env name))
(defgeneric graph-for-build-libraries (env name))
(defgeneric graph-for-build (env name))
(defgeneric graph-for-compile-build (env name))
(defgeneric graph-for-build-named (env name))
(defgeneric graph-for-image (env name))
(defgeneric graph-for-image-grain (env name pre-image-name dependencies &key))
(defgeneric graph-for-executable (env name))
(defgeneric graph-for-source (env name &key in))
(defgeneric graph-for-asdf (env name))
(defgeneric graph-for-require (env name))
(defgeneric ensure-grain-generated (env grain))
(defgeneric tweak-dependency (env dep))
(defgeneric linking-traversal-p (env))

(defclass traversal (simple-print-object-mixin)
  ((image-setup
    :accessor image-setup
    :documentation "xcvb-driver-command options to setup the image for the current world")
   (grain-names
    :initform nil
    :initarg :grain-names
    :reader traversed-grain-names-r
    :documentation "grain names in the stack of things we try to create -- to avoid circularities")
   ;; do we also need them as a set? possibly... to be measured.
   ;; we might benefit from a pure functional set implementation; maybe use fare-utils:pure of FSet
   (issued-dependencies
    :initform (make-hashset :test 'equal)
    :accessor issued-dependencies
    :documentation "dependencies issued as part of current computation, as a set")
   (traversed-dependencies-r
    :initform nil
    :accessor traversed-dependencies-r
    :documentation "dependencies issued as part of the current computation, in reverse order")))

(defmethod graph-for ((env traversal) spec)
  (log-format 10 "Producing graph-for ~S" spec)
  (let ((current-grains-r (reverse (traversed-grain-names-r env))))
    (let ((mem (member spec current-grains-r :test 'equal)))
      (when mem
	(user-error
	 "There is a circularity in the dependencies:~%~{ ~S~%  includes~%~} ~S~%"
	 mem (first mem)))))

  (let ((grain (do-graph-for (next-traversal env spec) spec)))
    (if (typep grain 'buildable-grain)
	(ensure-grain-generated env grain)
	(user-error "Grain ~S for ~S is not buildable" grain spec))
    grain))

(defmethod ensure-grain-generated (env (grain buildable-grain))
  (let ((generator (grain-generator grain)))
    (when (and generator (not (and (slot-boundp grain 'computation)
                                   (grain-computation grain))))
      (run-generator (next-traversal env (fullname grain)) generator))))

(defun do-graph-for (env spec)
  (call-with-grain-registration
   spec
   #'(lambda () (graph-for-dispatcher env spec))))

(defmethod next-traversal ((env traversal) spec)
  (make-instance
   (class-of env)
   :grain-names (cons spec (traversed-grain-names-r env))))

(defmethod traversed-dependencies ((env traversal))
  (reverse (traversed-dependencies-r env)))

(defmethod dependency-already-included-p :before (env grain)
  (check-type env traversal)
  (check-type grain grain))

(defmethod dependency-already-included-p ((env traversal) grain)
  (gethash grain (issued-dependencies env)))

(defmethod issue-dependency :before (env grain)
  (check-type env traversal)
  (check-type grain grain))

(defmethod issue-dependency ((env traversal) grain)
  ;; Note: long dependency lists do not mix well with pretty-print.
  (log-format-pp 10 "Issuing dependency for ~A" grain)
  (setf (gethash grain (issued-dependencies env)) t)
  (push grain (traversed-dependencies-r env)))

(defun call-with-dependency-loading (env grain thunk)
  (unless (dependency-already-included-p env grain)
    (issue-dependency env grain)
    (funcall thunk)))

(defmacro with-dependency-loading ((env grain) &body body)
  `(call-with-dependency-loading ,env ,grain (lambda () ,@body)))

(define-simple-dispatcher graph-for #'graph-for-atom :generic t)

(defmethod traversed-build-commands ((env traversal))
  (reverse (traversed-build-commands-r env)))

(defmethod build-command-issued-p ((env traversal) command)
  (values (gethash command (issued-build-commands env))))

(define-graph-for :asdf ((env traversal) system-name)
  (declare (ignorable env))
  (make-asdf-grain :name system-name
                   :implementation *lisp-implementation-type*))

(define-graph-for :require ((env traversal) name)
  (declare (ignorable env))
  (make-require-grain :name name))

(defun handle-target (fullname)
  (let* ((target (if fullname
                   (or (resolve-absolute-module-name fullname)
                       (resolve-asdf-name fullname))
                   (let* ((build-file (probe-file "build.xcvb"))
                          (build-module-grain
                           (and build-file (pathname-build build-file))))
                     ;; Question: should we make the below error cases warnings,
                     ;; and override conflicts with the current build?
                     ;; NB: User can put . in front of his CL_SOURCE_REGISTRY
                     ;; if that's what he wants.
                     (etypecase build-module-grain
                       (build-module-grain
                        build-module-grain)
                       (null
                        (user-error "No build specified, and no build.xcvb in the current directory"))
                       (invalid-build-registry-entry
                        (user-error "Implicitly specified build.xcvb in current directory ~
                                but it is invalid:~%~A~&"
                                    (invalid-build-reason build-module-grain)))))))
         (build (typecase target
                  ((or lisp-module-grain executable-grain) (build-module-grain-for target))
                  (asdf-grain nil)
                  (t
                    (user-error "User requested build ~S but it can't be found.~%~
			    You may check available builds with xcvb ssr.~%" fullname))))
         (name (fullname target))
         (dep (etypecase target
                (build-module-grain `(:build ,name))
                (lisp-module-grain `(:fasl ,(second name)))
                (asdf-grain name)
		(executable-grain name)))
         (directory (pathname-directory-pathname
                     (cond
                       (build (grain-pathname build))
                       ((typep target 'asdf-grain)
                        (nth-value 2 (asdf:locate-system (second name))))))))
    (values dep (or build target) directory)))
