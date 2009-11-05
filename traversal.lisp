#+xcvb (module (:depends-on ("names" "specials")))

(in-package :xcvb)

(defgeneric next-traversal (env spec))
(defgeneric dependency-already-included-p (env grain))
(defgeneric issue-dependency (env grain))
(defgeneric issue-build-command (env command))
(defgeneric traversed-dependencies (env))
(defgeneric traversed-build-commands (env))
(defgeneric build-command-issued-p (env command))
(defgeneric graph-for (env spec))
(defgeneric graph-for-atom (env atom))
(defgeneric graph-for-build-grain (env grain))
(defgeneric graph-for-fasls (env name))
(defgeneric graph-for-lisp (env name))
(defgeneric graph-for-fasls (env name))
(defgeneric graph-for-fasl (env name))
(defgeneric graph-for-cfasl (env name))
(defgeneric graph-for-build (env name))
(defgeneric graph-for-compile-build (env name))
(defgeneric graph-for-build-named (env name))
(defgeneric graph-for-image (env name))
(defgeneric graph-for-image-grain (env name pre-image-name dependencies))
(defgeneric graph-for-source (env name &key in))
(defgeneric graph-for-asdf (env name))
(defgeneric graph-for-require (env name))

(defclass xcvb-traversal (simple-print-object-mixin)
  ((image-setup
    :accessor image-setup
    :documentation "xcvb-driver-command options to setup the image for the current world")
   (grain-names
    :initform nil
    :initarg :grain-names
    :reader traversed-grain-names-r
    :documentation "grain names in the stack of things we try to create -- to avoid circularities")
   ;; do we also need them as a set? possibly... to be measured.
   ;; we might benefit from a pure functional set implementation; maybe use FSet?
   (issued-dependencies
    :initform (make-hashset :test 'equal)
    :accessor issued-dependencies
    :documentation "dependencies issued as part of current computation, as a set")
   (traversed-dependencies-r
    :initform nil
    :accessor traversed-dependencies-r
    :documentation "dependencies issued as part of the current computation, in reverse order")))

(defmethod graph-for ((env xcvb-traversal) spec)
  (let* ((current-grains-r (traversed-grain-names-r env)))
    (when (member spec current-grains-r :test 'equal)
      (error "circularity in the dependencies:~%~{ ~S~%~}"
             (member spec (reverse current-grains-r) :test 'equal)))
    (do-graph-for (next-traversal env spec) spec)))

(defun do-graph-for (env spec)
  (call-with-grain-registration
   spec
   #'(lambda () (graph-for-dispatcher env spec))))

(defmethod next-traversal ((env xcvb-traversal) spec)
  (make-instance
   (class-of env)
   :grain-names (cons spec (traversed-grain-names-r env))))

(defmethod traversed-dependencies ((env xcvb-traversal))
  (reverse (traversed-dependencies-r env)))

(defmethod dependency-already-included-p :before (env grain)
  (check-type env xcvb-traversal)
  (check-type grain grain))

(defmethod dependency-already-included-p ((env xcvb-traversal) grain)
  (gethash grain (issued-dependencies env)))

(defmethod issue-dependency :before (env grain)
  (check-type env xcvb-traversal)
  (check-type grain grain))

(defmethod issue-dependency ((env xcvb-traversal) grain)
  (setf (gethash grain (issued-dependencies env)) t)
  (push grain (traversed-dependencies-r env)))

(defun call-with-dependency-loading (env grain thunk)
  (unless (dependency-already-included-p env grain)
    (issue-dependency env grain)
    (funcall thunk)))

(defmacro with-dependency-loading ((env grain) &body body)
  `(call-with-dependency-loading ,env ,grain (lambda () ,@body)))

(define-simple-dispatcher graph-for #'graph-for-atom :generic)

(defmethod traversed-build-commands ((env xcvb-traversal))
  (reverse (traversed-build-commands-r env)))

(defmethod build-command-issued-p ((env xcvb-traversal) command)
  (values (gethash command (issued-build-commands env))))

(define-graph-for :asdf ((env xcvb-traversal) system-name)
  (declare (ignore env))
  (make-asdf-grain :name system-name
                   :implementation *lisp-implementation-type*))

(define-graph-for :require ((env xcvb-traversal) name)
  (declare (ignore env))
  (make-require-grain :name name))
