#+xcvb (module (:depends-on ("names" "specials")))

(in-package :xcvb)

;(declaim (optimize (debug 3) (speed 1) (safety 3)))
;(declaim (optimize (debug 1) (speed 3) (safety 2)))

;;; TODO: We probably need a better interface, so that
;;; the following aspects be handled in a generic way
;;; * the fact that we don't want to load the same dependency twice
;;; * the fact that we may want to "upgrade" some cfasl's to fasl's
;;;   -- or not, because that gets tricky since we want to preserve
;;;   the order of loads, and trickier still if for whatever reason
;;;   the dependencies of the fasl are not an upgrade from the
;;;   dependencies of the cfasl
;;;   -- that condition may be tested and an error issued otherwise.
(defgeneric next-traversal (env spec))
(defgeneric dependency-already-included-p (env grain))
(defgeneric issue-dependency (env grain))
(defgeneric issue-load-command (env command))
(defgeneric traversed-dependencies (env))
(defgeneric traversed-load-commands (env))
(defgeneric load-command-issued-p (env command))
(defgeneric graph-for (env spec))
(defgeneric graph-for-atom (env atom))
(defgeneric graph-for-build-grain (env grain))
(defgeneric graph-for-fasls (env fullname))

(defclass xcvb-traversal (simple-print-object-mixin)
  ((grain-names
    :initform nil
    :initarg :grain-names
    :reader traversed-grain-names-r
    :documentation "grain names in the stack of things we try to create -- to avoid circularities")
   (issued-dependencies
    :initform (make-hashset :test 'equal)
    :accessor issued-dependencies
    :documentation "dependencies issued as part of current computation, as a set")
   (dependencies-r
    :initform nil
    :accessor traversed-dependencies-r
    :documentation "dependencies issued as part of the current computation, in reverse order")))

(defmethod graph-for :around ((env xcvb-traversal) spec)
  (let* ((current-grains-r (traversed-grain-names-r env)))
    (when (member spec current-grains-r :test 'equal)
      (error "circularity in the dependencies:~%~{ ~S~%~}"
             (member spec (reverse current-grains-r) :test 'equal)))
    ;; the next-method should not forget to pass downwards (next-traversal env spec)
    (call-next-method)))

(defmethod next-traversal ((env xcvb-traversal) spec)
  (make-instance
   (class-of env)
   :grain-names (cons spec (traversed-grain-names-r env))))

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
