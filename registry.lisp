;;;;; Registry mapping names to grains, particularly BUILD files.
#+xcvb (module (:depends-on ("grain-interface" "specials")))

(in-package :xcvb)

;;; The registry itself
;; TODO: have distinct registries for builds and grains?

(defun registered-grain (name)
  (gethash name *grains*))

(defun (setf registered-grain) (grain name)
  (setf (gethash name *grains*) grain))

(defun call-with-grain-registration (fullname function &rest args)
  (let ((previous (registered-grain fullname)))
    (or previous (register-computed-grain fullname function args))))

(defun register-computed-grain (fullname function &optional args)
  (let* ((grain (apply function args))
         (gname (fullname grain)))
    ;;TODO: fix this!
    ;;(assert (equal fullname (fullname grain)))
    ;; Apparently, this assertion doesn't hold at least because of aliasing between
    ;; "foo" and (:lisp "foo"). Need to investigate where we're being sloppy,
    ;; use some normalization function and restore this invariant.
    (unless (or (equal fullname gname)
                (and (stringp gname)
                     (or (equal fullname `(:lisp ,gname))
                         (equal fullname `(:build ,gname)))))
      (log-format 7 "~&Registered grain for name ~S has fullname ~S~%" fullname gname))
    (setf (registered-grain fullname) grain)
    grain))

(defun make-grain (class &rest args &key fullname &allow-other-keys)
  (apply #'call-with-grain-registration fullname #'make-instance class args))

;;; Special magic for build entries in the registry

(defclass build-registry-entry ()
  ((root
    :initarg :root :accessor bre-root
    :documentation "root path under which the entry was found")))

(defgeneric brc-pathnames (brc))

(defclass build-registry-conflict (build-registry-entry simple-print-object-mixin)
  ((fullname
    :initarg :fullname :reader fullname)
   (pathnames
    :initarg :pathnames :reader brc-pathnames
    :documentation "pathnames of conflicting build files with the same name")))

(defmethod brc-pathnames ((build build-grain))
  (list (grain-pathname build)))
