;;;;; Registry mapping names to grains, particularly BUILD files.
#+xcvb (module (:depends-on ("grain-interface" "specials")))

(in-package :xcvb)

;;; The registry itself
;; TODO: have distinct registries for builds and grains?

(defun registered-grain (name)
  (gethash name *grains*))

(defun (setf registered-grain) (grain name)
  (let ((previous (registered-grain name)))
    (when (and previous (not (eq grain previous)))
      (error "There already exists a grain named ~A: ~S"
             name (registered-grain name))))
  (setf (gethash name *grains*) grain))

(defun call-with-grain-registration (fullname function &rest args)
  (let ((previous (registered-grain fullname)))
    (or previous (register-computed-grain fullname function args))))

(defun register-computed-grain (fullname function &optional args)
  (let* ((grain (apply function args))
         (gname (fullname grain)))
    ;;TODO: fix this!
    (unless (equal fullname gname)
      (log-format 7 "Registered grain for name ~S has fullname ~S" fullname gname))
    (setf (registered-grain fullname) grain)
    grain))

(defun make-grain (class &rest args &key fullname &allow-other-keys)
  (apply #'call-with-grain-registration fullname #'make-instance class args))

