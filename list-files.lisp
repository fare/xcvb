#+xcvb (module (:depends-on ("dependencies-interpreter" "main")))

(in-package :xcvb)

;;; LIST-FILES
(defparameter *files-list* ())
(defparameter *files-table* (make-hash-table :test 'equal))

(defclass file-listing-traversal (simplifying-traversal)
  ())

(defmethod graph-for :around ((env file-listing-traversal) spec)
  (multiple-value-bind (v foundp) (gethash spec *files-table*)
    (if foundp
        v
        (progn
          (setf (gethash spec *files-table*) nil)
          (let ((v (call-next-method)))
            (setf (gethash spec *files-table*) v)
            (pushnew v *files-list*)
            v)))))

(defun list-files (spec)
  (let ((env (make-instance 'file-listing-traversal))
        (*files-list* ())
        (*files-table* (make-hash-table :test 'equal)))
    (build-command-for env spec)
    (reverse *files-list*)))

;;; TODO: some magic to break circularities that are due to flattening conditionals.
(define-build-command-for :when ((env file-listing-traversal) expression &rest dependencies)
  (declare (ignore expression))
  (build-command-for* env dependencies))
(define-build-command-for :cond ((env file-listing-traversal) &rest cond-expressions)
  (dolist (cond-expression cond-expressions)
    (build-command-for* env (cdr cond-expression))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Remove XCVB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +remove-xcvb-option-spec+
  `(,@+build-option-spec+
    ,@+source-registry-option-spec+
    ,@+verbosity-option-spec+))

(defun remove-xcvb-command (&rest keys &key source-registry verbosity build debugging)
  (declare (ignore source-registry verbosity debugging))
  (apply 'handle-global-options keys)
  (setf *use-cfasls* nil)
  (remove-xcvb-from-build build))

(defun purge-xcvb-command (files)
  (initialize-environment)
  (loop :for f :in files
    :for p = (probe-file f) :do
    (when p
      (cond
        ((equal (cons (pathname-name p) (pathname-type p)) '("build" . "xcvb"))
         (delete-file p))
        ((equal (pathname-type p) "lisp")
         (log-format 4 "Removing module statements from ~A" p)
         (remove-module-from-file p))))))

;; Using the build.xcvb as a starting point, finds files and
;; strips XCVB modules from them.
(defun remove-xcvb-from-build (fullname)
  (multiple-value-bind (target-dependency build) (handle-target fullname)
    (log-format 7 "Removing XCVB from build ~A~% (path ~S)" build (grain-pathname build))
    (flet ((source-lisp-grain-p (grain)
             (log-format 7 "Inspecting grain ~A" grain)
             (when (and (typep grain 'lisp-file-grain)
                        (slot-boundp grain 'computation)
                        (null (grain-computation grain)))
               (log-format 8 "This grain is in build ~A" (build-module-grain-for grain))
               (log-format 9 "EQ: ~A EQUAL NAMES: ~A"
                           ;; for some reason, the builds are different objects(!). HOW???
                           (eq build (build-module-grain-for grain))
                           (equal (fullname build) (fullname (build-module-grain-for grain))))
               (equal (fullname build) (fullname (build-module-grain-for grain))))))
      (dolist (grain (remove-if-not #'source-lisp-grain-p (list-files target-dependency)))
        (log-format 5 "Removing module declaration from ~A" (grain-pathname grain))
        (remove-module-from-file (grain-pathname grain))))
    (log-format 5 "Deleting build file for ~A" (grain-pathname build))
    (delete-file (grain-pathname build))))
