#+xcvb (module (:depends-on ("dependencies-interpreter" "main")))

(in-package :xcvb)

;;; LIST-GRAINS
(defparameter *grains-list* ())
(defparameter *grains-table* (make-hash-table :test 'equal))

(defclass grain-listing-traversal (simplifying-traversal)
  ())

(defmethod graph-for :around ((env grain-listing-traversal) spec)
  (multiple-value-bind (v foundp) (gethash spec *grains-table*)
    (if foundp
        v
        (progn
          (setf (gethash spec *grains-table*) nil)
          (let ((v (call-next-method)))
            (setf (gethash spec *grains-table*) v)
            (pushnew v *grains-list*)
            v)))))

(defun list-grains (specs)
  (let ((env (make-instance 'grain-listing-traversal))
        (*use-cfasls* nil)
        (*grains-list* ())
        (*grains-table* (make-hash-table :test 'equal)))
    (dolist (spec specs)
      (build-command-for env spec))
    (reverse *grains-list*)))

;;; TODO: some magic to break circularities that are due to flattening conditionals.
(define-build-command-for :when ((env grain-listing-traversal) expression &rest dependencies)
  (declare (ignore expression))
  (build-command-for* env dependencies))
(define-build-command-for :cond ((env grain-listing-traversal) &rest cond-expressions)
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
  (remove-xcvb-from-build build))

(defparameter +list-files-option-spec+
  `(,@+multi-build-option-spec+
    ,@+source-registry-option-spec+
    ,@+verbosity-option-spec+
    (("long" #\l) :type boolean :optional t :documentation "long format")))

(defun list-files-command (&rest keys &key source-registry verbosity build debugging long)
  (declare (ignore source-registry verbosity debugging))
  (apply 'handle-global-options keys)
  (log-format 10 "Listing files for build ~S~%" build)
  ;; TODO: Put handle-case here to trap the error for a noncanonical fullname
  ;; the the user is likely to type one day by accident.
  (loop :for spec :in build
     :for (target bgrain) = (multiple-value-list (handle-target spec))
     :collect target :into targets
     :collect bgrain :into builds
     :finally
     (let* ((all-grains (list-grains targets))
	    (grains (remove-if-not
		     (lambda (grain)
		       (and (typep grain '(or lisp-module-grain source-grain))
			    (member (build-module-grain-for grain) builds)))
		     (remove-duplicates all-grains)))
	    (files (mapcar 'grain-pathname grains)))
       (format t (if long "(~{~S~^~%~})~%" "~{~A~%~}") files))))

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
      (dolist (grain (remove-if-not #'source-lisp-grain-p (list-grains (list target-dependency))))
        (log-format 5 "Removing module declaration from ~A" (grain-pathname grain))
        (remove-module-from-file (grain-pathname grain))))
    (log-format 5 "Deleting build file for ~A" (grain-pathname build))
    (delete-file (grain-pathname build))))
