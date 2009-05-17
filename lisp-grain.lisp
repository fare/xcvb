;;;;; Syntax and Semantics of Lisp grains, including BUILD.lisp files

(in-package :xcvb)

;;; Module forms

(defmacro module (&rest options)
  ;; Make sure that module declarations don't have an effect when compiled,
  ;; only when read by XCVB.
  (declare (ignore options))
  nil)

(defun parse-module-declaration (form &key path build-p)
  "Takes a module declaration FORM and returns a grain object for that module."
  (unless (module-form-p form)
    (error "Invalid or missing module declaration"))
  (destructuring-bind ((&rest keys) &rest extension-forms) (cdr form)
    (apply #'make-instance (if build-p 'build-grain 'lisp-grain)
           :pathname path :extension-forms extension-forms
           :computation nil
           keys)))

(defvar *target-system-features* nil)

(defun target-system-features ()
  (unless *target-system-features*
    (setf *target-system-features* (compute-target-system-features)))
  *target-system-features*)

(defun compute-target-system-features ()
  ;; TODO: extract and use the *features* of the TARGET system.
  ;; and mixin :xcvb-host-FOO for any feature FOO in the HOST system?
  ;;(cons :xcvb *features*))
  ;; In the meantime, just :xcvb to make things deterministic.
  (list :xcvb))

(defun read-first-file-form (filepath)
  "Reads the first form from the top of a file"
  (with-standard-io-syntax ()
    (let ((*features* (target-system-features))
	  (*package* (find-package :xcvb-user))
	  (*read-eval* nil))
      (with-open-file (in filepath)
        (read in)))))

(defun grain-from-file-declaration (path &key build-p)
  (parse-module-declaration (read-first-file-form path) :path path :build-p build-p))

(defun module-form-p (form)
  "Returns whether or not the given form is a valid xcvb:module form"
  (and (consp form)
       (eq (car form) 'xcvb:module)
       (listp (cdr form))
       (listp (cadr form))))


;;; Lisp Grains

(defgeneric handle-lisp-dependencies (grain))

(defmethod handle-lisp-dependencies ((grain lisp-grain))
  (unless (slot-boundp grain 'load-dependencies) ;; Only do it once
    (flet ((normalize (deps)
             (mapcar (lambda (dep) (normalize-dependency dep grain)) deps)))
      (with-slots (compile-depends-on load-depends-on depends-on
                   compile-dependencies load-dependencies) grain
        (let ((common-dependencies (normalize depends-on)))
          (setf compile-dependencies
                (append (mapcar #'compiled-dependency common-dependencies)
                        (normalize compile-depends-on)))
          (setf load-dependencies
                (append common-dependencies
                        (normalize load-depends-on))))))
    (handle-extension-forms grain))
  (values))

(defun handle-extension-forms (grain)
  (declare (ignore grain))
  ;; TODO: move that to extensions.lisp, etc.
  nil)

(defun make-grain-from-file (path &key build-p)
  "Takes a PATH to a lisp file, and returns the corresponding grain."
  ;;(format T "resolving ~@[build ~]: ~a~%" build-p path)
  (let ((grain (grain-from-file-declaration path :build-p build-p)))
    (compute-fullname grain)
    grain))

(defun %grain-from-relative-name (name build)
  (probe-file-grain
   (make-pathname
    :type "lisp"
    :defaults (merge-pathnames
               (portable-pathname-from-string name :allow-absolute nil)
               (grain-pathname build)))))

(defun grain-from-fullname (name)
  (setf name (canonicalize-fullname name))
  (or (registered-grain name)
      (loop :for prev = nil :then pos
            :for pos = (position #\/ name :from-end t :end prev)
            :while pos :do
              (let* ((prefix (subseq name 0 pos))
                     (build (registered-grain prefix)))
                (when (typep build 'build-grain)
                  (let ((it (%grain-from-relative-name (subseq name (1+ pos)) build)))
                    (when it
                      (setf (registered-grain name) it)
                      (return it)))))
            :finally (return nil))))

(defvar *build-grain* nil
  "Current BUILD.lisp grain being considered")

(defun grain-from-name (name &optional
                        (build-grain *build-grain*))
  "This function takes the name of a module, and the current build grain,
   and returns the correct module with that given name.
    * It first tries to find a grain with fullname <(fullname build-grain)/name>
    * Failing that, it tries name as a fullname"
    (or (and build-grain (grain-from-fullname (strcat (fullname build-grain) "/" name)))
        (grain-from-fullname name)
        (simply-error 'grain-not-found
                      "The grain with name ~S cannot be found" name)))

(defun build-pre-image-name (build-grain)
  (check-type build-grain build-grain)
  (let ((image (build-pre-image build-grain)))
    (etypecase image
      (string image)
      ((eql t) (merge-pathnames
                (portable-pathname-from-string (fullname build-grain))
                (portable-pathname-from-string "pre-image/"))))))


(defun build-image-name (build-grain)
  (check-type build-grain build-grain)
  (let ((image (build-image build-grain)))
    (etypecase image
      (null nil)
      (string image)
      ((eql t) (fullname build-grain)))))
