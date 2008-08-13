(in-package :xcvb)

(defun read-first-file-form (filepath)
  "Reads the first form from the top of a file"
  (with-open-file (in filepath) (read in)))

(defun strcat (&rest strings)
  "String concatenation function"
  (apply 'concatenate 'string strings))

(defun pathname-parent (pathname)
  "Takes a pathname and returns the pathname of the parent directory of the directory of the given pathname"
  (cond
    ((null pathname) nil)
    ((equal (pathname-directory pathname) '(:absolute)) (make-pathname :directory '(:absolute)))
    (t (merge-pathnames (make-pathname :directory '(:relative :up)) (make-pathname :name nil :type nil :defaults pathname) nil))))

  #|(if (null pathname)
    nil
    (let ((dir (pathname-directory pathname)))
      (make-pathname :directory (subseq dir 0 (- (length dir) 1)) :name nil :type nil :defaults pathname))))|#


(defun make-fullname-absolute (module)
  "This function prepends a \"/\" to the beginning of the module's fullname, if there isn't one there already"
  (if (eql 0 (position #\/ (fullname module)))
    (fullname module)
    (setf (fullname module) (strcat "/" (fullname module)))))

(defun module-form-p (form)
  "Returns whether or not the given form is an xcvb:module form"
  (eql (first form) 'xcvb:module))
#|  (destructuring-bind (module-decl &rest rest) form
    (declare (ignore rest))
    (eql module-decl 'xcvb:module)))|#

;This condition is signaled by the find-build-file function if no BUILD.lisp file can be found
(define-condition no-build-file-found (simple-error)
  ())

;This condition is signaled if the dependency graph has any cycles in it.
(define-condition dependency-cycle (simple-error)
  ())

(defun top-level-name (name)
  "This function takes a name, and returns everything up to the first \"/\" in the name"
  (subseq name 0 (position #\/ (namestring name))))

#|(defun fasl-extension (&optional (lisp-implementation *lisp-implementation*))
  "Returns the correct file extension for a fasl based on the given lisp implementation"
  (fasl-extension-helper lisp-implementation))

(defgeneric fasl-extension-helper (lisp-impl)
  (:documentation "Helper generic function for fasl-extension function"))

(defmethod fasl-extension-helper ((lisp-impl (eql :sbcl)))
  "fasl")

(defmethod fasl-extension-helper ((lisp-impl (eql :ccl)))
  "lx64fsl")

(defun cfasl-extension (&optional (lisp-implementation *lisp-implementation*))
  "Returns the correct file extension for a cfasl based on the given lisp implementation"
  (cfasl-extension-helper lisp-implementation))

(defgeneric cfasl-extension-helper (lisp-impl)
  (:documentation "Helper generic function for cfasl-extension function"))

(defmethod cfasl-extension-helper ((lisp-impl (eql :sbcl)))
  "cfasl")

(defmethod cfasl-extension-helper ((lisp-impl (eql :ccl)))
  "cfasl");This is basically a placeholder for now so that cfasl nodes can still have a target in ccl.  In reality, ccl does not yet support cfasls.  If/when it does, this might have to be changed to return the correct extension used by ccl
|#


(defun quit-form (&key exit-status (lisp-implementation *lisp-implementation*))
  "Returns the correct form to quit lisp, based on the value of lisp-implementation.  Can optionally be given a unix status code to exit with"
  (quit-form-helper lisp-implementation :exit-status exit-status))

(defgeneric quit-form-helper (lisp-impl &key exit-status)
  (:documentation "Helper generic function for quit-form function"))

(defmethod quit-form-helper ((lisp-impl (eql :sbcl)) &key exit-status)
  (declare (ignore lisp-impl))
  (format nil "(sb-ext:quit~@[ :unix-status ~a~])" exit-status))

(defmethod quit-form-helper ((lisp-impl (eql :ccl)) &key exit-status)
  (declare (ignore lisp-impl))
  (format nil "(ccl:quit~@[ ~a~])" exit-status))


(defun save-image-form (filepath &optional (lisp-impl *lisp-implementation*))
  "Returns the lisp form to save the lisp image to the given filepath"
  (save-image-form-helper filepath lisp-impl))

(defgeneric save-image-form-helper (filepath lisp-impl)
  (:documentation "Helper generic function for save-image-form function"))

(defmethod save-image-form-helper (filepath (lisp-impl (eql :sbcl)))
  (declare (ignore lisp-impl))
  (format nil "(sb-ext:save-lisp-and-die \\\"~a\\\")" (namestring filepath)))

(defmethod save-image-form-helper (filepath (lisp-impl (eql :ccl)))
  (declare (ignore lisp-impl))
  (format nil "(ccl:save-application \\\"~a\\\")" (namestring filepath)))

(defun simply-error (simple-error control &rest args)
  (error (or simple-error 'simple-error)
         :format-control control :format-arguments args))
