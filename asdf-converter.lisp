(in-package :xcvb)

(defun add-module-to-file (module-form filename)
  "Adds a module form to the beginning of a file, replacing the existing module form if there is one."
  (with-open-file (in (rename-file filename (make-pathname :type (strcat (pathname-type filename) ".xcvbtmp") :defaults filename)) :direction :input :if-does-not-exist :error)
    (with-open-file (out filename :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format out "~s~%" module-form)
      (let ((first-form (read in nil)))
        (unless (module-form-p first-form)
          (file-position in :start))
        (do ((line (read-line in nil) (read-line in nil)))
            ((null line))
          (write-line line out))))
    (delete-file in)))

(defun write-build-file (asdf-system)
  (with-open-file (out (make-pathname :name "BUILD" :type "lisp" :defaults (asdf:component-pathname asdf-system)) :direction :output :if-exists :supersede)
    (format out "(xcvb:module~%")
    (format out "~T:fullname ~s~%" (asdf:component-name asdf-system))
    (format out "~T:author ~s~%" (asdf:system-author asdf-system))
    (format out "~T:maintainer ~s~%" (asdf:system-maintainer asdf-system))
    (format out "~T:version ~s~%" (asdf:component-version asdf-system))
    (format out "~T:description ~s~%" (asdf:system-description asdf-system))
    (format out "~T:long-description ~s~%" (asdf:system-long-description asdf-system))
    (format out "~T:license ~s~%" (asdf:system-license asdf-system))
    ((slot-value asdf-system 'in-order-to)
;(defun replace-first-form (newform filename)
;  (with-open-file (out filename :direction output :if-exists :supersede


#|(defun convert-asdf-system-to-xcvb (asdf-system-name)
  (let ((asdf-system (asdf:find-system asdf-system-name)))
    (with-open-file (out (make-pathname :name "BUILD" :type "lisp" :defaults (asdf:component-pathname asdf-system)) :direction :output :if-exists :superscede)
      (write-module asdf-system out))))

(defun write-module (asdf-system filestream)
)|#
  

#|
(defclass asdf-system () 
  ((name :initarg :name :reader name :initform nil :documentation "The name of the asdf system.")
   (author :initarg :author :reader author :initform nil :documentation "The author of the system")
   (maintainer :initarg :maintainer :reader maintainer :initform nil :documentation "The maintainer(s) of the file")
   (licence :initarg :licence :reader licence :initform nil :documentation "The licence being used for the file")
   (description :initarg :description :reader description :initform nil :documentation "A short description of the file")
   (long-description :initarg :long-description :reader long-description :initform nil :documentation "A detailed description of the file")
   (depends-on :initarg :build-depends-on :initform nil :reader depends-on :documentation "A list of asdf systems that this system depends on")
   (components :initarg :components :initform nil :reader components :documentation "A list of files that make up this system")))


(defun get-asdf-system-from-file (filename)
  "Returns the first asdf:defsystem form out of the given file.  Throws an error if there is no asdf:defsystem form"
  (with-open-file (in filename :direction :input :if-does-not-exist :error)
    (do ((form (read in nil) (read in nil)))
        ((or (null form) (asdf-system-def-p form))
           (if (null form)
             (error "There is no asdf:defsystem form in the file")
             form)))))

(defun asdf-system-def-p (form)
  (destructuring-bind (system-decl &rest rest) form
    (declare (ignore rest))
    (if (eql system-decl 'asdf:defsystem)
      form
      nil)))
|#
#|
(defun parse-module (module)
  "Takes a module specifier and returns a module object representing that module.  Inherits licence, author, maintainer, description, and long-description slots from the build-module, if not specifically overwritten"
  (destructuring-bind (module-decl &key name fullname nickname origin licence author maintainer description long-description compile-depends-on load-depends-on build-depends-on) module
    (declare (ignore module-decl))
    (make-instance 'concrete-module
      :name name
      :fullname fullname
      :nickname nickname
      :origin origin
      :author (or author (if *build-module* (author *build-module*)))
      :maintainer (or maintainer (if *build-module* (maintainer *build-module*)))
      :licence (or licence (if *build-module* (licence *build-module*)))
      :description (or description (if *build-module* (description *build-module*)))
      :long-description (or long-description (if *build-module* (long-description *build-module*)))
      :compile-depends-on compile-depends-on
      :load-depends-on load-depends-on
      :build-depends-on build-depends-on)))

|#