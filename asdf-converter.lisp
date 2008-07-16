(in-package :xcvb)

(defun add-module-to-file (module filename)
  "Adds a module form to the beginning of a file, replacing the existing module form if there is one."
  (if (probe-file filename)
    (let ((tmppath (make-pathname :type (strcat (pathname-type filename) ".xcvbtmp") :defaults filename)))
      (rename-file filename tmppath)
      (with-open-file (in tmppath :direction :input :if-does-not-exist nil)
        (with-open-file (out filename :direction :output :if-does-not-exist :create :if-exists :supersede)
          (let ((first-form (read in nil)))
            (if (module-form-p first-form)
              (progn 
                (setf (extension-forms module) 
                      (extension-forms (parse-module first-form :build-module-p (typep module 'build-module))))
                (format out "~a~%" (module-string module)))
              (progn
                (format out "~a~%" (module-string module))
                (file-position in :start))))
          (do ((line (read-line in nil) (read-line in nil)))
                    ((null line))
                  (write-line line out)))
        (delete-file in)))
    (with-open-file (out filename :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format out "~a~%" (module-string module)))))

(defun build-module-for-asdf-system (asdf-system)
  (let ((fullname (if (slot-boundp asdf-system 'asdf::name)
                    (slot-value asdf-system 'asdf::name)))
        (author (if (slot-boundp asdf-system 'asdf::author)
                  (slot-value asdf-system 'asdf::author)))
        (maintainer (if (slot-boundp asdf-system 'asdf::maintainer)
                      (slot-value asdf-system 'asdf::maintainer)))
        (version (if (slot-boundp asdf-system 'asdf::version)
                   (slot-value asdf-system 'asdf:version)))
        (description (if (slot-boundp asdf-system 'asdf::description)
                       (slot-value asdf-system 'asdf::description)))
        (long-description (if (slot-boundp asdf-system 'asdf::long-description)
                            (slot-value asdf-system 'asdf::long-description)))
        (licence (if (slot-boundp asdf-system 'asdf::licence)
                   (slot-value asdf-system 'asdf::licence)))
        (build-asdf-deps (if (slot-boundp asdf-system 'asdf::in-order-to)
                           (destructuring-bind 
                                 ((load-op1 (load-op2 &rest deps)) &rest rest)
                               (slot-value asdf-system 'asdf::in-order-to)
                             (declare (ignore load-op1 load-op2 rest))
                             deps)))
        (build-file-deps (mapcar 
                          (lambda (component) (enough-namestring (make-pathname :type nil :defaults (asdf:component-pathname component)) (asdf:component-pathname asdf-system)))
                          (asdf:module-components asdf-system))))
    (make-instance 'build-module 
      :name fullname 
      :fullname fullname 
      :author author 
      :maintainer maintainer 
      :version version 
      :description description 
      :long-description long-description 
      :licence licence
      :build-requires (mapcar (lambda (dep) (list :asdf (symbol-name dep))) build-asdf-deps)
      :compile-depends-on build-file-deps
      :load-depends-on build-file-deps
      :filepath (asdf:component-pathname asdf-system))))

(defun module-string (module)
  (with-output-to-string (out)
    (format out "(xcvb:module (")
    (if (fullname module)
      (format out "~%~7,0T:fullname ~s" (fullname module)))
    (if (author module)
      (format out "~%~7,0T:author ~s" (author module)))
    (if (maintainer module)
      (format out "~%~7,0T:maintainer ~s" (maintainer module)))
    (if (version module)
      (format out "~%~7,0T:version ~s" (version module)))
    (if (description module)
      (format out "~%~7,0T:description ~s" (description module)))
    (if (long-description module)
      (format out "~%~7,0T:long-description ~s" (long-description module)))
    (if (licence module)
      (format out "~%~7,0T:licence ~s" (licence module)))
    (if (compile-depends-on module)
      (format out "~%~7,0T:compile-depends-on (~%~{~15,0T~(~s~)~^~%~})" (compile-depends-on module)))
    (if (load-depends-on module)
      (format out "~%~7,0T:load-depends-on (~%~14,7T~{~15,0T~(~s~)~^~%~})" (load-depends-on module)))
    (format out ")")
    (if (and (typep module 'build-module) (build-requires module))
      (format out "~%~7,0T(:set :this-module :build-requires ~(~s~))" (build-requires module)))
    (if (extension-forms module)
      (format out "~%~{~7,0T~(~s~)~^~%~}" (extension-forms module)))
    (format out ")")))

(defun get-dependencies-from-component (asdf-component)
  (let ((in-order-to (slot-value asdf-component 'asdf::in-order-to)))
    (if in-order-to
      (destructuring-bind ((load-op1 (load-op2 &rest deps)) &rest rest)
          in-order-to
        (declare (ignore load-op1 load-op2 rest))
        deps))))
        ;(remove-duplicates deps :test #'equal)))))
    
(defun add-module-to-component (asdf-component build-module)
  (let* ((dependencies (get-dependencies-from-component asdf-component))
        (filepath (asdf:component-pathname asdf-component))
        (fullname (strcat (fullname build-module) "/" (enough-namestring (make-pathname :type nil :defaults filepath) (filepath build-module)))))
    (add-module-to-file (make-instance 'concrete-module 
                          :name (asdf:component-name asdf-component)
                          :fullname fullname
                          :filepath filepath
                          :compile-depends-on dependencies
                          :load-depends-on dependencies)
                        filepath)))
    

(defun convert-asdf-system-to-xcvb (system-name)
  (let* ((asdf-system (asdf:find-system system-name))
         (build-module (build-module-for-asdf-system asdf-system)))
    (add-module-to-file build-module (make-pathname :name "BUILD" :type "lisp" :defaults (asdf:component-pathname asdf-system)))
    (mapcar (lambda (component) (add-module-to-component component build-module)) (asdf:module-components asdf-system))))
