(in-package :xcvb)


(defun module-string (module)
  "Returns a string representation of a module object that can be put at the 
top of a source file"
  (with-output-to-string (out)
    (format out "(xcvb:module (")
    (format out "~@[~%~7,0T:fullname ~s~]" (fullname module))
    (format out "~@[~%~7,0T:author ~s~]" (author module))
    (format out "~@[~%~7,0T:maintainer ~s~]" (maintainer module))
    (format out "~@[~%~7,0T:version ~s~]" (version module))
    (format out "~@[~%~7,0T:description ~s~]" (description module))
    (format out "~@[~%~7,0T:long-description ~s~]" (long-description module))
    (format out "~@[~%~7,0T:licence ~s~]" (licence module))
    (if (and (eql 
              (length (compile-depends-on module)) 
              (length (load-depends-on module)))
             (loop 
               for comp-dep in (compile-depends-on module)
               for load-dep in (load-depends-on module)
               always (and (listp comp-dep)
                           (eql (first comp-dep) :compile)
                           (null (rest (rest comp-dep)))
                           (equal load-dep (second comp-dep)))))
      (format out "~@[~%~7,0T:depends-on (~%~14,7T~{~15,0T~(~s~)~^~%~})~]"
              (load-depends-on module))
      (progn
        (format out 
                "~@[~%~7,0T:compile-depends-on (~%~{~15,0T~(~s~)~^~%~})~]" 
                (compile-depends-on module))
        (format out 
                "~@[~%~7,0T:load-depends-on (~%~14,7T~{~15,0T~(~s~)~^~%~})~]" 
                (load-depends-on module))))
    (format out ")")
    (if (and (typep module 'build-module) (build-requires module))
      (format out "~@[~%~7,0T(:set :this-module :build-requires ~(~s~))~]" 
              (build-requires module)))
    (format out "~@[~%~{~7,0T~(~s~)~^~%~}~]" (extension-forms module))
    (format out ")")))


(defun add-module-to-file (module &optional (filename (filepath module)))
  "Adds a module form to the beginning of a file, replacing the existing module
form if there is one (but leaving the extension forms)."
  (if (probe-file filename)
    (let ((tmppath (make-pathname 
                    :type (strcat (pathname-type filename) ".xcvbtmp") 
                    :defaults filename)))
      (rename-file filename tmppath)
      (with-open-file (in tmppath :direction :input :if-does-not-exist nil)
        (with-open-file (out filename 
                             :direction :output 
                             :if-does-not-exist :create 
                             :if-exists :supersede)
          (let ((first-form (read in nil)))
            (if (module-form-p first-form)
              (progn 
                (setf (extension-forms module) 
                      (extension-forms 
                       (parse-module first-form 
                                     :build-module-p (typep module 'build-module))))
                (format out "~a~%" (module-string module)))
              (progn
                (format out "~a~%" (module-string module))
                (file-position in :start))))
          (do ((line (read-line in nil) (read-line in nil)))
              ((null line))
            (write-line line out)))
        (delete-file in)))
    (with-open-file 
        (out filename 
             :direction :output 
             :if-does-not-exist :create 
             :if-exists :supersede)
      (format out "~a~%" (module-string module)))))


(defun get-dependencies-from-component (asdf-component)
  "Returns a list of the files or systems that the asdf:component depends on"
  (let ((component-depends-on 
         (asdf:component-depends-on 'asdf:compile-op asdf-component)))
    (unless (null component-depends-on)
      (destructuring-bind ((compile-op &rest deps)) component-depends-on
        (declare (ignore compile-op))
        deps))))


(defun get-build-module-for-asdf-system (asdf-system)
  "Returns a build-module with information from the given asdf system"
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
        (asdf-deps (get-dependencies-from-component asdf-system))
        (file-deps (mapcar 
                    (lambda (component) 
                      (enough-namestring 
                       (make-pathname :type nil 
                                      :defaults (asdf:component-pathname component)) 
                       (asdf:component-pathname asdf-system)))
                    (asdf:module-components asdf-system))))
    (make-instance 'build-module 
      :fullname fullname 
      :author author 
      :maintainer maintainer 
      :version version 
      :description description 
      :long-description long-description 
      :licence licence
      :build-requires (mapcar (lambda (dep) (list :asdf (symbol-name dep))) asdf-deps)
      :compile-depends-on (if *use-cfasls* 
                            (mapcar (lambda (dep) (list :compile dep)) file-deps) 
                            file-deps)
      :load-depends-on file-deps
      :filepath (make-pathname 
                 :name "BUILD" 
                 :type "lisp" 
                 :defaults (asdf:component-pathname asdf-system)))))

(defun dependency-sort (components system)
  "Sorts a list of asdf components according to their inter-dependencies between one 
another."
  (stable-sort components 
               (lambda (comp1 comp2) 
                 (member comp1 
                         (rest (first (asdf:component-depends-on 
                                       'asdf:compile-op 
                                       (asdf:find-component system comp2))))
                         :test #'equal))))
                      
(defun get-module-for-component (asdf-component build-module asdf-system)
  "Returns a module object for the file represented by the given asdf-component"
  (let* ((dependencies 
          (dependency-sort (reverse (get-dependencies-from-component asdf-component))
                           asdf-system))
         (filepath (asdf:component-pathname asdf-component))
         (fullname (strcat 
                    (fullname build-module) 
                    "/" 
                    (enough-namestring 
                     (make-pathname :type nil :defaults filepath) 
                     (filepath build-module)))))
    (make-instance 'concrete-module 
      :fullname fullname
      :filepath filepath
      :compile-depends-on (if *use-cfasls* 
                            (mapcar (lambda (dep) (list :compile dep)) dependencies) 
                            dependencies)
      :load-depends-on dependencies)))
    

(defun convert-asdf-system-to-xcvb (system-name)
  "Takes the name of an asdf system, and adds xcvb module declarations to the top of all
the files in that system, and also writes a corresponding BUILD.lisp file for that
system, so that the system can now be compiled with xcvb."
  (let* ((asdf-system (asdf:find-system system-name))
         (build-module (get-build-module-for-asdf-system asdf-system)))
    (add-module-to-file build-module)
    (dolist (component (asdf:module-components asdf-system))
      (let ((module (get-module-for-component component build-module asdf-system)))
        (add-module-to-file module)))))