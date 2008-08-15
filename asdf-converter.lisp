(in-package :xcvb)

(defun equivalent-deps-p (module)
  "This function takes a module and returns whether or not its compile and load 
dependencies are in such a form that :depends-on can be used to describe all of 
its compile and load dependencies, instead of having to list the compile 
depenencies and load dependencies separately.  This happens when the compile and
load dependencies depend on all the same files, in the same order, with the 
compile dependencies depending on compile-time effects of the files, and the 
load dependencies depending on load-time effects of the files."
  (and (= (length (compile-depends-on module))
          (length (load-depends-on module)))
       (loop
         for comp-dep in (compile-depends-on module)
         for load-dep in (load-depends-on module)
         always (and (listp comp-dep)
                     (eql (first comp-dep) :compile)
                     (null (rest (rest comp-dep)))
                     (if (listp load-dep)
                        (and (eql (first load-dep) :load)
                             (null (rest (rest load-dep)))
                             (equal (second load-dep) (second comp-dep)))
                        (equal load-dep (second comp-dep)))))))

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
    (cond 
      ((equivalent-deps-p module)
       (format out "~@[~%~7,0T:depends-on (~%~14,7T~{~15,0T~s~^~%~})~]"
               (load-depends-on module)))
      (t
       (format out
               "~@[~%~7,0T:compile-depends-on (~%~{~15,0T~s~^~%~})~]"
               (compile-depends-on module))
       (format out
               "~@[~%~7,0T:load-depends-on (~%~14,7T~{~15,0T~s~^~%~})~]"
               (load-depends-on module))))
    (format out ")")
    (if (and (typep module 'build-module) (build-requires module))
      (format out "~@[~%~7,0T(:set :this-module :build-requires ~s)~]"
              (build-requires module)))
    (format out "~@[~%~{~7,0T~s~^~%~}~]" (extension-forms module))
    (format out ")")))


(defun add-module-to-file (module &optional (filename (filepath module)))
  "Adds a module form to the beginning of a file, replacing the existing module
form if there is one (but leaving the extension forms)."
  (if (probe-file filename)
    (let ((tmppath (make-pathname
                    :type (strcat (pathname-type filename) ".xcvbtmp")
                    :defaults filename)))
      ;;TODO: maybe learn to skip over blank lines and comments,
      ;; so as to preserve any header information in the file
      (with-open-file (in filename :direction :input :if-does-not-exist nil)
        (with-open-file (out tmppath
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede)
          (let ((first-form (read in nil)))
            (if (module-form-p first-form)
              (progn
                (setf (extension-forms module)
                      (extension-forms
                       (parse-module first-form
                                     :build-module-p (typep module 
                                                            'build-module))))
                (format out "~a~%" (module-string module)))
              (progn
                (format out "~a~%" (module-string module))
                (file-position in :start))))
          (do ((line (read-line in nil) (read-line in nil)))
              ((null line))
            (write-line line out))))
        (rename-file tmppath filename 
                     #+clozure :if-exists #+clozure :rename-and-delete))
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
  (flet ((maybe-slot-value (object slot)
           (if (slot-boundp object slot)
             (slot-value object slot))))
    (let ((fullname (maybe-slot-value asdf-system 'asdf::name))
          (author (maybe-slot-value asdf-system 'asdf::author))
          (maintainer (maybe-slot-value asdf-system 'asdf::maintainer))
          (version (maybe-slot-value asdf-system 'asdf::version))
          (description (maybe-slot-value asdf-system 'asdf::description))
          (long-description (maybe-slot-value asdf-system 
                                              'asdf::long-description))
          (licence (maybe-slot-value asdf-system 'asdf::licence))
          (asdf-deps (get-dependencies-from-component asdf-system))
          (file-deps (mapcar
                      (lambda (component)
                        (enough-namestring
                         (make-pathname 
                          :type nil
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
        :build-requires (mapcar (lambda (dep) 
                                  (list :asdf (coerce-asdf-system-name dep)))
                                asdf-deps)
        :compile-depends-on (if *use-cfasls*
                              (mapcar (lambda (dep) (list :compile dep)) 
                                      file-deps)
                              file-deps)
        :load-depends-on file-deps
        :filepath (make-pathname
                   :name "BUILD"
                   :type "lisp"
                   :defaults (asdf:component-pathname asdf-system))))))

(defun dependency-sort (components system)
  "Sorts a list of asdf components according to their dependencies."
  ;;TODO: is this guaranteed to work considering the comparison function
  ;;is a partial function? Or do we need a more elaborate topological sort?
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
          (dependency-sort (reverse 
                            (get-dependencies-from-component asdf-component))
                           asdf-system))
         (filepath (asdf:component-pathname asdf-component))
         (fullname (strcat ;NUN
                    (fullname build-module)
                    "/"
                    (enough-namestring
                     (make-pathname :type nil :defaults filepath)
                     (filepath build-module)))))
    (make-instance 'standard-module
      :fullname fullname
      :filepath filepath
      :build-module build-module
      :compile-depends-on (if *use-cfasls*
                            (mapcar (lambda (dep) (list :compile dep)) 
                                    dependencies)
                            dependencies)
      :load-depends-on dependencies)))


(defun convert-asdf-system-to-xcvb (system-name)
  "Takes the name of an asdf system, and adds xcvb module declarations to the
top of all the files in that system, and also writes a corresponding BUILD.lisp
file for that system, so that the system can now be compiled with xcvb."
  (let* ((asdf-system (asdf:find-system system-name))
         (build-module (get-build-module-for-asdf-system asdf-system)))
    (add-module-to-file build-module)
    (dolist (component (asdf:module-components asdf-system))
      (add-module-to-file (get-module-for-component component 
                                                    build-module 
                                                    asdf-system)))))
