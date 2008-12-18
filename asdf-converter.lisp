(in-package :xcvb)

(defun equivalent-deps-p (module)
  "This function takes a module and returns whether or not
its compile and load dependencies are in such a form that
:depends-on can be used to describe all of its compile and load dependencies,
instead of having to list the compile and load dependencies separately.
This happens when the compile and load dependencies
are all the same files, in the same order,
with the compile dependencies depending on compile-time effects of the files,
and the load dependencies depending on load-time effects of the files."
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
    (when (typep module 'build-module)
      (format out "~@[~%~7,0T:fullname ~s~]" (fullname module)))
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

(defun read-comment-header (in)
  "From Lisp file stream IN, read any header made of blanks and ;-comments
until something else is found, then return that header as a string"
  (with-output-to-string (out)
     (loop
	 (case (peek-char nil in nil)
	   ((#\space #\tab #\newline #\linefeed) (princ (read-char in) out))
	   ((#\;) (write-line (read-line in) out))
	   (t (return))))))

(defun skip-whitespace (in)
  "From stream IN, read any number of whitespace until non-whitespace is found."
  (loop while (member (peek-char nil in nil) '(#\space #\tab #\newline #\linefeed))
    do (read-char in)))


(defun add-module-to-file (module &optional (filename (filepath module)))
  "Adds a module form to the beginning of a file, replacing the existing module
form if there is one (but leaving the extension forms)."
  (if (probe-file filename)
    (let* ((tmppath (make-pathname
		     :type (strcat (pathname-type filename) ".xcvbtmp")
		     :defaults filename))
	   (*features* (cons :xcvb *features*)))
      (with-open-file (in filename :direction :input :if-does-not-exist nil)
        (with-open-file (out tmppath
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede)
	  (format out "~a" (read-comment-header in))
          (let* ((first-form-position (file-position in))
		 (first-form (read in nil)))
            (if (module-form-p first-form)
                (setf (extension-forms module)
                      (extension-forms
                       (parse-module first-form
                                     :build-module-p (typep module 'build-module))))
                (file-position in first-form-position))
	    (format out "#+xcvb~%~a~%~%" (module-string module)))
	  (skip-whitespace in)
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
  (let ((name-to-module (make-hash-table :test 'equal))) ; Precompute to avoid O(n^2) behavior
    (dolist (c (asdf:module-components system))          ; Should be lifted upwards.
      (setf (gethash (asdf:component-name c) name-to-module) c))
    (flet ((carname (x) (asdf:component-name (car x)))
	   (nameop (x) (list (gethash x name-to-module))))
    (mapcar #'carname
	    (asdf-dependency-grovel::components-in-traverse-order
	     system (mapcar #'nameop components))))))

(defun get-module-for-component (asdf-component build-module asdf-system)
  "Returns a module object for the file represented by the given asdf-component"
  (let* ((dependencies
          (dependency-sort (get-dependencies-from-component asdf-component)
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


(defvar *components-path* #p"/tmp/simplified-system-components.lisp-expr")

(defun asdf-to-xcvb (&key
		     system
		     (systems (when system (list system)))
		     (simplified-system :simplified-system)
		     (base-pathname
		      (pathname-directory-pathname
		       (truename
			(asdf::system-definition-pathname
			 (asdf::coerce-name (first systems))))))
		     (components-path *components-path*)
		     systems-to-preload)

  "Takes the name of one or several ASDF system(s) and
merge them into a single XCVB build,
adding xcvb module declarations to the top of all the files in that build,
and writing a corresponding BUILD.lisp file,
so that the system can now be compiled with XCVB."

  (dolist (sys systems-to-preload)
    (asdf:operate 'asdf:load-op sys))
  (eval
   `(asdf:defsystem ,simplified-system
     :components ((asdf-dependency-grovel:component-file
		   "simplified-system-components"
		   :output-file ,components-path
		   :base-asd-file nil
		   :load-systems ,systems
		   :merge-systems ,systems
		   :cull-redundant nil
		   :base-pathname ,base-pathname
		   :verbose nil))))
  (let ((asdf-dependency-grovel::*system-base-dir*
	 (cl-launch:apply-output-pathname-translations base-pathname)))
    (asdf:oos 'asdf-dependency-grovel:dependency-op simplified-system))
  (eval
   `(asdf:defsystem :migrated-system
     ,@(with-open-file (s (cl-launch:apply-output-pathname-translations
			   (merge-pathnames *components-path*)))
		       (cdar (read s)))))
  (let ((asdf-system (asdf:find-system :migrated-system)))
      (setf (slot-value asdf-system 'asdf::name) (asdf::coerce-name (first systems)))
      (setf (slot-value asdf-system 'asdf::relative-pathname) base-pathname)
      (let* ((*default-pathname-defaults* base-pathname)
	     (build-module (get-build-module-for-asdf-system asdf-system)))
	(add-module-to-file build-module)
	(dolist (component (asdf:module-components asdf-system))
	  (add-module-to-file (get-module-for-component component
							build-module
							asdf-system))))))
