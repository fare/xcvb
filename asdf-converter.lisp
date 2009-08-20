#+xcvb (module (:depends-on ("lisp-grain")))

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
  (and (= (length (slot-value module 'compile-depends-on))
          (length (slot-value module 'load-depends-on)))
       (loop
         :for comp-dep :in (slot-value module 'compile-depends-on)
         :for load-dep :in (slot-value module 'load-depends-on)
         :always (or (equal comp-dep load-dep)
		     (and (consp comp-dep)
			  (eql (first comp-dep) :compile)
			  (null (rest (rest comp-dep)))
			  (if (consp load-dep)
			      (and (eql (first load-dep) :load)
				   (null (rest (rest load-dep)))
				   (equal (second load-dep) (second comp-dep)))
			      (equal load-dep (second comp-dep))))))))

(defun module-string (grain)
  "Returns a string representation of a module object that can be put at the
top of a source file"
  (with-output-to-string (out)
    (format out "#+xcvb (module (")
    (when (build-grain-p grain)
      (format out "~@[:fullname ~S~]" (subseq (fullname grain) 1)))
    (dolist (slot '(author maintainer version licence description long-description))
      (when (and (slot-boundp grain slot) (slot-value grain slot))
        (format out "~@[:~(~A~) ~S~]" slot (slot-value grain slot))))
    (with-slots (load-depends-on compile-depends-on depends-on) grain
      (if (equivalent-deps-p grain)
          (format out "~@[:depends-on (~{~15,0T~S~^ ~})~]" load-depends-on)
          (format out
                  "~@[:compile-depends-on ( ~{~15,0T~S~^ ~})~]~
                   ~@[:load-depends-on ( ~14,7T~{~15,0T~S~^ ~})~]"
                  compile-depends-on load-depends-on)))
    (when (build-grain-p grain)
      (dolist (slot '(build-depends-on supersedes-asdf))
	(when (and (slot-boundp grain slot) (slot-value grain slot))
	  (format out "~@[ :~(~A~) ~S~]" slot (slot-value grain slot)))))
    (format out ")")
    (format out "~@[ ~{~S~^ ~}~]" (and (slot-boundp grain 'extension-forms)
					      (grain-extension-forms grain)))
    (format out ")")))

(defun read-comment-header (in)
  "From Lisp file stream IN, read any header made of blanks and ;-comments
until something else is found, then return that header as a string"
  (with-output-to-string (out)
     (loop :for x = (peek-char nil in nil) :do
	(cond
	  ((member x '(#\space #\tab #\newline #\linefeed))
	   (princ (read-char in) out))
	  ((eql x #\;)
	   (write-line (read-line in) out))
	  (t (return))))))

(defun skip-whitespace (in)
  "From stream IN, read any number of whitespace until non-whitespace is found."
  (loop :while (member (peek-char nil in nil) '(#\space #\tab #\newline #-clisp #\linefeed))
        :do (read-char in)))

;; If the file does not have an existing XCVB module, a module is
;; created and inserted into the file.  If the file already has an
;; XCVB module, the file's module is replaced with new XCVB module,
;; keeping the old module's extension forms.  If 'module' argument is
;; NIL, then the file's XCVB module is removed.
(defun replace-module-in-file (filename module)
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
	    ;; Copy comment header to tmppath file.
	    (format out "~a" (read-comment-header in))
	    ;; Write out xcvb module to tmppath file.
	    (let* ((first-form-position (file-position in))
		   (first-form (read in nil)))
	      (if (not (module-form-p first-form))
		  (file-position in first-form-position)
		  (when module
		    (setf (grain-extension-forms module)
			  (grain-extension-forms
			   (parse-module-declaration
			    first-form
			    :path filename
			    :build-p (build-grain-p module))))))
	      (when module (format out "~a~%~%" (module-string module))))
	    (skip-whitespace in)
	    ;; Copy rest of file to tmppath file.
	    (do ((line (read-line in nil) (read-line in nil)))
		((null line))
	      (write-line line out))))
        (rename-file tmppath filename
                     #+clozure :if-exists #+clozure :rename-and-delete))
      ;; If module=NIL, we're removing xcvb module from file and file must exist
      (progn
	(assert module)
	(with-open-file
	    (out filename
		 :direction :output
		 :if-does-not-exist :create
		 :if-exists :supersede)
	  (format out "~a~%" (module-string module))))))

(defun remove-module-from-file (filename)
  (replace-module-in-file filename NIL))

(defun add-module-to-file (module &optional (filename (grain-pathname module)))
  (replace-module-in-file filename module))

;; See also http://paste.lisp.org/display/66610
(defun get-dependencies-from-components (components)
  "Returns a list of the files or systems that the asdf:component depends on"
  (let (dependencies)
    (dolist (component components)
      (let ((component-depends-on
	     (asdf:component-depends-on 'asdf:compile-op component)))
	(unless (null component-depends-on)
	  (destructuring-bind ((compile-op &rest deps)) component-depends-on
	    (declare (ignore compile-op))
	    (dolist (d deps)
	      (pushnew (coerce-asdf-system-name d) dependencies :test #'equal))))))
    (nreverse dependencies)))

(defun get-dependencies-from-component (component)
  (get-dependencies-from-components (list component)))


(defun get-build-grain-for-asdf-system (asdf-system original-systems)
  "Returns a build-grain with information from the given asdf system"
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
          (asdf-deps (get-dependencies-from-components original-systems))
          (file-deps (mapcar
                      (lambda (component)
                        (enough-namestring
                         (make-pathname
                          :type nil
                          :defaults (asdf:component-pathname component))
                         (asdf:component-pathname asdf-system)))
                      (asdf:module-components asdf-system))))
      (make-instance 'build-grain
        :fullname fullname
        :author author
        :maintainer maintainer
        :version version
        :description description
        :long-description long-description
        :licence licence
        :build-depends-on (mapcar (lambda (dep)
                                  (list :asdf (coerce-asdf-system-name dep)))
                                asdf-deps)
        :compile-depends-on (if *use-cfasls*
                              (mapcar (lambda (dep) (list :compile dep))
                                      file-deps)
                              file-deps)
        :load-depends-on file-deps
	:supersedes-asdf (mapcar #'asdf::coerce-name original-systems)
	:pathname (make-pathname
                   :name "build"
                   :type "xcvb"
                   :defaults (asdf:component-pathname asdf-system))))))

(defun dependency-sort (components system name-to-module
			traverse-order-components)
  "Sorts a list of asdf components according to their dependencies."
  (flet ((carname (x) (asdf:component-name (car x)))
         (nameop (x) (list (gethash x name-to-module))))
    (mapcar #'carname
            (asdf-dependency-grovel:components-in-traverse-order
             system (mapcar #'nameop components)
	     traverse-order-components))))

(defun get-module-for-component (asdf-component build-grain asdf-system name-to-module traverse-order-components)
  "Returns a module object for the file represented by the given asdf-component"
  (let ((comp-index (position asdf-component traverse-order-components))
	(traverse-order-names    ;; Should replace with hash-table lookup.
	 (mapcar #'coerce-asdf-system-name traverse-order-components)))
    (flet ((forward-ref-p (dep)
	     (let ((dep-index
		    (position-if (lambda (d) (equal dep d))
				 traverse-order-names)))
	       (> dep-index comp-index))))
      (let* ((backward-defs
	      (remove-if #'forward-ref-p
			 (get-dependencies-from-component asdf-component))))
	(let* ((dependencies
		(dependency-sort
		 backward-defs
		 asdf-system name-to-module
		 traverse-order-components))
	       (filepath (asdf:component-pathname asdf-component))
	       (fullname (strcat
			  (fullname build-grain)
			  "/"
			  (portable-pathname-output
			   (enough-namestring
			    (make-pathname :type nil :defaults filepath)
			    (grain-pathname build-grain))
			   :allow-absolute nil))))
    (let ((lisp-grain
	   (make-instance 'lisp-grain
	    :pathname filepath
	    :computation nil
	    :compile-depends-on (if *use-cfasls*
				    (mapcar (lambda (dep) (list :compile dep))
					    dependencies)
				    dependencies)
	    :load-depends-on dependencies)))
      (setf (fullname lisp-grain) fullname)
      lisp-grain))))))


(defvar *components-path* #p"/tmp/simplified-system-components.lisp-expr")

(defun name-to-module-map (system)
  (let ((name-to-module (make-hash-table :test 'equal)))
    (dolist (c (asdf:module-components system))
      (setf (gethash (asdf:component-name c) name-to-module) c))
    name-to-module))

(defun guess-base-pathname-for-systems (systems)
  (with-nesting ()
    (let ((first-system (first systems))))
    (progn (unless first-system
             (error "No system provided")))
    (let* ((first-system-name (asdf::coerce-name first-system))
           (sysdef-pathname (asdf::system-definition-pathname first-system-name))))
    (progn (unless sysdef-pathname
             (error "Could not find system :~A" first-system-name)))
    (pathname-directory-pathname (truename sysdef-pathname))))

(defun asdf-to-xcvb (&key
		     system
		     (systems (when system (list system)))
		     (simplified-system :simplified-system)
		     (base-pathname (guess-base-pathname-for-systems systems))
		     (components-path *components-path*)
		     systems-to-preload
		     verbose)

  "Takes the name of one or several ASDF system(s) and
merge them into a single XCVB build,
adding xcvb module declarations to the top of all the files in that build,
and writing a corresponding build.xcvb file,
so that the system can now be compiled with XCVB."
  ;; Remove any system used by XCVB so that asdf-to-xcvb may work on them.
  (dolist (sys systems)
    (remhash (asdf::coerce-name sys) asdf::*defined-systems*))
  (xcvb-driver:with-controlled-compiler-conditions ()
    (when verbose (DBG "preloading systems"))
    (dolist (sys systems-to-preload)
      (asdf:operate 'asdf:load-op sys))
    (setf cl-launch:*output-pathname-translations* nil)
    (eval
     `(asdf:defsystem ,simplified-system
	  :components ((asdf-dependency-grovel:component-file
			"simplified-system-components"
			:output-file ,components-path
			:base-asd-file nil
			:load-systems ,systems
			:merge-systems ,systems
			:base-pathname ,base-pathname
			:verbose ,verbose))))
    (when verbose (DBG "Starting the dependency grovelling"))
    (let ((asdf-dependency-grovel::*system-base-dir*
	   (cl-launch:apply-output-pathname-translations base-pathname)))
      (asdf:oos 'asdf-dependency-grovel:dependency-op simplified-system)))
  (when verbose (DBG "Adding dependency information to files"))
  (eval
   `(asdf:defsystem :migrated-system
     ,@(with-open-file (s (cl-launch:apply-output-pathname-translations
			   (merge-pathnames components-path)))
		       (cdar (read s)))))
  (let ((asdf-system (asdf:find-system :migrated-system)))
      (setf (slot-value asdf-system 'asdf::name) (asdf::coerce-name (first systems)))
      (setf (slot-value asdf-system 'asdf::relative-pathname) base-pathname)
      (let ((*default-pathname-defaults* base-pathname)
            (build-grain (get-build-grain-for-asdf-system
                          asdf-system (mapcar #'asdf:find-system systems)))
            (name-to-module (name-to-module-map asdf-system)) ; Precompute to ensure O(n) behavior
	    (traverse-order-components (asdf:module-components asdf-system)))
	(add-module-to-file build-grain)
	(when verbose (DBG "Added module to build.xcvb"))
	(dolist (component traverse-order-components)
	  (when verbose (DBG "Add module to component ~S~%" component))
	  (add-module-to-file (get-module-for-component
                               component build-grain asdf-system name-to-module
			       traverse-order-components))))))

;; Using the build.xcvb as a starting point, finds files and 
;; strips XCVB modules from them.
(defun remove-xcvb-from-build (&key xcvb-path verbosity build)
  (reset-variables)
  (when xcvb-path
    (set-search-path! xcvb-path))
  (when verbosity
    (setf *xcvb-verbosity* verbosity))
  (search-search-path)
  (let* ((build (registered-build (canonicalize-fullname build))))
    (with-slots (depends-on) build
      (mapcar (lambda (name)
		(let ((path (module-subpathname (grain-pathname build) name)))
		  (remove-module-from-file path)))
	   depends-on)))) 