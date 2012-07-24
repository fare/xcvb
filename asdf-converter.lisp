#+xcvb (module (:depends-on ("grain-interface" "commands" "source-registry")))

(in-package :xcvb)

(declaim (optimize (safety 3) (debug 3) (speed 1)))

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
		     (and (list-of-length-p 2 comp-dep)
			  (eq (first comp-dep) :compile)
                          (or (and (list-of-length-p 2 load-dep)
                                   (eq (first load-dep) :load)
				   (equal (second load-dep) (second comp-dep)))
			      (equal load-dep (second comp-dep))))))))

(defun module-form (grain)
  "Returns a sexp representation of a module object that can be put at the
top of a source file"
  (labels ((handle-slot (slot)
             (when (and (slot-boundp grain slot) (slot-value grain slot))
               `(,(keywordify slot) ,(slot-value grain slot))))
           (handle-slots (slots)
             (mapcan #'handle-slot slots)))
  `(module
    (,@(when (build-module-grain-p grain)
             `(:fullname ,(fullname grain)))
     ,@(handle-slots '(author maintainer version licence description long-description))
     ,@(with-slots (load-depends-on compile-depends-on depends-on) grain
         (if (equivalent-deps-p grain)
             (when load-depends-on `(:depends-on ,load-depends-on))
             `(:compile-depends-on ,compile-depends-on
               :load-depends-on load-depends-on)))
     ,@(when (build-module-grain-p grain) (handle-slots '(build-depends-on supersedes-asdf))))
    ,@(when (and (build-module-grain-p grain) (slot-boundp grain 'extension-forms))
            (grain-extension-forms grain)))))

(defparameter *module-pprint-dispatch*
  (let ((*print-pprint-dispatch* *print-pprint-dispatch*))
    ;;(set-pprint-dispatch 'module ...)
    *print-pprint-dispatch*))

(defun module-string (grain)
  "Returns a string representation of a module object that can be put at the
top of a source file"
  (with-safe-io-syntax (:package :xcvb)
    (let* ((form (module-form grain))
           (*print-pprint-dispatch* *module-pprint-dispatch*)
           (*print-case* :downcase)
           (*print-readably* nil)
           (*print-escape* t)
           (*print-pretty* nil)
           (short-string (format nil "~S" form))
           (l (length short-string)))
      (with-output-to-string (s)
        (princ "#+xcvb" s)
        (cond
          ((< l 73) (princ " " s) (princ short-string s))
          ((< l 80) (terpri s) (princ short-string s))
          (t (terpri s)
             (write form :stream s :pretty t :miser-width 79)))))))

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

(defun replace-module-in-file (filename module)
  "If the file does not have an existing XCVB module,
   a module is created and inserted into the file.
   If the file already has an XCVB module,
   the file's module is replaced with new XCVB module,
   keeping the old module's extension forms.
   If 'module' argument is NIL, then the file's XCVB module is removed."
  (cond
    ((probe-file filename)
     (let* ((tmppath (make-pathname
                      :type (strcat (pathname-type filename) "_xcvbtmp")
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
                  (first-form (let ((*read-eval* nil))
                                (ignore-errors (read in nil)))))
             (if (not (module-form-p first-form))
                 (file-position in first-form-position)
                 (when module
                   (setf (grain-extension-forms module)
                         (getf (rest
                                (parse-module-declaration
                                 first-form
                                 :keys `(:pathname ,filename)
                                 :build-p (build-module-grain-p module)))
                               :grain-extension-forms))))
             (when module (format out "~a~%~%" (module-string module))))
           (skip-whitespace in)
           ;; Copy rest of file to tmppath file.
           (do ((line (read-line in nil) (read-line in nil)))
               ((null line))
             (write-line line out))))
       (log-format 9 "Moving temporary file ~A to permanent file ~A" tmppath filename)
       (rename-file-overwriting-target tmppath filename)))
     (module
      (with-open-file (out filename
                       :direction :output :if-does-not-exist :create :if-exists :error)
        (format out "~a~%" (module-string module))))
     (t
      (error "Cannot remove module declaration from non-existing file ~S" filename))))

(defun remove-module-from-file (filename)
  (replace-module-in-file filename nil))

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
              ;; need to use enough-namestring wrt the build.xcvb directory!
              (pushnew d dependencies :test #'equal))))))
    (nreverse dependencies)))

(defun get-dependencies-from-component (component)
  (get-dependencies-from-components (list component)))

(defun get-build-module-grain-for-asdf-system
    (asdf-system original-systems asdf-deps
     original-traverse-order-map &key name)
  "Returns a build-module-grain with information from the given asdf system,
assuming said system is a simplified system as created by"
  (flet ((maybe-slot-value (object slot)
           (and (slot-boundp object slot) (slot-value object slot))))
    (let* ((fullname (or name (maybe-slot-value asdf-system 'asdf::name)))
           ;;(canonical-fullname (canonicalize-fullname fullname))
           (directory (pathname-directory-pathname (component-truename asdf-system)))
           (*default-pathname-defaults* directory)
           (author (maybe-slot-value asdf-system 'asdf::author))
           (maintainer (maybe-slot-value asdf-system 'asdf::maintainer))
           (version (maybe-slot-value asdf-system 'asdf::version))
           (description (maybe-slot-value asdf-system 'asdf::description))
           (long-description (maybe-slot-value asdf-system
                                               'asdf::long-description))
           (licence (maybe-slot-value asdf-system 'asdf::licence))
           (file-deps (mapcar
                       #'asdf-dependency-grovel::normalized-component-name
                       (dependency-sort (asdf:module-components asdf-system)
                                        original-traverse-order-map)))
           (build
            (make-instance 'build-module-grain
             :fullname fullname
             :author author
             :maintainer maintainer
             :version version
             :description description
             :long-description long-description
             :licence licence
             :build-depends-on (mapcar (lambda (dep) `(:asdf ,dep))
                                       (set-difference (mapcar #'coerce-asdf-system-name asdf-deps)
                                                       original-systems :test 'equal))
             :compile-depends-on (if *use-cfasls*
                                     (mapcar (lambda (dep) (list :compile dep))
                                             file-deps)
                                     file-deps)
             :load-depends-on file-deps
             :supersedes-asdf original-systems
             :pathname (make-pathname
                        :name "build"
                        :type "xcvb"
                        :defaults directory))))
      (setf (gethash build *truename-build-fullnames*) fullname)
      (register-build-named fullname build t)
      build)))

(defgeneric component-truename (x))

(defmethod component-truename ((x asdf:component))
  (truename (asdf:component-pathname x)))

(defmethod component-truename ((x asdf:system))
  (truename (asdf:system-definition-pathname x)))

(defun systems-traverse-order-map (systems &optional (traverse-type 'asdf:load-op))
  (let* ((systems (mapcar #'asdf:find-system systems))
         (op (make-instance traverse-type))
         (opspecs (mapcan (lambda (system) (asdf::traverse op system)) systems)))
    (sequence-position-map
     (mapcar 'component-truename (mapcar 'cdr opspecs)))))

(defun name-component-map (asdf-module)
  (sequence-function-map #'identity (asdf:module-components asdf-module)
                         :key #'asdf:component-name))

(defun component-position (component component-order-map)
  (gethash (component-truename component) component-order-map))

(defun dependency-sort (components component-order-map)
  "Sorts a list of asdf components according to their dependencies."
  (sort (copy-list components) #'< :key
        #'(lambda (x) (or (component-position x component-order-map) -1))))

(defun get-module-for-component (asdf-component build-module-grain
                                 name-component-map original-traverse-order-map)
  "Returns a module object for the file represented by the given asdf-component"
  (let* ((comp-position
          (component-position asdf-component original-traverse-order-map))
         (component-dependencies
          (get-dependencies-from-component asdf-component))
         (backward-deps
          (labels ((forward-dep-p (dep)
                     (> (component-position dep original-traverse-order-map)
                        comp-position))
                   (forward-dep-p* (dep)
                     (and (forward-dep-p dep)
                          (progn
                            (log-format 7 "Removing forward dependency from ~A to ~A"
                                        (component-truename asdf-component)
                                        (component-truename dep))
                            t))))
            (remove-if #'forward-dep-p*
                       (mapcar (lambda (x) (gethash x name-component-map))
                               component-dependencies))))
         (dependencies
          (mapcar #'asdf-dependency-grovel::normalized-component-name
                  (dependency-sort backward-deps original-traverse-order-map)))
         (compile-dependencies
          (if *use-cfasls*
              (mapcar (lambda (dep) `(:compile ,dep)) dependencies)
              dependencies))
         (filepath (component-truename asdf-component))
         (name (strcat
                (fullname build-module-grain)
                "/"
                (portable-pathname-output
                 (asdf-dependency-grovel::strip-extension
                  (enough-namestring
                   filepath
                   (grain-pathname build-module-grain))
                  "lisp")
                 :allow-absolute nil)))
         (around-compile (asdf-dependency-grovel::escaped-around-compile-hook asdf-component))
         (encoding (asdf:component-encoding asdf-component)))
    (make-instance 'lisp-file-grain
                   :fullname `(:lisp ,name)
                   :parent build-module-grain
                   :pathname filepath
                   :computation nil
                   :compile-depends-on compile-dependencies
                   :load-depends-on dependencies
                   :around-compile around-compile
		   :encoding encoding)))

(defvar *components-path* #p"simplified-system-components.lisp-expr")

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

(defun asdf-to-xcvb (&key name system (systems (when system (list system)))
		     (simplified-system :simplified-system) base-pathname
		     (components-path (merge-pathnames* *components-path* *temporary-directory*))
		     systems-to-preload verbose)
  "Takes the name of one or several ASDF system(s) and
merge them into a single XCVB build,
adding xcvb module declarations to the top of all the files in that build,
and writing a corresponding build.xcvb file,
so that the system can now be compiled with XCVB."
  (unless base-pathname
    (setf base-pathname (guess-base-pathname-for-systems systems)))
  (log-format 6 "Preloading systems")
  (xcvb-driver:with-controlled-compiler-conditions ()
    (map () 'asdf:load-system systems-to-preload))
  (setf systems (mapcar 'asdf::coerce-name systems)
        system (if system (asdf::coerce-name system) (car systems)))
  (log-format 6 "Remove any system possibly used by XCVB itself ~%~
		 so that asdf-to-xcvb may work on them.")
  (dolist (sys systems) (remhash sys asdf::*defined-systems*))
  (log-format 6 "Extract the original traverse order from ASDF before any transformation.")
  (let ((original-traverse-order-map (systems-traverse-order-map systems))
        (*load-pathname* base-pathname)
        (*default-pathname-defaults* (pathname-directory-pathname base-pathname)))
    (log-format 6 "Clear the system cache *again* because we'll re-define thing transformed.")
    (dolist (sys systems) (remhash sys asdf::*defined-systems*))
    (asdf:initialize-output-translations "/:")
    (asdf::do-defsystem simplified-system
      :pathname base-pathname
      :source-file base-pathname
      :components `((asdf-dependency-grovel:component-file
                     "simplified-system-components"
                     :output-file ,components-path
                     :base-asd-file ,base-pathname
                     :load-systems ,systems
                     :merge-systems ,systems
                     :base-pathname ,base-pathname
                     :verbose ,verbose)))
    (log-format 6 "Starting the dependency grovelling")
    (let ((asdf-dependency-grovel::*system-base-dir*
           (asdf:apply-output-translations base-pathname))
          (*features* (cons :grovel-dependencies *features*)))
      (with-standard-io-syntax
	(asdf:oos 'asdf-dependency-grovel:dependency-op simplified-system)))
    (log-format 6 "Adding dependency information to files")
    (let* ((original-asdf-deps
            (mapcar 'asdf::coerce-name
                    (get-dependencies-from-components (mapcar 'asdf:find-system systems))))
           (system-components
            (read-first-file-form
             components-path))
           (asdf-system
            (progn
              (log-format 6 "Creating a system with simplified dependencies")
              (asdf:clear-system :a2x-simplified-system)
              (asdf::do-defsystem :a2x-simplified-system
                :pathname base-pathname
                :source-file base-pathname
                :components
                (mapcan (lambda (x) (getf (cdr x) :components))
                        system-components))))
           (build-module-grain
            (get-build-module-grain-for-asdf-system
             asdf-system systems original-asdf-deps
             original-traverse-order-map :name (or name system)))
           (name-component-map (name-component-map asdf-system))) ; Precompute to ensure O(n) behavior
      (log-format 6 "Adding module to build.xcvb")
      (add-module-to-file build-module-grain)
      (log-format 6 "Adding module to each component")
      (dolist (component (asdf:module-components asdf-system))
        (when (typep component 'asdf:cl-source-file)
          (log-format 6 "Adding module to component ~S" component)
          (add-module-to-file (get-module-for-component
                               component build-module-grain
                               name-component-map original-traverse-order-map)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASDF to XCVB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command asdf-to-xcvb-command
    (("asdf-to-xcvb" "a2x")
     ()
     `((("system" #\a) :type string :optional nil :list t :documentation "Specify an ASDF system to convert (can be repeated)")
       (("base" #\B) :type string :optional t :documentation "Base pathname for the new build")
       (("name" #\n) :type string :optional t :documentation "name of the resulting build")
       ,@+setup-option-spec+
       ,@+source-registry-option-spec+
       (("preload" #\l) :type string :optional t :list t :documentation "Specify an ASDF system to preload (can be repeated)")
       ,@+verbosity-option-spec+)
     "Convert ASDF system to XCVB"
     "Attempt an automated conversion of an ASDF system to XCVB.
Optionally load a setup Lisp file before anything else, so the user gets
a chance to load and/or configure ASDF itself and any extension thereof.")
  (handle-global-options :verbosity verbosity :debugging debugging)
  (asdf:initialize-source-registry source-registry)
  (when setup (load setup))
  (asdf-to-xcvb
   :name name
   :systems (mapcar #'coerce-asdf-system-name system)
   :systems-to-preload (mapcar #'coerce-asdf-system-name preload)
   :base-pathname (when base (ensure-directory-pathname base))
   :verbose (and verbosity (> verbosity 5))))
