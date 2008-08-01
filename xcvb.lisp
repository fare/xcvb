(in-package :xcvb)

;; TODO: document the non-portable uses of namestrings
;; as being unix namestrings. Choose a three-letter acronym, eg
;; NUN (non-portable unix namestring) to tuck as a comment like this
;;   (enough-namestring fullname1 fullname2) ;NUN
;; Document that in a documentation file for developers
;; Have two main documents: one for users, one for developers

;; TODO: make it 80-column friendly

(defparameter *module-map* nil
  "Map of all module objects made thus far to
   prevent recreating the same module twice")
(defparameter *node-map* nil
  "Map of all the nodes created thus far to
   prevent redundand nodes in the dependency graph")
(defparameter *build-module* nil
  "Module object for the nearest surrounding BUILD.lisp file")
(defparameter *buildpath* nil
  "Filesystem path for the nearest surrounding BUILD.lisp file")

(defmacro module (&rest options)
  (declare (ignore options))
  nil)

(defclass module () ())

(defclass concrete-module (module)
  ((name
    :initarg :name :reader name :initform nil
    :documentation "The name of the module.
Will usually be the same as the name of the file")
   (fullname
    :initarg :fullname :accessor fullname :initform nil
    :documentation "The full name of the module.
This is found from the fullname of his module's build.lisp file")
   (nickname
    :initarg :nickname :reader nickname :initform nil
    :documentation "A short name to be used to refer to the module")
   (author
    :initarg :author :reader author :initform nil
    :documentation "The author of the file")
   (maintainer
    :initarg :maintainer :reader maintainer :initform nil
    :documentation "The maintainer(s) of the file")
   (version
    :initarg :version :reader version :initform nil
    :documentation "The version number of the file")
   (licence
    :initarg :licence :reader licence :initform nil
    :documentation "The licence being used for the file")
   (description
    :initarg :description :reader description :initform nil
    :documentation "A short description of the file")
   (long-description
    :initarg :long-description :reader long-description
    :initform nil :documentation "A detailed description of the file")
   (compile-depends-on
    :initarg :compile-depends-on :initform nil :accessor compile-depends-on
    :documentation "A list of dependencies that must be loaded before this file can be compiled")
   (load-depends-on
    :initarg :load-depends-on :initform nil :accessor load-depends-on
    :documentation "A list of dependencies that must be loaded before this file can be loaded")
   (filepath
    :initarg :filepath :accessor filepath
    :documentation "The absolute path to the file that the module was declared in")
   (filename
    :initarg :filename :accessor filename
    :documentation "The filename of the file that the module was declared in")
   (extension-forms
    :initarg :extension-forms :initform nil :accessor extension-forms
    :documentation "extension forms!")))

(defclass build-module (concrete-module)
  ((build-requires
    :initarg :build-requires :accessor build-requires :initform nil
    :documentation "A list of dependencies that apply to all files in the system specified by this BUILD.lisp file.  These dependencies will be loaded first thing into an image that will be used for all future compile/load operations")))



;; TODO: rename to REGISTER-MODULE
(defun add-to-module-map (module)
  "Adds the given module object to the hashtable containing all the modules thus far"
  (unless (null (nickname module))
    (setf (gethash (nickname module) *module-map*) module))
  (setf (gethash (make-fullname-absolute module) *module-map*) module))


;; TODO: call parse-module earlier
;; TODO: wherever you throw errors, have a define-condition
;; (a) can inherit from simple-error, use simply-error
(defun get-module-form-from-file (filename)
  "Returns the module form out of the top of the given file.
   Throws an error if there is no module form,
   or it is not at the top of the file"
  (let ((form (read-first-file-form filename)))
    (unless (module-form-p form)
      (error "Missing module declaration"))
    form))

;; TODO: only destructure once
(defun parse-module (module &key build-module-p)
  "Takes a module declaration form and returns a module object representing
  that module.  Inherits licence, author, and maintainer slots
  from the build-module, if not specifically overwritten"
  (destructuring-bind (module-decl (&key name fullname nickname licence version author maintainer description long-description compile-depends-on load-depends-on) &rest extension-forms) module
    (declare (ignore module-decl))
    (let ((module
           (make-instance (if build-module-p 'build-module 'concrete-module)
             :name name
             :fullname fullname
             :nickname nickname
             :author (or author (if *build-module* (author *build-module*)))
             :maintainer (or maintainer (if *build-module* (maintainer *build-module*)))
             :licence (or licence (if *build-module* (licence *build-module*)))
             :version (or version (if *build-module* (version *build-module*)))
             :description description
             :long-description long-description
             :compile-depends-on compile-depends-on
             :load-depends-on load-depends-on
             :extension-forms extension-forms)))
      module)))


;; TODO: rename into MODULE-FROM-PATH ?
(defun resolve-module (module-path &key parent-module build-module-p)
  "Takes a filepath to a lisp file, and returns the module object
 represented by the module specifer at the top of that lisp file.
 If the argument parent-module is supplied, then the new module
 will be given a fullname relative to the fullname of parent-module
 (if the fullname isn't already specified)"
  ;;(format T "resolving ~:[module~;build-module~]: ~a~%" build-module-p module-path)
  (when (and *build-module* (equal (pathname (filepath *build-module*)) (pathname module-path)))
    ;;(format T "using build-module~%")
    (return-from resolve-module *build-module*))
  (let ((module (parse-module (get-module-form-from-file module-path) :build-module-p build-module-p)))
    (setf (filepath module) module-path)
    (setf (filename module) (file-namestring module-path))
    (if (and parent-module (not (fullname module)))
      (setf (fullname module)
            (namestring (merge-pathnames
                         (enough-namestring (filepath module)
                                            (filepath parent-module))
                         (fullname parent-module))))
      ;;(strcat (fullname parent-module) "/" (enough-namestring (filepath module) (filepath parent-module))))
      (setf (fullname module)
            ;;(namestring (make-pathname :name (pathname-name (filepath module)) :type (pathname-type (filepath module)) :defaults (fullname module)))))
            (strcat (fullname module) "/" (file-namestring (filepath module)))))
    (handle-extension-forms module)
    (add-to-module-map module)))

;; TODO: rename to LOOKUP-MODULE-IN-FILESYSTEM ? (gah)
(defun create-module (name parent-module)
  "Takes a name of a new module, and the module whose fullname this new module
   will be under, and gets the filepath of the parent module and tries to find
   a file for the new module under that filepath.  If it finds one, then it
   creates that module out of the module declaration at the top of that file,
   if not, it returns nil"
  (let ((source-file-pathname
         (make-pathname
          :type "lisp"
          :defaults (merge-pathnames name (filepath parent-module)))))
    (when (probe-file source-file-pathname)
      (resolve-module source-file-pathname :parent-module parent-module))))

;; rename into MODULE-FROM-NAME ?
(defun get-module (name &optional (build-module *build-module*))
  "This function takes the name of module, and the current build module,
   and returns the correct module with that given name.
    * It starts by checking
      if there already is a module with the fullname of
      <(fullname build-module)/name>.
    * If not, then it gets the filepath of the build module, and looks for
      a file for the module under the filepath of the build module.
    * If there is no such file, then it assumes that the top-level-name
      of the given name must be global, and so looks up that global name,
      and if found, looks for a file for the module under the filepath of
      the module of top-level-name.
    * Finally, if that doesn't exist, it tries to find the module in the
      registry (currently just throws an error)"
  (or (gethash (strcat (fullname build-module) "/" name) *module-map*)
      (create-module name build-module)
      (let ((parent-module (gethash (top-level-name name) *module-map*)))
        (if parent-module
          (or (create-module name parent-module)
              (lookup-in-registry name build-module))))
      (simply-error ()
                    "The module with name \"~a\" cannot be found" name)))


(defun lookup-in-registry (module-name build-module)
  (declare (ignore module-name build-module))
  (error "The registry has not been implemented yet"))


;; TODO: move the setf'ing of *build-module* to the callers.
(defun find-build-file (source-filepath)
  "Looks for the first BUILD.lisp file in the given directory or any
of its parent directories that contains a fullname and returns its filepath.
It also sets the special variable *build-module* to the module corresponding
to build file that was found.
Throws an error if no BUILD.lisp file with a fullname is found"
  (if (eql source-filepath nil)
    (error 'no-build-file-found
           :format-control "no BUILD.lisp file found"))
  (let ((build-filepath (make-pathname :name "BUILD" :type "lisp" :defaults source-filepath)))
    (if (probe-file build-filepath)
      (let ((build-module (resolve-module build-filepath :build-module-p T)))
        (when (fullname build-module)
          (make-fullname-absolute build-module)
          (setf *build-module* build-module)
          (add-to-module-map build-module)
          (return-from find-build-file build-filepath))))
    (find-build-file (pathname-parent source-filepath))))


;;This class represents a node in the graph of dependencies
(defclass dependency-graph-node ()
  ((fullname
    :initarg :fullname :initform nil :accessor fullname
    :documentation "abstract name for the intention in the node,
e.g. //xcvb-test/foo/bar/quux.fasl") ;TODO
   (target
    :initarg :target :reader target
    :documentation "name of the target in the Makefile,
e.g. foo/bar/quux.fasl")
   (name ;;TODO: move it out, not used (except for subclass asdf-node)
    :initarg :name :reader name)
   (long-name
    :initarg :long-name :reader long-name
    :documentation "(NOT USED YET) A description of the whole computation
leading to this node from elementary parts, e.g.
(:compile \"//foo/quux.lisp\" (:load (:compile \"//foo/macros.lisp\") ...))")
   (hash-value
    :initarg :hash-value :reader hash-value
    :documentation "(NOT USED YET)
A crypto hash of the contents of the node, or else the long-hash-name (???)")
   (long-hash-name
    :initarg :long-hash-name :reader long-hash-name
    :documentation "(NOT USED YET) A crypto hash of the computation
leading to this node from other nodes with crypto hash values, e.g.
(h`(:compile ,hv1 (:load ,hv2) ...))")))


(defclass dependency-graph-node-with-dependencies (dependency-graph-node)
  ((compile-dependencies
    :initarg :compile-dependencies :initform nil :accessor compile-dependencies)
   (load-dependencies
    :initarg :load-dependencies :initform nil :accessor load-dependencies)))

(defclass lisp-node (dependency-graph-node-with-dependencies) ())

(defclass source-file-node (dependency-graph-node) ())

;; TODO: rename to object-file-node ?
(defclass fasl-or-cfasl-node (dependency-graph-node-with-dependencies) ())

(defclass fasl-node (fasl-or-cfasl-node) ())

(defclass cfasl-node (fasl-or-cfasl-node) ())

(defclass image-dump-node (dependency-graph-node)
  ((lisp-image :initarg :lisp-image :initform nil :reader lisp-image)))

(defclass asdf-system-node (dependency-graph-node) ())

(defgeneric add-dependency (node dependency &key type)
  (:documentation "Adds a dependency to a node in the dependency graph."))

(defmethod add-dependency ((node dependency-graph-node-with-dependencies) (dependency dependency-graph-node) &key type)
  (case type
    (:load (pushnew dependency (load-dependencies node)))
    (:compile (pushnew dependency (compile-dependencies node)))
    (otherwise (error "Invalid type of dependency.  Must be either :compile or :load"))))


(defun add-dependencies (node dependency-list &key type)
  "Adds a list of dependencies to a node in the dependency graph."
  (dolist (dep dependency-list)
    (add-dependency node dep :type type)))

(defun create-dependency-node (dependency previous-nodes-map previous-nodes-list)
  "Takes a form for a dependency, and builds the correct type of dependency-graph-node.  It also needs the map of previous nodes and the list of previous nodes to pass on to the functions that actually create the nodes of the right type, so that they can detect and handle dependency cycles properly."
  (if (typep dependency 'list)
    (destructuring-bind (dep-type dep) dependency
      (create-dependency-node-from-type dep-type dep previous-nodes-map previous-nodes-list))
    (create-fasl-node (get-module dependency) previous-nodes-map previous-nodes-list)))

(defgeneric create-dependency-node-from-type (dependency-type dependency-name previous-nodes-map previous-nodes-list)
  (:documentation "Takes a symbol specifying the type of the dependency and a string specifying its name, and creates the dependency-graph-node of the proper type.  It also needs the map of previous nodes and the list of previous nodes to pass on to the functions that actually create the nodes of the right type, so that they can detect and handle dependency cycles properly."))

(defmethod create-dependency-node-from-type ((dependency-type (eql :compile)) dependency-name previous-nodes-map previous-nodes-list)
  (create-cfasl-node (get-module dependency-name) previous-nodes-map previous-nodes-list))

(defmethod create-dependency-node-from-type ((dependency-type (eql :asdf)) dependency-name previous-nodes-map previous-nodes-list)
  (create-asdf-system-node dependency-name))
       
       
;; TODO: document better when you need to use this dependency?
;; = have a root node for the dependency DAG
;; maybe get rid of it? or rename it as root node of the whole graph
;; (why need a root instead of say iterating on the DAG?)
(defun create-lisp-node (dependencies)
  "This function constructs a lisp-node in the dependency graph
for a lisp with the given dependencies loaded"
  (let ((lisp-node (make-instance 'lisp-node :target "lisp" :fullname "lisp"))
        (previous-nodes-map (make-hash-table :test #'equal))
        (previous-nodes-list nil))
    (flet ((f (dep)
             (typecase dep
               (dependency-graph-node dep)
               (module
                  (create-fasl-node dep previous-nodes-map previous-nodes-list))
               (otherwise
                  (create-dependency-node
                   dep previous-nodes-map previous-nodes-list)))))
      (add-dependencies lisp-node (mapcar #'f dependencies) :type :compile)
    lisp-node)))


(defun create-source-file-node (module)
  "This function constructs a source-file-node in the dependency graph"
  (let ((fullname
         (namestring (make-pathname :type "lisp" :defaults (pathname (fullname module)))))) ;NUN - TODO: shouldn't (fullname module) already be the right type?
    (or (gethash fullname *node-map*);If this node already exists, don't recreate it
        (let* ((target (enough-namestring (make-pathname :type "lisp" :defaults (filepath module)) *buildpath*));Target is the filepath of the source file relative to the BUILD.lisp file ; NUN
               (source-node (make-instance 'source-file-node
                              :name (namestring (make-pathname :type nil :defaults (pathname target)))
                              :fullname fullname
                              :target target)))
          (setf (gethash fullname *node-map*) source-node)))))

(defun create-asdf-system-node (system-name)
  "This function constructs a asdf-system-node in the dependency graph"
  (let ((fullname (strcat "//asdf/" system-name)))
    (or (gethash fullname *node-map*);If this node already exists, don't recreate it
        (setf (gethash fullname *node-map*)
              (make-instance 'asdf-system-node :name system-name :target fullname :fullname fullname)))))


(defun create-image-dump-node (lisp-node dump-path)
  "This function constructs an image-dump-node in the dependency graph, which is designed to dump an image of the lisp state described by lisp-node"
  (make-instance 'image-dump-node
    :target (enough-namestring dump-path *buildpath*)
    :lisp-image lisp-node
    :fullname (format nil "image-dump:~a" dump-path)))


(defun call-while-catching-dependency-cycle (fullname previous-nodes-map previous-nodes-list thunk)
  "This macro wraps a body of code with code to detect and handle a dependency cycle"
  (when (nth-value 1 (gethash fullname previous-nodes-map))
    (error 'dependency-cycle
           :format-control "Dependency cycle found: ~s"
           :format-arguments (list (cons fullname previous-nodes-list))))
  (when (and (string-equal (pathname-type fullname) (fasl-extension))
             (nth-value 1 (gethash (make-pathname :type (cfasl-extension) :defaults fullname) previous-nodes-map)))
    (error 'dependency-cycle
            :format-control "Dependency cycle found, compiling a file cannot depend on loading it. ~s"
            :format-arguments (list (cons fullname previous-nodes-list))))
  (setf (gethash fullname previous-nodes-map) nil) ;Add node to map of previous nodes to detect dependency cycles
  (prog1
      (funcall thunk (cons fullname previous-nodes-list)) ;Add node to list of previous nodes so that if dependency cycle is found, the cycle can be printed
    (remhash fullname previous-nodes-map)));remove this node from the map of previous nodes

(defmacro with-catching-dependency-cycle ((fullname previous-nodes-map previous-nodes-list) &body body)
  "This macro wraps a body of code with code to detect and handle a dependency cycle"
  `(call-while-catching-dependency-cycle ,fullname ,previous-nodes-map ,previous-nodes-list
                                         (lambda (,previous-nodes-list) ,@body)))


(defun set-fasl-node-dependencies (fasl-node module previous-nodes-map previous-nodes-list)
   "This function takes a fasl-node and its module and builds nodes for all of that module's dependencies, and adds them as dependencies of the fasl-node"
  (add-dependencies fasl-node (mapcar
                               (lambda (name) (create-dependency-node
                                               name
                                               previous-nodes-map
                                               previous-nodes-list))
                               (load-depends-on module)) :type :load)
  (add-dependencies fasl-node (mapcar
                               (lambda (name) (create-dependency-node
                                               name
                                               previous-nodes-map
                                               previous-nodes-list))
                               (compile-depends-on module)) :type :compile)
  (add-dependency fasl-node (create-source-file-node module) :type :compile));Add dependency on the lisp source file

(defun set-cfasl-node-dependencies (cfasl-node module previous-nodes-map previous-nodes-list)
   "This function takes a cfasl-node and its module and builds nodes for all of that module's dependencies, and adds them as dependencies of the cfasl-node"
  (let ((dependencies (mapcar
                       (lambda (name) (create-dependency-node
                                       name
                                       previous-nodes-map
                                       previous-nodes-list))
                       (compile-depends-on module))))
  (add-dependencies cfasl-node dependencies :type :compile)
  (add-dependencies cfasl-node dependencies :type :load)
  (add-dependency cfasl-node (create-source-file-node module) :type :compile)));Add dependency on the lisp source file

(defun create-fasl-node (module previous-nodes-map previous-nodes-list)
  "This function constructs a fasl-node in the dependency graph.  It also builds dependency-graph-nodes for any of its dependencies."
  (let ((fullname (namestring (make-pathname :type (fasl-extension) :defaults (pathname (fullname module))))))
    ;(format T "building fasl file node with name: ~a.~%" fullname)
    (with-catching-dependency-cycle (fullname previous-nodes-map previous-nodes-list)
      (let ((existing-node (gethash fullname *node-map*)))
        (or existing-node ;If this node already exists, don't re-create it.
            (let* ((target (enough-namestring
                            (make-pathname :type (fasl-extension) :defaults (filepath module))
                            *buildpath*))
                   (fasl-node (make-instance 'fasl-node
                                :name (namestring (make-pathname :type nil :defaults target))
                                :fullname fullname
                                :target target)))
              (set-fasl-node-dependencies fasl-node module previous-nodes-map previous-nodes-list)
              (setf (gethash fullname *node-map*) fasl-node)))))));Add this node to *node-map*


(defun create-cfasl-node (module previous-nodes-map previous-nodes-list)
  "This function constructs a cfasl-node in the dependency graph.  It also builds dependency-graph-nodes for any of its dependencies."
  (let ((fullname (namestring (make-pathname :type (cfasl-extension) :defaults (pathname (fullname module))))))
    ;(format T "building cfasl file node with name: ~a.~%" fullname)
    (with-catching-dependency-cycle (fullname previous-nodes-map previous-nodes-list)
      (let ((existing-node (gethash fullname *node-map*)))
        (or existing-node ;If this node already exists, don't re-create it.
            (let* ((target (enough-namestring
                            (make-pathname :type (cfasl-extension) :defaults (filepath module))
                            *buildpath*))
                   (cfasl-node (make-instance 'cfasl-node
                                :name (namestring (make-pathname :type nil :defaults target))
                                :fullname fullname
                                :target target)))
              (set-cfasl-node-dependencies cfasl-node module previous-nodes-map previous-nodes-list)
              (setf (gethash fullname *node-map*) cfasl-node)))))));Add this node to *node-map*



(defun create-dump-image-graph (imagepath sourcepath)
  "Constructs a dependency graph to dump a lisp image at imagepath with the lisp file at sourcepath loaded"
  (create-image-dump-node (create-dependency-graph sourcepath) imagepath))

(defun create-dependency-graph (sourcepath)
  "Constructs a dependency graph with a fasl-node for the lisp file at sourcepath as the root of the graph.  If build-for-asdf is non-nil, then the graph will be build with all dependencies being treated as load dependencies"
  (setf *node-map* (make-hash-table :test #'equal))
  (setf *module-map* (make-hash-table :test #'equal))
  (setf *build-module* nil)
  (setf *buildpath* (make-pathname :name nil :type nil :defaults (find-build-file (pathname sourcepath))))
  (create-lisp-node (list (resolve-module (pathname sourcepath) :parent-module *build-module*))))
