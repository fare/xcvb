(in-package :xcvb)

(defparameter *module-map* nil
  "Map of all module objects made thus far to prevent recreating the same module twice")
(defparameter *node-map* nil
  "Map of all the nodes created thus far to prevent redundand nodes in the dependency graph")
(defparameter *build-module* nil
  "Module object for the nearest surrounding BUILD.lisp file")
(defparameter *build-node* nil
  "Dependency-graph-node object for the nearest surrounding BUILD.lisp file")
(defparameter *buildpath* nil
  "Filesystem path for the nearest surrounding BUILD.lisp file")
(defparameter *special-functions-map* (make-hash-table)
  "Map of keywords that may appear in the special-forms part of a module declaration to functions")
;(defparameter *build-for-asdf-p* nil
;  "Flag to specify if the dependency graph should be built for creating an asdf file")


(defclass module () ())

(defclass concrete-module (module)
  ((name :initarg :name :reader name :initform (error "Must supply a name for the module") :documentation "The name of the module. Will usually be the same as the name of the file")
   (fullname :initarg :fullname :accessor fullname :initform nil :documentation "The full name of the module.  This is found from the fullname of his module's build.lisp file")
   (nickname :initarg :nickname :reader nickname :initform nil :documentation "A short name to be used to refer to the module")
   (origin :initarg :origin :reader origin :initform nil :documentation "The origin specifies the filepath that all names will be assumed to be relative to.  If the origin isn't specified in the module, it will search for a build.lisp file in the current directory and any parent directories and treat the first directory found containing one as the origin")
   (author :initarg :author :reader author :initform nil :documentation "The author of the file")
   (maintainer :initarg :maintainer :reader maintainer :initform nil :documentation "The maintainer(s) of the file")
   (version :initarg :version :reader version :initform nil :documentation "The version number of the file")
   (licence :initarg :licence :reader licence :initform nil :documentation "The licence being used for the file")
   (description :initarg :description :reader description :initform nil :documentation "A short description of the file")
   (long-description :initarg :long-description :reader long-description :initform nil :documentation "A detailed description of the file")
   (compile-depends-on :initarg :compile-depends-on :initform nil :reader compile-depends-on :documentation "A list of dependencies that must be loaded before this file can be compiled")
   (load-depends-on :initarg :load-depends-on :initform nil :reader load-depends-on :documentation "A list of dependencies that must be loaded before this file can be loaded")
   (build-depends-on :initarg :build-depends-on :initform nil :reader build-depends-on :documentation "A list of dependencies that must be loaded before this file can be compiled or loaded - in the order in which they should be loaded")
   (filepath :initarg :filepath :accessor filepath :documentation "The absolute path to the file that the module was declared in")
   (filename :initarg :filename :accessor filename :documentation "The filename of the file that the module was declared in")))

(defclass build-module (concrete-module)
  ((build-requires :initarg :build-requires :accessor build-requires :initform nil :documentation "A list of dependencies that apply to all files in the system specified by this BUILD.lisp file.  These dependencies will be loaded first thing into an image that will be used for all future compile/load operations")))


(defun strcat (&rest strings)
  "String concatenation function"
  (apply 'concatenate 'string strings))

;(defun appendnew (&rest lists &key key (test #'eql testp) (test-not nil notp))
  

;This condition is signaled by the find-origin function if no BUILD.lisp file can be found
(define-condition no-origin-found (simple-error)
  ())

;This condition is signaled if the dependency graph has any cycles in it.
(define-condition dependency-cycle (simple-error)
  ())


(defun pathname-parent (pathname)
  "Takes a pathname and returns the pathname of the parent directory of the directory of the given pathname"
  (if (null pathname)
    nil
    (let ((dir (pathname-directory pathname)))
      (make-pathname :directory (subseq dir 0 (- (length dir) 1)) :name nil :type nil :defaults pathname))))

(defun add-to-module-map (module)
  "Adds the given module to the hashtable containing all the modules thus far"
  (unless (null (nickname module))
    (setf (gethash (nickname module) *module-map*) module))
  (setf (gethash (make-fullname-absolute module) *module-map*) module))

(defmacro module (&rest options)
  (declare (ignore options))
  nil)

(defun module-form-p (form)
  "Returns whether or not the given form is an xcvb:module form"
  (destructuring-bind (module-decl &rest rest) form
    (declare (ignore rest))
    (eql module-decl 'xcvb:module)))

(defun read-first-file-form (filename)
  "Reads the first form from the top of a file"
  (with-open-file (in filename) (read in)))

(defun get-module-from-file (filename)
  "Returns the module form out of the top of the given file.  Throws an error if there is no module form, or it is not at the top of the file"
  (let ((form (read-first-file-form filename)))
    (unless (module-form-p form)
      (error "Missing module declaration"))
    form))

(defun parse-module (module)
  "Takes a module specifier and returns a module object representing that module.  Inherits licence, author, maintainer, description, and long-description slots from the build-module, if not specifically overwritten"
  (destructuring-bind (module-decl (&key name fullname nickname origin licence version author maintainer description long-description compile-depends-on load-depends-on build-depends-on) &rest special-forms) module
    (declare (ignore module-decl))
    (let ((module 
           (make-instance 'concrete-module
             :name name
             :fullname fullname
             :nickname nickname
             :origin origin
             :author (or author (if *build-module* (author *build-module*)))
             :maintainer (or maintainer (if *build-module* (maintainer *build-module*)))
             :licence (or licence (if *build-module* (licence *build-module*)))
             :version (or version (if *build-module* (version *build-module*)))
             :description (or description (if *build-module* (description *build-module*)))
             :long-description (or long-description (if *build-module* (long-description *build-module*)))
             :compile-depends-on compile-depends-on
             :load-depends-on load-depends-on
             :build-depends-on build-depends-on)))
      (handle-special-forms module special-forms)
      module)))

(defmacro defspecial (name keyword args &body body)
  `(setf (gethash ,keyword *special-functions-map*)
         (defun ,name (,@args) ,@body)))

(defspecial add-dependency-to-module :add (module dep-type value)
  (case dep-type
      (:compile-depends-on (pushnew value (slot-value module 'compile-depends-on) :test #'equal))
      (:load-depends-on (pushnew value (slot-value module 'load-depends-on) :test #'equal))
      (:build-depends-on (pushnew value (slot-value module 'build-depends-on) :test #'equal))
      (otherwise (error "Invalid property for :add operation, must be one of :compile-depends-on, :load-depends-on, or :build-depends-on"))))



#|(defun add-dependency-to-module (module &rest args)
  (destructuring-bind ((dep-type value)) args
    (case dep-type
      (:compile-depends-on (pushnew value (slot-value module 'compile-depends-on) :test #'equal))
      (:load-depends-on (pushnew value (slot-value module 'load-depends-on) :test #'equal))
      (:build-depends-on (pushnew value (slot-value module 'build-depends-on) :test #'equal))
      (otherwise (error "Invalid property for :add operation, must be one of :compile-depends-on, :load-depends-on, or :build-depends-on")))))|#

(defspecial remove-dependency-from-module :remove (module dep-type value)
  (case dep-type
    (:compile-depends-on (setf (slot-value module 'compile-depends-on) (remove value (slot-value module 'compile-depends-on) :test #'equal)))
    (:load-depends-on (setf (slot-value module 'load-depends-on) (remove value (slot-value module 'load-depends-on) :test #'equal)))
    (:build-depends-on (setf (slot-value module 'build-depends-on) (remove value (slot-value module 'build-depends-on) :test #'equal)))
    (otherwise (error "Invalid property for :remove operation, must be one of :compile-depends-on, :load-depends-on, or :build-depends-on"))))

(defspecial set-module-slot :set (module slot-name value)
  (case slot
    (:name (setf (slot-value module 'name) value))
    (:fullname (setf (slot-value module 'fullname) value))
    (:nickname (setf (slot-value module 'nickname) value))
    (:origin (setf (slot-value module 'origin) value))
    (:author (setf (slot-value module 'author) value))
    (:maintainer (setf (slot-value module 'maintainer) value))
    (:licence (setf (slot-value module 'licence) value))
    (:version (setf (slot-value module 'version) value))
    (:description (setf (slot-value module 'description) value))
    (:long-description (setf (slot-value module 'long-description) value))
    (:compile-depends-on (setf (slot-value module 'compile-depends-on) value))
    (:load-depends-on (setf (slot-value module 'load-depends-on) value))
    (:build-depends-on (setf (slot-value module 'build-depends-on) value))
    (otherwise (error "Invalid module property"))))

 

#|(defun setup-special-functions-map ()
  (setf (gethash :add *special-functions-map*) #'add-dependency-to-module)
  (setf (gethash :remove *special-functions-map*) #'remove-dependency-from-module)
  (setf (gethash :set *special-functions-map*) #'set-module-slot))

(setup-special-functions-map)|#

(defun handle-special-forms (module special-forms)
  "This handles the special forms from the module declaration.  These forms can do things such as (but not limited to) change slots in the module, specify system-wide dependencies, or extend xcvb itself."
  (dolist (form special-forms)
    (destructuring-bind (operation &rest args) form
      (apply 
       (gethash operation *special-functions-map*) 
       (mapcar (lambda (arg) (if (eql arg :this-module) module arg)) args)))))
#|
(defun handle-special-forms (module special-forms)
  "This handles the special forms from the module declaration.  These forms can do things such as (but not limited to) change slots in the module, specify system-wide dependencies, or extend xcvb itself."
  (dolist (form special-forms)
    (destructuring-bind (operation &rest args) form
      (case operation
        (:add 
           (case property
             (:compile-depends-on (pushnew value (slot-value module 'compile-depends-on) :test #'equal))
             (:load-depends-on (pushnew value (slot-value module 'load-depends-on) :test #'equal))
             (:build-depends-on (pushnew value (slot-value module 'build-depends-on) :test #'equal))
             (otherwise (error "Invalid property for :add operation, must be one of :compile-depends-on, :load-depends-on, or :build-depends-on"))))
        (:remove 
           (case property
             (:compile-depends-on (remove value (slot-value module 'compile-depends-on) :test #'equal))
             (:load-depends-on (remove value (slot-value module 'load-depends-on) :test #'equal))
             (:build-depends-on (remove value (slot-value module 'build-depends-on) :test #'equal))
             (otherwise (error "Invalid property for :remove operation, must be one of :compile-depends-on, :load-depends-on, or :build-depends-on"))))
        (:set 
           (case property
             (:name (setf (slot-value module 'name) value))
             (:fullname (setf (slot-value module 'fullname) value))
             (:nickname (setf (slot-value module 'nickname) value))
             (:origin (setf (slot-value module 'origin) value))
             (:author (setf (slot-value module 'author) value))
             (:maintainer (setf (slot-value module 'maintainer) value))
             (:licence (setf (slot-value module 'licence) value))
             (:version (setf (slot-value module 'version) value))
             (:description (setf (slot-value module 'description) value))
             (:long-description (setf (slot-value module 'long-description) value))
             (:compile-depends-on (setf (slot-value module 'compile-depends-on) value))
             (:load-depends-on (setf (slot-value module 'load-depends-on) value))
             (:build-depends-on (setf (slot-value module 'build-depends-on) value))
             (otherwise (error "Invalid module property"))))))))|#

(defun resolve-module (module-path &optional (parent-module nil parent-module-supplied-p) #|&optional (context *module-context*)|#)
  "Takes a filepath to a lisp file, and returns the module object represented by the module specifer at the top of that lisp file.  If the argument parent-module is supplied, then the new module will be given a fullname relative to the fullname of parent-module"
  (let ((module (parse-module (get-module-from-file module-path))))
    (setf (filepath module) module-path)
    (setf (filename module) (file-namestring module-path))
    (if parent-module-supplied-p
      (setf (fullname module) (strcat (fullname parent-module) "/" (enough-namestring (filepath module) (filepath parent-module)))))
    (add-to-module-map module)))

      
(defun create-module (name parent-module)
  "Takes a name of a new module, and the module whose fullname this new module will be under, and gets the filepath of the parent module and tries to find a file for the new module under that filepath.  If it finds one, then it creates that module out of the module declaration at the top of that file, if not, it returns nil"
  (let ((source-file-pathname (make-pathname :type "lisp" :defaults (merge-pathnames name (filepath parent-module)))))
    (if (probe-file source-file-pathname)
      (resolve-module source-file-pathname parent-module)
      nil)))

(defun get-module (name &optional (build-module *build-module*))
  "This function takes the name of module, and the current build module, and returns the correct module with that given name.  It starts by checking if there already is a module with the fullname of <(fullname build-module)/name>.  If not, then it gets the filepath of the build module, and looks for a file for the module under the filepath of the build module.  If there is no such file, then it assumes that the top-level-name of the given name must be global, and so looks up that global name, and if found, looks for a file for the module under the filepath of the module of top-level-name.  Finally, if that doesn't exist, it tries to find the module in the registry (currently just throws an error)"
  (or (gethash (strcat (fullname build-module) "/" name) *module-map*)
      (create-module name build-module)
      (let ((parent-module (gethash (top-level-name name) *module-map*)))
        (if parent-module
          (or (create-module name parent-module)
              (lookup-in-registry name build-module))))))


(defun find-origin (source-filepath)
  "Looks for the first BUILD.lisp file in the given directory or any of its parent directories that contains a fullname and returns its filepath.  It also sets the module corresponding to that file to the special variable *build-module*.   Throws an error if no BUILD.lisp file with a fullname is found"
  (if (eql source-filepath nil) 
    (error 'no-origin-found :format-control "no origin found for ~S" :format-arguments (list source-filepath)))
  (let ((build-filepath (make-pathname :name "BUILD" :type "lisp" :defaults source-filepath)))
    (if (probe-file build-filepath)
      (let ((build-module (resolve-module build-filepath)))
        (when (fullname build-module)
          (make-fullname-absolute build-module)
          (setf *build-module* build-module)
          (add-to-module-map build-module)
          (return-from find-origin build-filepath))))
    (find-origin (pathname-parent source-filepath))))
  

(defun make-fullname-absolute (module)
  "This function prepends a \"/\" to the beginning of the module's fullname, if there isn't one there already"
  (if (eql 0 (position #\/ (fullname module)))
    (fullname module)
    (setf (fullname module) (strcat "/" (fullname module)))))


;;This class represents a node in the graph of dependencies
(defclass dependency-graph-node ()
  ((fullname :initarg :fullname :initform nil :accessor fullname)
   (target :initarg :target :reader target)
   (name :initarg :name :reader name)
   (long-name :initarg :long-name :reader long-name)
   (hash-name :initarg :hash-name :reader hash-name)
   (long-hash-name :initarg :long-hash-name :reader long-hash-name)))
   
(defclass dependency-graph-node-with-dependencies (dependency-graph-node)
  ((compile-dependencies :initarg :compile-dependencies :initform nil :accessor compile-dependencies)
   (load-dependencies :initarg :load-dependencies :initform nil :accessor load-dependencies)))


(defclass lisp-node (dependency-graph-node-with-dependencies) ())

(defclass source-file-node (dependency-graph-node) ())

(defclass fasl-node (dependency-graph-node-with-dependencies) ())

(defclass image-dump-node (dependency-graph-node) 
  ((lisp-image :initarg :lisp-image :initform nil :reader lisp-image)))

(defclass asdf-system-node (dependency-graph-node) 
  (#|(name :initarg :name :reader name)|#))


(defgeneric add-dependency (node dependency &key type)
  (:documentation "Adds a dependency to a node in the dependency graph.  TODO: this should also add the node to the hashmap of current nodes and check that the node doesn't already exist, if it does,throw an error"))

(defmethod add-dependency ((node dependency-graph-node-with-dependencies) (dependency dependency-graph-node) &key type)
  (case type
    (:load (pushnew dependency (load-dependencies node)))
    (:compile (pushnew dependency (compile-dependencies node)))
    (otherwise (error "Invalid type of dependency.  Must be either :compile or :load"))))

(defgeneric add-dependencies (node dependency-list &key type)
  (:documentation "Adds a list of dependencies to a node in the dependency graph.  TODO: this should also add the node to the hashmap of current nodes and check that the node doesn't already exist, if it does,throw an error"))

(defmethod add-dependencies ((node dependency-graph-node-with-dependencies) (dependency-list list) &key type)
  ;(assert (every #'(lambda (x) (typep x 'dependency-graph-node)) dependency-list) ())
  ;(setf (dependencies node) (nconc (dependencies node) dependency-list)))
  (case type
    (:load (setf (load-dependencies node) (remove-duplicates (nconc (load-dependencies node) dependency-list) :from-end T)))
    (:compile (setf (compile-dependencies node) (remove-duplicates (nconc (compile-dependencies node) dependency-list) :from-end T)))
    (otherwise (error "Invalid type of dependency.  Must be either :compile or :load"))))

(defun create-lisp-node (module-list)
  "This function constructs a lisp-node in the dependency graph, and builds [a] node(s) for the fasl(s) it depends on"
  (let ((lisp-node (make-instance 'lisp-node :target "lisp" :fullname "lisp"))
        (previous-nodes-map (make-hash-table :test #'equal))
        (previous-nodes-list nil))
    (add-dependencies lisp-node (mapcar (lambda (module) (create-fasl-node module previous-nodes-map previous-nodes-list)) module-list) :type :compile)
    lisp-node))


(defun create-source-file-node (module)
  "This function constructs a source-file-node in the dependency graph"
  (let ((fullname (namestring (make-pathname :type "lisp" :defaults (pathname (fullname module))))))
    (or (gethash fullname *node-map*);If this node already exists, don't recreate it
        (let* ((target (pathname (enough-namestring (make-pathname :type "lisp" :defaults (filepath module)) *buildpath*)));Target is the filepath of the fasl relative to the BUILD.lisp file
               (source-node (make-instance 'source-file-node :name (namestring (make-pathname :type nil :defaults target)) :fullname fullname :target target)))
          (setf (gethash fullname *node-map*) source-node)))))

(defun create-asdf-system-node (system-name)
  "This function constructs a asdf-system-node in the dependency graph"
  (let ((fullname (strcat "//asdf/" system-name)))
    (or (gethash fullname *node-map*);If this node already exists, don't recreate it
        (setf (gethash fullname *node-map*) (make-instance 'asdf-system-node :name system-name :target fullname :fullname fullname)))))


(defun create-image-dump-node (lisp-node dump-path)
  "This function constructs an image-dump-node in the dependency graph, which is designed to dump an image of the lisp state described by lisp-node"
  (make-instance 'image-dump-node :target (pathname (enough-namestring dump-path *buildpath*)) :lisp-image lisp-node :fullname (format nil "image-dump:~a" dump-path)))
    

(defun create-dependency-node (dependency previous-nodes-map previous-nodes-list)
  "Takes the name of a dependency, and builds either a fasl-node or an asdf-system-node"
  (if (typep dependency 'list)
    (destructuring-bind (&key asdf) dependency (create-asdf-system-node asdf))
    (create-fasl-node (get-module dependency) previous-nodes-map previous-nodes-list)))  

(defun call-while-catching-dependency-cycle (fullname previous-nodes-map previous-nodes-list thunk)
  "This macro wraps a body of code with code to detect and handle a dependency cycle"
  (when (nth-value 1 (gethash fullname previous-nodes-map))
    (error 'dependency-cycle
           :format-control "Dependency cycle found: ~s"
           :format-arguments (list (cons fullname previous-nodes-list))))
  (setf (gethash fullname previous-nodes-map) nil) ;Add node to map of previous nodes to detect dependency cycles
  (prog1
      (funcall thunk (cons fullname previous-nodes-list)) ;Add node to list of previous nodes so that if dependency cycle is found, the cycle can be printed
    (remhash fullname previous-nodes-map)));remove this node from the map of previous nodes

(defmacro catch-dependency-cycle ((fullname previous-nodes-map previous-nodes-list) &body body)
  "This macro wraps a body of code with code to detect and handle a dependency cycle"
  `(call-while-catching-dependency-cycle ,fullname ,previous-nodes-map ,previous-nodes-list
                                         (lambda (,previous-nodes-list) ,@body)))
  
(defun set-fasl-node-dependencies (fasl-node module previous-nodes-map previous-nodes-list)
  "This function takes a fasl-node and its module and builds nodes for all of that module's dependencies, and adds them as dependencies of the fasl-node"
  #|(dolist (dep-name (build-depends-on module));handle build-dependencies
    (let ((dep-node (create-dependency-node dep-name previous-nodes-map previous-nodes-list)))
      (if (and (eql module *build-module*) (typep dep-node 'asdf-system-node))
        (progn
          (format T "****************************************")
          (pushnew dep-node (asdf-dependencies fasl-node)))
        (progn
          (pushnew dep-node (compile-dependencies fasl-node))
          (pushnew dep-node (load-dependencies fasl-node))))))
  (dolist (dep-name (compile-depends-on module));handle compile-dependencies
    (let ((dep-node (create-dependency-node dep-name previous-nodes-map previous-nodes-list)))
      (if (and (eql module *build-module*) (typep dep-node 'asdf-system-node))
        (pushnew dep-node (asdf-dependencies fasl-node))
        (pushnew dep-node (compile-dependencies fasl-node)))))
  (dolist (dep-name (load-depends-on module));handle load-dependencies
    (let ((dep-node (create-dependency-node dep-name previous-nodes-map previous-nodes-list)))
      (if (and (eql module *build-module*) (typep dep-node 'asdf-system-node))
        (pushnew dep-node (asdf-dependencies fasl-node))
        (pushnew dep-node (load-dependencies fasl-node)))))
  (pushnew (create-source-file-node module) (compile-dependencies fasl-node)));Add dependency on the lisp source file|#
  (let ((build-deps (mapcar (lambda (name) (create-dependency-node name previous-nodes-map previous-nodes-list)) 
                            (build-depends-on module))))
    (add-dependencies fasl-node (append (mapcar 
                                         (lambda (name) (create-dependency-node 
                                                         name 
                                                         previous-nodes-map 
                                                         previous-nodes-list))
                                         (load-depends-on module)) build-deps) :type :load)
    (add-dependencies fasl-node (append (mapcar 
                                         (lambda (name) (create-dependency-node 
                                                         name 
                                                         previous-nodes-map 
                                                         previous-nodes-list)) 
                                         (compile-depends-on module)) build-deps) :type :compile)
    (add-dependency fasl-node (create-source-file-node module) :type :compile)));Add dependency on the lisp source file

(defun create-fasl-node (module previous-nodes-map previous-nodes-list)
  "This function constructs a fasl-node in the dependency graph.  It also builds fasl-nodes for any of its dependencies."
  (let ((fullname (namestring (make-pathname :type "fasl" :defaults (pathname (fullname module))))))
    (format T "building fasl file node with name: ~a.~%" fullname)
    (catch-dependency-cycle (fullname previous-nodes-map previous-nodes-list)
      (let ((existing-node (gethash fullname *node-map*)))
        (or existing-node ;If this node already exists, don't re-create it.
            (let* ((target (pathname (enough-namestring 
                                      (make-pathname :type "fasl" :defaults (filepath module)) 
                                      *buildpath*)))
                   (fasl-node (make-instance 'fasl-node
                                :name (namestring (make-pathname :type nil :defaults target)) 
                                :fullname fullname 
                                :target target)))
              (set-fasl-node-dependencies fasl-node module previous-nodes-map previous-nodes-list)
              (setf (gethash fullname *node-map*) fasl-node)))))));Add this node to *node-map*


(defun lookup-in-registry (filename build-module)
  (declare (ignore filename build-module))
  (error "Dependency doesn't exist"))

(defun top-level-name (name)
  "This function takes a name, and returns everything up to the first \"/\" in the name"
  (subseq name 0 (position #\/ name)))


(defun create-dump-image-graph (imagepath sourcepath)
  "Constructs a dependency graph to dump a lisp image at imagepath with the lisp file at sourcepath loaded"
  (create-image-dump-node (create-dependency-graph sourcepath) imagepath))

(defun create-dependency-graph (sourcepath #|&key build-for-asdf|#)
  "Constructs a dependency graph with a fasl-node for the lisp file at sourcepath as the root of the graph.  If build-for-asdf is non-nil, then the graph will be build with all dependencies being treated as load dependencies"
  (setf *node-map* (make-hash-table :test #'equal))
  (setf *module-map* (make-hash-table :test #'equal))
  (setf *buildpath* (make-pathname :name nil :type nil :defaults (find-origin (pathname sourcepath))))
  ;(setf *build-for-asdf-p* build-for-asdf)
  (create-lisp-node (list (resolve-module (pathname sourcepath) *build-module*))))


(defgeneric traverse-internal (node operation visited-nodes-map)
  (:documentation "Takes a node and returns a list of all nodes in order that must be processed in order to create the target of the node. For fasl-nodes, the operation argument specifies whether to return a list of what is needed to create the fasl (by compiling the source file) or what is needed to load the fasl if it already exists.  For other kinds of nodes, the operation argument mearly changes whether or not the node the method is being called on is included in the resulting list."))

(defmethod traverse-internal ((node lisp-node) (operation (eql :create)) visited-nodes-map)
  (reduce (lambda (dep rest) (nconc (traverse-internal dep :load visited-nodes-map) rest)) (compile-dependencies node) :from-end T :initial-value nil))

(defmethod traverse-internal ((node lisp-node) (operation (eql :load)) visited-nodes-map)
  (cons node (reduce (lambda (dep rest) (nconc (traverse-internal dep :load visited-nodes-map) rest)) (compile-dependencies node) :from-end T :initial-value nil)))

(defmethod traverse-internal ((node asdf-system-node) operation visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map));If this node has already been put in the traversal, don't include it again.
    (setf (gethash (fullname node) visited-nodes-map) T);Add the node to the map of nodes that have already been traversed
    (list node)))

(defmethod traverse-internal ((node image-dump-node) (operation (eql :create)) visited-nodes-map)
  (traverse-internal (lisp-image node) :create visited-nodes-map))

(defmethod traverse-internal ((node image-dump-node) (operation (eql :load)) visited-nodes-map)
  (cons node (traverse-internal (lisp-image node) :load visited-nodes-map)))

(defmethod traverse-internal ((node source-file-node) operation visited-nodes-map)
  (list node))

(defmethod traverse-internal ((node fasl-node) (operation (eql :load)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (cons node (reduce (lambda (dep rest) (nconc (traverse-internal dep :load visited-nodes-map) rest)) (load-dependencies node) :from-end T :initial-value nil))))

(defmethod traverse-internal ((node fasl-node) (operation (eql :create)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest) (nconc (traverse-internal dep :load visited-nodes-map) rest)) (compile-dependencies node) :from-end T :initial-value nil)))


(defun traverse (node operation)
  "Wrapper around traverse-internal generic function"
  (reverse (traverse-internal node operation (make-hash-table :test 'equal))))
  

#|
(defmethod traverse ((node fasl-node))
  (let ((visited-nodes-map (make-hash-table :test 'equal)))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest) (nconc (traverse-internal dep visited-nodes-map) rest)) (compile-dependencies node) :from-end T :initial-value nil)))|#

#|(defmethod traverse ((node lisp-node))
  (let ((visited-nodes-map (make-hash-table :test 'equal)))
    (reduce (lambda (dep rest) (nconc (traverse-internal dep visited-nodes-map) rest)) (compile-dependencies node) :from-end T :initial-value nil)))|#


#|
(defun module-declaration (module)
  ...)

(defun compile-dependencies (module)
  (append ...)
  ...)

(defun load-dependencies (module)
  ...)

(defun compile-module (module)
  ...)

(defun load-module (module)
  ...)

|#


#|(defgeneric get-filename-from-node (node))

(defmethod get-filename-from-node ((node fasl-file-node))
  (destructuring-bind (&key ((:fasl target))) (read-from-string (target node))
   (file-namestring (make-pathname :type "fasl" :defaults (pathname target)))))

(defmethod get-filename-from-node ((node image-file-node))
  (destructuring-bind (&key ((:image target))) (read-from-string (target node))
   (file-namestring (make-pathname :type "img" :defaults (pathname target)))))

(defmethod get-filename-from-node ((node source-file-node))
  (destructuring-bind (&key ((:source target))) (read-from-string (target node))
   (file-namestring (make-pathname :type "lisp" :defaults (pathname target)))))
|#


#|(defun create-filepath (filename module)
  "This function takes a module and the name of a file, and returns the absolute path to the file assuming it is located relative to the path of the origin of the module"
;  (make-pathname :type "lisp" :defaults (merge-pathnames filename (filepath (lookup-build-module (system-name module))))))
;  (make-pathname :type "lisp" :defaults (merge-pathnames filename (origin module))))
  (make-pathname :type "lisp" :defaults (merge-pathnames filename (filepath module)))) ;module is the build module!  Maybe should change name to reflect that...|#

#|(defun system-name (module)
  (destructuring-bind (system-name) (subseq (pathname-directory (fullname module)) 1 2) system-name))|#


#|(defun absolute-pathname-p (pathname)
  (declare (ignore pathname))
  T)

(defun absolutify-pathname (pathname)
  (flet ((try (x)
           (if (absolute-pathname-p x)
             (return-from absolutify-pathname x))))
    (try pathname)
    (let ((pathname (merge-pathnames pathname)))
      (try pathname)
      (truename pathname))))|#

#|
;;;The following 4 functions are for interacting with hashtables.  They provide an easy interface to the 4 most common operations performed on a hashtable - put, get, contains, and remove.
(defun ht-put (table key value)
  "Puts the given key into the given hashtable, associated with the given value"
  (setf (gethash key table) value))

(defun ht-get (table key)
  "Gets the value assosiated with the given key from the given hashtable"
  (gethash key table))

(defun ht-contains (table key)
  "Returns whether or not the given hashtable has a binding for the given key"
  (nth-value 1 (gethash key table)))

(defun ht-remove (table key)
  "Removes the key-value pair assosiated with the given key from the given hashtable"
  (remhash key table))
|#


#|
(defun sbcl-fasl-compiler-command (dependencies)
  (append (list *sbcl-core*)
          (loop for i in dependencies
                nconc (cond
                        (...
                         (list "--eval" (format nil "(load ~S)" fasl)))
                        (...
                         (list "--eval" (format nil "(asdf:oos 'asdf:load-op :~A)" system)))))
          (list "--eval" "(sb-ext:quit)")))


|#
