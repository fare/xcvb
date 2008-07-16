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
(defparameter *extension-functions-map* (make-hash-table)
  "Map of keywords that may appear in the extension-forms part of a module declaration to functions")


(defclass module () ())

(defclass concrete-module (module)
  ((name :initarg :name :reader name :initform nil :documentation "The name of the module. Will usually be the same as the name of the file")
   (fullname :initarg :fullname :accessor fullname :initform nil :documentation "The full name of the module.  This is found from the fullname of his module's build.lisp file")
   (nickname :initarg :nickname :reader nickname :initform nil :documentation "A short name to be used to refer to the module")
   (author :initarg :author :reader author :initform nil :documentation "The author of the file")
   (maintainer :initarg :maintainer :reader maintainer :initform nil :documentation "The maintainer(s) of the file")
   (version :initarg :version :reader version :initform nil :documentation "The version number of the file")
   (licence :initarg :licence :reader licence :initform nil :documentation "The licence being used for the file")
   (description :initarg :description :reader description :initform nil :documentation "A short description of the file")
   (long-description :initarg :long-description :reader long-description :initform nil :documentation "A detailed description of the file")
   (compile-depends-on :initarg :compile-depends-on :initform nil :reader compile-depends-on :documentation "A list of dependencies that must be loaded before this file can be compiled")
   (load-depends-on :initarg :load-depends-on :initform nil :reader load-depends-on :documentation "A list of dependencies that must be loaded before this file can be loaded")
   (filepath :initarg :filepath :accessor filepath :documentation "The absolute path to the file that the module was declared in")
   (filename :initarg :filename :accessor filename :documentation "The filename of the file that the module was declared in")
   (extension-forms :initarg :extension-forms :initform nil :accessor extension-forms :documentation "extension forms!")))

(defclass build-module (concrete-module)
  ((build-requires :initarg :build-requires :accessor build-requires :initform nil :documentation "A list of dependencies that apply to all files in the system specified by this BUILD.lisp file.  These dependencies will be loaded first thing into an image that will be used for all future compile/load operations")))


(defun strcat (&rest strings)
  "String concatenation function"
  (apply 'concatenate 'string strings))


;This condition is signaled by the find-build-file function if no BUILD.lisp file can be found
(define-condition no-build-file-found (simple-error)
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

(defun parse-module (module &key build-module-p)
  "Takes a module specifier and returns a module object representing that module.  Inherits licence, author, maintainer, description, and long-description slots from the build-module, if not specifically overwritten"
  (destructuring-bind (module-decl (&key name fullname nickname licence version author maintainer description long-description compile-depends-on load-depends-on #|build-depends-on|#) &rest extension-forms) module
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
             :description (or description (if *build-module* (description *build-module*)))
             :long-description (or long-description (if *build-module* (long-description *build-module*)))
             :compile-depends-on compile-depends-on
             :load-depends-on load-depends-on
             :extension-forms extension-forms)))
      (handle-extension-forms module extension-forms)
      module)))

(defmacro defextension (name keyword args &body body)
  `(setf (gethash ,keyword *extension-functions-map*)
         (defun ,name (,@args) ,@body)))

(defextension add-dependencies-to-module :add (module dep-type deps)
  (with-slots (compile-depends-on load-depends-on) module
    (case dep-type
      (:compile-depends-on 
         (setf compile-depends-on (remove-duplicates (append compile-depends-on deps) :test #'equal :from-end T)))
      (:load-depends-on 
         (setf load-depends-on (remove-duplicates (append load-depends-on deps) :test #'equal :from-end T)))
      (:compile-and-load-depends-on 
         (setf compile-depends-on (remove-duplicates (append compile-depends-on deps) :test #'equal :from-end T))
         (setf load-depends-on (remove-duplicates (append load-depends-on deps) :test #'equal :from-end T)))
      (otherwise (error "Invalid property for :add operation, must be one of (:compile-depends-on :load-depends-on :compile-and-load-depends-on)")))))


(defextension remove-dependency-from-module :remove (module dep-type value)
  (case dep-type
    (:compile-depends-on 
       (setf (slot-value module 'compile-depends-on) (remove value (slot-value module 'compile-depends-on) :test #'equal)))
    (:load-depends-on 
       (setf (slot-value module 'load-depends-on) (remove value (slot-value module 'load-depends-on) :test #'equal)))
    (:compile-and-load-depends-on 
       (setf (slot-value module 'compile-depends-on) (remove value (slot-value module 'compile-depends-on) :test #'equal))
       (setf (slot-value module 'load-depends-on) (remove value (slot-value module 'load-depends-on) :test #'equal)))
    (otherwise (error "Invalid property for :remove operation, must be one of (:compile-depends-on :load-depends-on :compile-and-load-depends-on"))))

(defextension set-module-slot :set (module slot-name value)
  (let ((slot-symbol (find-symbol (string slot-name) :xcvb)))
    (if slot-symbol
      (setf (slot-value module slot-symbol) value)
      (error 'simple-error :format-control "the slot ~a is not a valid slot for the module" :format-arguments (list slot-name)))))

(defextension load-into-xcvb :xcvb-requires (deps)
  (load-systems deps))

(defun load-systems (systems)
  (declare (ignore systems))
  (error "not yet implemented"))


(defun handle-extension-forms (module extension-forms)
  "This handles the extension forms from the module declaration.  These forms can do things such as (but not limited to) change slots in the module, specify system-wide dependencies, or extend xcvb itself."
  (dolist (form extension-forms)
    (destructuring-bind (operation &rest args) form
      (apply 
       (gethash operation *extension-functions-map*) 
       (mapcar (lambda (arg) (if (eql arg :this-module) module arg)) args)))))


(defun resolve-module (module-path &key (parent-module nil parent-module-supplied-p) build-module-p)
  "Takes a filepath to a lisp file, and returns the module object represented by the module specifer at the top of that lisp file.  If the argument parent-module is supplied, then the new module will be given a fullname relative to the fullname of parent-module"
  (format T "resolving ~:[module~;build-module~]: ~a~%" build-module-p module-path)
  (if (and *build-module* (equal (pathname (filepath *build-module*)) (pathname module-path)))
    (progn
      (format T "using build-module~%")
      (return-from resolve-module *build-module*)))
  (let ((module (parse-module (get-module-from-file module-path) :build-module-p build-module-p)))
    (setf (filepath module) module-path)
    (setf (filename module) (file-namestring module-path))
    (if parent-module-supplied-p
      (setf (fullname module) (strcat (fullname parent-module) "/" (enough-namestring (filepath module) (filepath parent-module)))))
    (add-to-module-map module)))

      
(defun create-module (name parent-module)
  "Takes a name of a new module, and the module whose fullname this new module will be under, and gets the filepath of the parent module and tries to find a file for the new module under that filepath.  If it finds one, then it creates that module out of the module declaration at the top of that file, if not, it returns nil"
  (let ((source-file-pathname (make-pathname :type "lisp" :defaults (merge-pathnames name (filepath parent-module)))))
    (if (probe-file source-file-pathname)
      (resolve-module source-file-pathname :parent-module parent-module)
      nil)))

(defun get-module (name &optional (build-module *build-module*))
  "This function takes the name of module, and the current build module, and returns the correct module with that given name.  It starts by checking if there already is a module with the fullname of <(fullname build-module)/name>.  If not, then it gets the filepath of the build module, and looks for a file for the module under the filepath of the build module.  If there is no such file, then it assumes that the top-level-name of the given name must be global, and so looks up that global name, and if found, looks for a file for the module under the filepath of the module of top-level-name.  Finally, if that doesn't exist, it tries to find the module in the registry (currently just throws an error)"
  (or (gethash (strcat (fullname build-module) "/" name) *module-map*)
      (create-module name build-module)
      (let ((parent-module (gethash (top-level-name name) *module-map*)))
        (if parent-module
          (or (create-module name parent-module)
              (lookup-in-registry name build-module))))))


(defun find-build-file (source-filepath)
  "Looks for the first BUILD.lisp file in the given directory or any of its parent directories that contains a fullname and returns its filepath.  It also sets the special variable *build-module* to the module corresponding to build file that was found.   Throws an error if no BUILD.lisp file with a fullname is found"
  (if (eql source-filepath nil) 
    (error 'no-build-file-found 
           :format-control "no BUILD.lisp file found for ~S" 
           :format-arguments (list source-filepath)))
  (let ((build-filepath (make-pathname :name "BUILD" :type "lisp" :defaults source-filepath)))
    (if (probe-file build-filepath)
      (let ((build-module (resolve-module build-filepath :build-module-p T)))
        (when (fullname build-module)
          (make-fullname-absolute build-module)
          (setf *build-module* build-module)
          (add-to-module-map build-module)
          (return-from find-build-file build-filepath))))
    (find-build-file (pathname-parent source-filepath))))
  

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

(defclass asdf-system-node (dependency-graph-node) ())


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
  (dolist (dep dependency-list)
    (add-dependency node dep :type type)))


(defun create-lisp-node (dependencies)
  "This function constructs a lisp-node in the dependency graph for a lisp with the given dependencies loaded"
  (let ((lisp-node (make-instance 'lisp-node :target "lisp" :fullname "lisp"))
        (previous-nodes-map (make-hash-table :test #'equal))
        (previous-nodes-list nil))
    (add-dependencies lisp-node 
                      (mapcar (lambda (dep) 
                                (typecase dep 
                                  (module (create-fasl-node dep previous-nodes-map previous-nodes-list))
                                  (dependency-graph-node dep)
                                  (otherwise (create-dependency-node dep previous-nodes-map previous-nodes-list))))
                              dependencies)
                      :type :compile)
    lisp-node))


(defun create-source-file-node (module)
  "This function constructs a source-file-node in the dependency graph"
  (let ((fullname (namestring (make-pathname :type "lisp" :defaults (pathname (fullname module))))))
    (or (gethash fullname *node-map*);If this node already exists, don't recreate it
        (let* ((target (enough-namestring (make-pathname :type "lisp" :defaults (filepath module)) *buildpath*));Target is the filepath of the fasl relative to the BUILD.lisp file
               (source-node (make-instance 'source-file-node :name (namestring (make-pathname :type nil :defaults (pathname target))) :fullname fullname :target target)))
          (setf (gethash fullname *node-map*) source-node)))))

(defun create-asdf-system-node (system-name)
  "This function constructs a asdf-system-node in the dependency graph"
  (let ((fullname (strcat "//asdf/" system-name)))
    (or (gethash fullname *node-map*);If this node already exists, don't recreate it
        (setf (gethash fullname *node-map*) (make-instance 'asdf-system-node :name system-name :target fullname :fullname fullname)))))


(defun create-image-dump-node (lisp-node dump-path)
  "This function constructs an image-dump-node in the dependency graph, which is designed to dump an image of the lisp state described by lisp-node"
  (make-instance 'image-dump-node :target (enough-namestring dump-path *buildpath*) :lisp-image lisp-node :fullname (format nil "image-dump:~a" dump-path)))
    

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

(defun create-fasl-node (module previous-nodes-map previous-nodes-list)
  "This function constructs a fasl-node in the dependency graph.  It also builds fasl-nodes for any of its dependencies."
  (let ((fullname (namestring (make-pathname :type "fasl" :defaults (pathname (fullname module))))))
    (format T "building fasl file node with name: ~a.~%" fullname)
    (catch-dependency-cycle (fullname previous-nodes-map previous-nodes-list)
      (let ((existing-node (gethash fullname *node-map*)))
        (or existing-node ;If this node already exists, don't re-create it.
            (let* ((target (enough-namestring 
                            (make-pathname :type "fasl" :defaults (filepath module)) 
                            *buildpath*))
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
  (setf *build-module* nil)
  (setf *buildpath* (make-pathname :name nil :type nil :defaults (find-build-file (pathname sourcepath))))
  (create-lisp-node (list (resolve-module (pathname sourcepath) :parent-module *build-module*))))


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
  
