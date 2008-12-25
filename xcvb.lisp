(in-package :xcvb)

;; TODO: currently, a referenced file always keeps the build-module
;; of the referencing file, unless specified as (:external "foo/bar")
;; or as (:external "foo/bar" "path/to/BUILD.lisp")
;; -- all paths relative to current build-module.
;; In the future, we may want to always probe the BUILD-module of a file
;; from its containing directory and its parents.
;; functions involved include: module-from-name, and plenty of others.
;; We can then get rid of :external in create-dependency-node-from-type.
;; Other functions may create modules and nodes whose build-module is
;; the same as the referencing module/node instead of that deduced from
;; the filesystem.

(defparameter *module-map* nil
  "Map of all module objects made thus far to
   prevent recreating the same module twice")
(defparameter *node-map* nil
  "Map of all the nodes created thus far to
   prevent redundand nodes in the dependency graph")
(defparameter *build-module* nil
  "Module object for the nearest surrounding BUILD.lisp file
to the initial file that is being operated on.")

(defmacro module (&rest options)
  (declare (ignore options))
  nil)

(defclass module () ())

(defclass concrete-module (module)
  ((fullname
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
    :documentation "A list of dependencies that must be loaded before this
file can be compiled")
   (load-depends-on
    :initarg :load-depends-on :initform nil :accessor load-depends-on
    :documentation "A list of dependencies that must be loaded before this
file can be loaded")
   (filepath
    :initarg :filepath :accessor filepath
    :documentation "The absolute path to the file that
the module was declared in")
   (supersedes-asdf
    :initarg :supersedes-asdf :reader supersedes-asdf :initform nil
    :documentation "A list of ASDF systems superseded by this module")
   (extension-forms
    :initarg :extension-forms :initform nil :accessor extension-forms
    :documentation "extension forms!")))

(defmethod initialize-instance :after ((module concrete-module) &key depends-on)
  (when depends-on
    (with-slots (compile-depends-on load-depends-on) module
      (setf compile-depends-on
            (append compile-depends-on
                    (mapcar (lambda (dep) (list :compile dep))
                            depends-on)))
      (setf load-depends-on (append load-depends-on depends-on)))))

(defclass standard-module (concrete-module)
  ((build-module
    :initarg :build-module :accessor build-module
    :documentation "The build module for this module")))

(defclass build-module (concrete-module)
  ((build-requires
    :initarg :build-requires :accessor build-requires :initform nil
    :documentation
"A list of dependencies that apply to all files in the
system specified by this BUILD.lisp file.
These dependencies will be loaded first thing
into an image that will be used for all future compile/load operations")))


(defun register-module (module)
  "Adds the given module object to the hashtable containing all the modules thus far.
It is keyed both by its fullname and its nickname."
  (unless (null (nickname module))
    (setf (gethash (nickname module) *module-map*) module))
  (setf (gethash (make-fullname-absolute module) *module-map*) module)) ;;TODO: why?


(defun parse-module (form &key build-module-p)
  "Takes a module declaration form and returns a module object representing
  that module.  Inherits licence, author, and maintainer slots
  from the build-module, if not specifically overwritten"
  (destructuring-bind (module-decl
                       (&key fullname
                             nickname
                             licence
                             version
                             author
                             maintainer
                             description
                             long-description
                             compile-depends-on
                             depends-on
                             load-depends-on)
                       &rest extension-forms) form
    (unless (eql module-decl 'xcvb:module)
      (error "Missing module declaration"))
    (when (and build-module-p (not fullname))
      (error "Build module must have a fullname"))
    (let ((module
           (make-instance (if build-module-p 'build-module 'standard-module)
             :fullname fullname
             :nickname nickname
             :author author
             :maintainer maintainer
             :licence licence
             :version version
             :description description
             :long-description long-description
             :compile-depends-on compile-depends-on
             :load-depends-on load-depends-on
             :depends-on depends-on
             :extension-forms extension-forms)))
      module)))

(defun read-first-file-form (filepath)
  "Reads the first form from the top of a file"
  (with-standard-io-syntax ()
    (let ((*features* (cons :xcvb *features*))
	  (*package* (find-package :xcvb-user))
	  (*read-eval* nil))
      (with-open-file (in filepath) (read in)))))

(defun module-form-p (form)
  "Returns whether or not the given form is an xcvb:module form"
  (eql (first form) 'xcvb:module))
#|  (destructuring-bind (module-decl &rest rest) form
    (declare (ignore rest))
    (eql module-decl 'xcvb:module)))|#

(defun create-module (module-path &key parent-module build-module-p)
  "Takes a filepath to a lisp file, and returns the module object
 represented by the module specifer at the top of that lisp file.
 If the argument parent-module is supplied, then the new module
 will be given a fullname relative to the fullname of parent-module
 (if the fullname isn't already specified)"
  ;;(format T "resolving ~:[module~;build-module~]: ~a~%" build-module-p module-path)
  (when (and *build-module* (equal (pathname (filepath *build-module*))
                                   (pathname module-path)))
    ;;(format T "using build-module~%")
    (return-from create-module *build-module*))
  (let ((module (parse-module (read-first-file-form module-path) :build-module-p build-module-p)))
    (setf (filepath module) module-path)
    (when (not (typep module 'build-module))
      (if (typep parent-module 'build-module)
        (setf (build-module module) parent-module)
        (setf (build-module module) (build-module parent-module))))
    (if (and parent-module (not (fullname module)))
      (setf (fullname module)
            (namestring (merge-pathnames ;NUN
                         (enough-namestring (filepath module)
                                            (filepath parent-module))
                         (fullname parent-module))))
      ;;(strcat (fullname parent-module) "/" (enough-namestring (filepath module) (filepath parent-module))))
      (setf (fullname module)
            ;;(namestring (make-pathname :name (pathname-name (filepath module)) :type (pathname-type (filepath module)) :defaults (fullname module))))) ;NUN
            (strcat (fullname module)
                    "/"
                    (file-namestring (filepath module)))))
    (when (typep module 'build-module)
      (make-fullname-absolute module)) ;;TODO: why this???
    (handle-extension-forms module)
    (register-module module)))

(defun module-from-name (name &optional
                         (build-module *build-module*)
                         old-build-module)
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
  (flet ((module-from-parent-module (name
                                     parent-module
                                     &optional old-build-module)
           (let ((source-file-pathname
                  (make-pathname
                   :type "lisp"
                   :defaults (merge-pathnames name (filepath
                                                    (or old-build-module
                                                        parent-module))))))
             (when (probe-file source-file-pathname)
               (create-module source-file-pathname
                              :parent-module parent-module)))))
    (or (gethash (strcat (namestring
                          (make-pathname
                           :name nil
                           :type nil
                           :defaults (fullname build-module)))
                         "/"
                         name) ;NUN
                 *module-map*)
        (module-from-parent-module name build-module old-build-module)
        (let ((parent-module (gethash (top-level-name name) *module-map*)))
          (if parent-module
            (or (module-from-parent-module name parent-module)
                (lookup-in-registry name build-module))))
        (progn
          (format t "The path of the build-module is: ~a~%" (filepath build-module))
          (simply-error ()
                      "The module with name \"~a\" cannot be found" name)))))


(defun lookup-in-registry (module-name build-module)
  (declare (ignore module-name build-module))
  (error "The registry has not been implemented yet"))


(defun find-build-file (source-filepath)
  "Looks for the first BUILD.lisp file in the given directory or any
of its parent directories and returns its filepath.
Throws an error if no BUILD.lisp file is found"
  (if (equal source-filepath (pathname-parent source-filepath))
    (error 'no-build-file-found
           :format-control "no BUILD.lisp file found"))
  (let ((build-filepath
         (make-pathname :name "BUILD" :type "lisp" :defaults source-filepath)))
    (if (probe-file build-filepath)
      build-filepath
      (find-build-file (pathname-parent source-filepath)))))


;;This class represents a node in the graph of dependencies
(defclass dependency-graph-node ()
  ((fullname
    :initarg :fullname :initform nil :accessor fullname
    :documentation "abstract name for the intention of the node,
e.g. //xcvb-test/foo/bar/quux.fasl") ;TODO
   (build-module
    :initarg :build-module :initform nil :reader build-module
    :documentation "The build-module for this node")
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

(defclass lisp-image-node (dependency-graph-node-with-dependencies) ())

(defclass file-node (dependency-graph-node)
  ((source-filepath
    :initarg :source-filepath
    :initform (error "Must supply source-filepath")
    :reader source-filepath)))

(defclass source-file-node (file-node) ())

(defclass object-file-node (dependency-graph-node-with-dependencies file-node) ())

;;;NOTE:
;;; Currently this is non-functional, because
;;; -1- we don't have access to the target lisp's configuration at this point,
;;; so we don't know whether or not it will have CFASLs
;;; -2- we haven't implemented a facility to duplicate the work in the Makefile
;;; based on a conditional, either.
;;; But ultimately, we'd like to be able to declare that a component may depend
;;; on either the cfasl or the fasl for the specified dependency,
;;; whichever is available and more practical.
(defclass cfasl-or-fasl-node ();;TODO figure out inheritance
  ((fasl-node
    :initarg :fasl-node
    :initform (error "Must supply fasl-node")
    :reader fasl-node)
   (cfasl-node
    :initarg :cfasl-node
    :initform (error "Must supply cfasl-node")
    :reader cfasl-node)))

(defclass fasl-node (object-file-node) ())

(defclass cfasl-node (object-file-node) ())

(defclass image-dump-node (dependency-graph-node)
  ((lisp-image :initarg :lisp-image :initform nil :reader lisp-image)
   (dump-path :initarg :dump-path :initform nil :reader dump-path)))

(defclass asdf-system-node (dependency-graph-node)
  ((name
    :initarg :name :reader name)))

(defclass load-source-node (file-node)
  ())

(defgeneric add-dependency (node dependency &key type)
  (:documentation "Adds a dependency to a node in the dependency graph."))

(defmethod add-dependency ((node dependency-graph-node-with-dependencies)
                           (dependency dependency-graph-node)
                           &key type)
  (case type
    (:load (push dependency (load-dependencies node)))
    (:compile (push dependency (compile-dependencies node)))
    (otherwise (error "Invalid type of dependency.
Must be either :compile or :load"))))


(defun add-dependencies (node dependency-list &key type)
  "Adds a list of dependencies to a node in the dependency graph."
  (dolist (dep dependency-list)
    (add-dependency node dep :type type)))

(defun create-dependency-node (dependency
                               build-module
                               previous-nodes-map
                               previous-nodes-list)
  "Takes a form for a dependency, and builds the correct type of
 dependency-graph-node.  It also needs the map of previous nodes and the list
of previous nodes to pass on to the functions that actually create the nodes of
the right type, so that they can detect and handle dependency cycles properly."
  (if (typep dependency 'list)
    (destructuring-bind (dep-type &rest dep) dependency
      (create-dependency-node-from-type
       dep-type
       dep
       build-module
       previous-nodes-map
       previous-nodes-list))
    (create-dependency-node-from-type :load
                                      (list dependency)
                                      build-module
                                      previous-nodes-map
                                      previous-nodes-list)))

(defgeneric create-dependency-node-from-type (dependency-type
                                              dependency
                                              build-module
                                              previous-nodes-map
                                              previous-nodes-list
                                              &optional old-build-module)
  (:documentation "Takes a symbol specifying the type of the dependency and a
string specifying its name, and creates the dependency-graph-node of the proper
type.  It also needs the map of previous nodes and the list of previous nodes
to pass on to the functions that actually create the nodes of the right type,
so that they can detect and handle dependency cycles properly."))

(defmethod create-dependency-node-from-type ((dependency-type (eql :load))
                                             dependency
                                             build-module
                                             previous-nodes-map
                                             previous-nodes-list
                                             &optional old-build-module)
  (destructuring-bind (dep-name) dependency
    (create-fasl-node (module-from-name dep-name build-module old-build-module)
                      previous-nodes-map
                      previous-nodes-list)))

(defmethod create-dependency-node-from-type ((dependency-type (eql :external))
                                             dependency
                                             build-module
                                             previous-nodes-map
                                             previous-nodes-list
                                             &optional old-build-module)
  (declare (ignore old-build-module))
  (destructuring-bind (dep &optional build-file-path) dependency
    (if (typep dep 'list)
      (destructuring-bind (dep-type dep-name &rest rest) dep
        (let ((build-file-path
               (if build-file-path
                 (merge-pathnames build-file-path (filepath build-module))
                 (find-build-file  (merge-pathnames dep-name
                                                    (filepath build-module))))))
          (create-dependency-node-from-type
           dep-type
           (cons dep-name rest)
           (create-module build-file-path :build-module-p T)
           previous-nodes-map
           previous-nodes-list
           build-module)))
      (let ((build-file-path
             (or build-file-path
                 (find-build-file
                  (merge-pathnames dep (filepath build-module))))))
        (create-dependency-node-from-type
         :load
         (list dep)
         (create-module build-file-path :build-module-p T)
         previous-nodes-map
         previous-nodes-list
         build-module)))))

(defmethod create-dependency-node-from-type ((dependency-type (eql :compile))
                                             dependency
                                             build-module
                                             previous-nodes-map
                                             previous-nodes-list
                                             &optional old-build-module)
  (destructuring-bind (dep-name) dependency
    (create-cfasl-node (module-from-name dep-name build-module old-build-module)
                       previous-nodes-map
                       previous-nodes-list)))

;;NOTE - This should currently never be used.  cfasl-or-fasl node functionality
;;has not yet been implemented
(defmethod create-dependency-node-from-type ((dependency-type
                                              (eql :cfasl-or-fasl))
                                             dependency
                                             build-module
                                             previous-nodes-map
                                             previous-nodes-list
                                             &optional old-build-module)
  (destructuring-bind (dep-name) dependency
    (make-instance 'cfasl-or-fasl-node
      :fasl-node (create-fasl-node (module-from-name dep-name
                                                     build-module
                                                     old-build-module)
                                   previous-nodes-map
                                   previous-nodes-list)
      :cfasl-node (create-cfasl-node (module-from-name dep-name
                                                       build-module
                                                       old-build-module)
                                     previous-nodes-map
                                     previous-nodes-list))))

(defmethod create-dependency-node-from-type ((dependency-type (eql :asdf))
                                             dependency
                                             build-module
                                             previous-nodes-map
                                             previous-nodes-list
                                             &optional old-build-module)
  (declare (ignore old-build-module))
  (destructuring-bind (dep-name) dependency
    (create-asdf-system-node dep-name)))

(defmethod create-dependency-node-from-type ((dependency-type (eql :load-source))
                                             dependency
                                             build-module
                                             previous-nodes-map
                                             previous-nodes-list
                                             &optional old-build-module)
  (declare (ignore old-build-module))
  (destructuring-bind (dep-name) dependency
    (create-load-source-node dep-name)))

;; TODO: document better when you need to use this dependency?
;; = have a root node for the dependency DAG
;; maybe get rid of it? or rename it as root node of the whole graph
;; (why need a root instead of say iterating on the DAG?)
(defun create-lisp-image-node (dependencies &optional name)
  "This function constructs a lisp-image-node in the dependency graph
for a lisp with the given dependencies loaded.
This is used as the root as the dependency graph
unless there is a dump-image node above it."
  (let ((previous-nodes-map (make-hash-table :test #'equal))
        (previous-nodes-list nil))
    (flet ((f (dep)
             (typecase dep
               (dependency-graph-node dep)
               (module
                  (create-fasl-node
                   dep previous-nodes-map previous-nodes-list))
               (otherwise
                  (create-dependency-node dep
                                          *build-module*
                                          previous-nodes-map
                                          previous-nodes-list)))))
      (let* ((dependencies (mapcar #'f dependencies))
             (fullname (or name
                           (format nil "//lisp-image/~{~A~^//~}"
                                   (mapcar #'fullname dependencies))))
             (lisp-image-node (make-instance 'lisp-image-node
                                :fullname fullname
                                :build-module *build-module*
                                :compile-dependencies (reverse dependencies))))
        lisp-image-node))))


(defun create-source-file-node (module)
  "This function constructs a source-file-node in the dependency graph"
  (let ((fullname
         (namestring (fullname module))))
    ;;If this node already exists, don't recreate it
    (or (gethash fullname *node-map*)
        (let* ((source-node (make-instance 'source-file-node
                              :fullname fullname
                              :build-module (if (typep module 'build-module)
                                              module
                                              (build-module module))
                              :source-filepath (filepath module))))
          (setf (gethash fullname *node-map*) source-node)))))

(defun create-asdf-system-node (system-name)
  "This function constructs a asdf-system-node in the dependency graph"
  (let ((fullname (strcat "//asdf/" system-name)))
    (or (gethash fullname *node-map*)
        (setf (gethash fullname *node-map*)
              (make-instance 'asdf-system-node
                :name (coerce-asdf-system-name system-name)
                :fullname fullname)))))

(defun create-load-source-node (source-file-name)
  "This function constructs a load-source-node in the dependency graph"
  (let* ((truename (truename source-file-name)) ;NUN
	 (fullname (strcat "//load-source/" (namestring truename)))) ;NUN
    (or (gethash fullname *node-map*)
        (setf (gethash fullname *node-map*)
              (make-instance 'load-source-node
                :source-filepath truename
                :fullname fullname)))))

(defun create-image-dump-node (lisp-image-node dump-path)
  "This function constructs an image-dump-node in the dependency graph, which is designed to dump an image of the lisp state described by lisp-image-node"
  (make-instance 'image-dump-node
    :dump-path dump-path
    :lisp-image lisp-image-node
    :build-module *build-module*
    :fullname (format nil "//image-dump/~a" dump-path)))


(defun call-while-catching-dependency-cycle (fullname
                                             previous-nodes-map
                                             previous-nodes-list
                                             thunk)
  "This function wraps another function call with code to detect and handle a dependency cycle"
  (when (nth-value 1 (gethash fullname previous-nodes-map))
    (error 'dependency-cycle
           :format-control "Dependency cycle found: ~s"
           :format-arguments (list (cons fullname previous-nodes-list))))
  (when (and (string-equal (pathname-type fullname) "fasl")
             (nth-value 1 (gethash
                           (make-pathname :type "cfasl" :defaults fullname)
                           previous-nodes-map)))
    (error 'dependency-cycle
            :format-control "Dependency cycle found, compiling a file cannot
depend on loading it. ~s"
            :format-arguments (list (cons fullname previous-nodes-list))))
  ;;Add node to map of previous nodes to detect dependency cycles
  (setf (gethash fullname previous-nodes-map) nil)
  (unwind-protect
       ;;Add node to list of previous nodes so that if dependency cycle is found,
       ;;the cycle can be printed
       (funcall thunk (cons fullname previous-nodes-list))
    ;;remove this node from the map of previous nodes
    (remhash fullname previous-nodes-map)))

(defmacro with-catching-dependency-cycle ((fullname
                                           previous-nodes-map
                                           previous-nodes-list)
                                          &body body)
  "This macro wraps a body of code with code to detect and handle a dependency
cycle"
  `(call-while-catching-dependency-cycle
    ,fullname
    ,previous-nodes-map
    ,previous-nodes-list
    (lambda (,previous-nodes-list) ,@body)))


(defun set-fasl-node-dependencies (fasl-node
                                   module
                                   previous-nodes-map
                                   previous-nodes-list)
   "This function takes a fasl-node and its module and builds nodes for all of
that module's dependencies, and adds them as dependencies of the fasl-node"
  (add-dependencies fasl-node (mapcar
                               (lambda (name) (create-dependency-node
                                               name
                                               (build-module fasl-node)
                                               previous-nodes-map
                                               previous-nodes-list))
                               (load-depends-on module)) :type :load)
  (add-dependencies fasl-node (mapcar
                               (lambda (name) (create-dependency-node
                                               name
                                               (if (typep module 'build-module)
                                                 module
                                                 (build-module module))
                                               previous-nodes-map
                                               previous-nodes-list))
                               (compile-depends-on module)) :type :compile)
  ;;Add dependency on the lisp source file
  (add-dependency fasl-node (create-source-file-node module) :type :compile))

(defun set-cfasl-node-dependencies (cfasl-node
                                    module
                                    previous-nodes-map
                                    previous-nodes-list)
  "This function takes a cfasl-node and its module and builds nodes for all of
that module's dependencies, and adds them as dependencies of the cfasl-node"
  (let ((dependencies (mapcar
                       (lambda (name) (create-dependency-node
                                       name
                                       (build-module cfasl-node)
                                       previous-nodes-map
                                       previous-nodes-list))
                       (compile-depends-on module))))
    (add-dependencies cfasl-node dependencies :type :compile)
    (add-dependencies cfasl-node dependencies :type :load)
    ;;Add dependency on the lisp source file
    (add-dependency cfasl-node (create-source-file-node module) :type :compile)))

(defun create-fasl-node (module previous-nodes-map previous-nodes-list)
  "This function constructs a fasl-node in the dependency graph.  It also
builds dependency-graph-nodes for any of its dependencies."
  (let ((fullname (namestring ;NUN
                   (make-pathname :type "fasl" :defaults (fullname module)))))
    ;(format T "building fasl file node with name: ~a.~%" fullname)
    (with-catching-dependency-cycle
        (fullname previous-nodes-map previous-nodes-list)
      (let ((existing-node (gethash fullname *node-map*)))
        (or existing-node ;If this node already exists, don't re-create it.
            (let* ((fasl-node (make-instance 'fasl-node
                                :fullname fullname
                                :build-module (if (typep module 'build-module)
                                                module
                                                (build-module module))
                                :source-filepath (filepath module))))
              (set-fasl-node-dependencies fasl-node
                                          module
                                          previous-nodes-map
                                          previous-nodes-list)
              (setf (gethash fullname *node-map*) fasl-node)))))))


(defun create-cfasl-node (module previous-nodes-map previous-nodes-list)
  "This function constructs a cfasl-node in the dependency graph.  It also
builds dependency-graph-nodes for any of its dependencies."
  (let ((fullname (namestring ;NUN
                   (make-pathname :type "cfasl" :defaults (fullname module)))))
    ;(format T "building cfasl file node with name: ~a.~%" fullname)
    (with-catching-dependency-cycle
        (fullname previous-nodes-map previous-nodes-list)
      (let ((existing-node (gethash fullname *node-map*)))
        (or existing-node ;If this node already exists, don't re-create it.
            (let* ((cfasl-node (make-instance 'cfasl-node
                                 :fullname fullname
                                 :build-module (if (typep module 'build-module)
                                                 module
                                                 (build-module module))
                                 :source-filepath (filepath module))))
              (set-cfasl-node-dependencies cfasl-node
                                           module
                                           previous-nodes-map
                                           previous-nodes-list)
              (setf (gethash fullname *node-map*) cfasl-node)))))))


(defun create-dump-image-graph (imagepath sourcepath)
  "Constructs a dependency graph to dump a lisp image at imagepath with the
lisp file at sourcepath loaded"
  (create-image-dump-node (create-dependency-graph sourcepath) imagepath))

(defun create-dependency-graph (sourcepath)
  "Constructs a dependency graph with a lisp-image-node for a lisp image
with the given lisp file loaded as the root of the graph."
  (setf sourcepath (merge-pathnames sourcepath "BUILD.lisp"))
  (setf *node-map* (make-hash-table :test #'equal))
  (setf *module-map* (make-hash-table :test #'equal))
  (setf *build-module* nil)
  (setf *build-module* (create-module (find-build-file sourcepath)
                                      :build-module-p T))
  (create-lisp-image-node
   (list (create-module sourcepath :parent-module *build-module*))))

(defun xcvb-setup-dependencies ()
  (mapcar (lambda (x)
	    `(:load-source
	      ,(merge-pathnames x *xcvb-lisp-directory*)))
	  *xcvb-setup-dependencies*))
