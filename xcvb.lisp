(in-package :xcvb)

;; TODO: currently, a referenced file always keeps the build-grain
;; of the referencing file, unless specified as (:external "foo/bar")
;; or as (:external "foo/bar" "path/to/BUILD.lisp")
;; -- all paths relative to current build-grain.
;; In the future, we may want to always probe the BUILD-grain of a file
;; from its containing directory and its parents.
;; functions involved include: grain-from-name, and plenty of others.
;; We can then get rid of :external in create-dependency-node-from-type.
;; Other functions may create grains and nodes whose build-grain is
;; the same as the referencing grain/node instead of that deduced from
;; the filesystem.

;;This class represents a node in the graph of dependencies
(defclass dependency-graph-node ()
  ((fullname
    :initarg :fullname :initform nil :accessor fullname
    :documentation "abstract name for the intention of the node,
e.g. //xcvb-test/foo/bar/quux.fasl") ;TODO
   (build-grain
    :initarg :build-grain :initform nil :reader build-grain
    :documentation "The build-grain for this node")
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
    (:load (push dependency (load-depends-on node)))
    (:compile (push dependency (compile-depends-on node)))
    (otherwise (error "Invalid type of dependency.
Must be either :compile or :load"))))


(defun add-dependencies (node dependency-list &key type)
  "Adds a list of dependencies to a node in the dependency graph."
  (dolist (dep dependency-list)
    (add-dependency node dep :type type)))

(defun create-dependency-node (dependency
                               build-grain
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
       build-grain
       previous-nodes-map
       previous-nodes-list))
    (create-dependency-node-from-type :load
                                      (list dependency)
                                      build-grain
                                      previous-nodes-map
                                      previous-nodes-list)))

(defgeneric create-dependency-node-from-type (dependency-type
                                              dependency
                                              build-grain
                                              previous-nodes-map
                                              previous-nodes-list)
  (:documentation "Takes a symbol specifying the type of the dependency and a
string specifying its name, and creates the dependency-graph-node of the proper
type.  It also needs the map of previous nodes and the list of previous nodes
to pass on to the functions that actually create the nodes of the right type,
so that they can detect and handle dependency cycles properly."))

(defmethod create-dependency-node-from-type ((dependency-type (eql :load))
                                             dependency
                                             build-grain
                                             previous-nodes-map
                                             previous-nodes-list)
  (destructuring-bind (dep-name) dependency
    (create-fasl-node (grain-from-name dep-name build-grain)
                      previous-nodes-map
                      previous-nodes-list)))

(defmethod create-dependency-node-from-type ((dependency-type (eql :external))
                                             dependency
                                             build-grain
                                             previous-nodes-map
                                             previous-nodes-list)
  (destructuring-bind (dep) dependency
    (if (typep dep 'list)
      (destructuring-bind (dep-type dep-name &rest rest) dep
        (let ((build-file-path
               (grain-pathname build-grain))) ;; TODO: the build file FOR THE DEPENDENCY
          (create-dependency-node-from-type
           dep-type
           (cons dep-name rest)
           (make-grain-from-file build-file-path :build-p t)
           previous-nodes-map
           previous-nodes-list
           build-grain)))
      (let ((build-file-path
             (grain-pathname build-grain))) ;; TODO: use the build file FOR THE DEPENDENCY
        (create-dependency-node-from-type
         :load
         (list dep)
         (make-grain-from-file build-file-path :build-p t)
         previous-nodes-map
         previous-nodes-list
         build-grain)))))

(defmethod create-dependency-node-from-type ((dependency-type (eql :compile))
                                             dependency
                                             build-grain
                                             previous-nodes-map
                                             previous-nodes-list)
  (destructuring-bind (dep-name) dependency
    (create-cfasl-node (grain-from-name dep-name build-grain)
                       previous-nodes-map
                       previous-nodes-list)))

;;NOTE - This should currently never be used.  cfasl-or-fasl node functionality
;;has not yet been implemented
(defmethod create-dependency-node-from-type ((dependency-type
                                              (eql :cfasl-or-fasl))
                                             dependency
                                             build-grain
                                             previous-nodes-map
                                             previous-nodes-list)
  (destructuring-bind (dep-name) dependency
    (make-instance 'cfasl-or-fasl-node
      :fasl-node (create-fasl-node (grain-from-name dep-name
                                                     build-grain)
                                   previous-nodes-map
                                   previous-nodes-list)
      :cfasl-node (create-cfasl-node (grain-from-name dep-name
                                                       build-grain)
                                     previous-nodes-map
                                     previous-nodes-list))))

(defmethod create-dependency-node-from-type ((dependency-type (eql :asdf))
                                             dependency
                                             build-grain
                                             previous-nodes-map
                                             previous-nodes-list)
  (destructuring-bind (dep-name) dependency
    (create-asdf-system-node dep-name)))

(defmethod create-dependency-node-from-type ((dependency-type (eql :load-source))
                                             dependency
                                             build-grain
                                             previous-nodes-map
                                             previous-nodes-list)
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
               (grain
                  (create-fasl-node
                   dep previous-nodes-map previous-nodes-list))
               (otherwise
                  (create-dependency-node dep
                                          *build-grain*
                                          previous-nodes-map
                                          previous-nodes-list)))))
      (let* ((dependencies (mapcar #'f dependencies))
             (fullname (or name
                           (format nil "//lisp-image/~{~A~^//~}"
                                   (mapcar #'fullname dependencies))))
             (lisp-image-node (make-instance 'lisp-image-node
                                :fullname fullname
                                :build-grain *build-grain*
                                :compile-dependencies (reverse dependencies))))
        lisp-image-node))))


(defun create-source-file-node (grain)
  "This function constructs a source-file-node in the dependency graph"
  (let ((fullname
         (namestring (fullname grain))))
    ;;If this node already exists, don't recreate it
    (or (gethash fullname *grains*)
        (let* ((source-node (make-instance 'source-file-node
                              :fullname fullname
                              :build-grain (if (typep grain 'build-grain)
                                              grain
                                              (build-grain grain))
                              :source-filepath (grain-pathname grain))))
          (setf (gethash fullname *grains*) source-node)))))

(defun create-asdf-system-node (system-name)
  "This function constructs a asdf-system-node in the dependency graph"
  (let ((fullname (strcat "//asdf/" system-name)))
    (or (gethash fullname *grains*)
        (setf (gethash fullname *grains*)
              (make-instance 'asdf-system-node
                :name (coerce-asdf-system-name system-name)
                :fullname fullname)))))

(defun create-load-source-node (source-file-name)
  "This function constructs a load-source-node in the dependency graph"
  (let* ((truename (truename source-file-name)) ;NUN
	 (fullname (strcat "//load-source/" (namestring truename)))) ;NUN
    (or (gethash fullname *grains*)
        (setf (gethash fullname *grains*)
              (make-instance 'load-source-node
                :source-filepath truename
                :fullname fullname)))))

(defun create-image-dump-node (lisp-image-node dump-path)
  "This function constructs an image-dump-node in the dependency graph, which is designed to dump an image of the lisp state described by lisp-image-node"
  (make-instance 'image-dump-node
    :dump-path dump-path
    :lisp-image lisp-image-node
    :build-grain *build-grain*
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
                                   grain
                                   previous-nodes-map
                                   previous-nodes-list)
   "This function takes a fasl-node and its grain and builds nodes for all of
that grain's dependencies, and adds them as dependencies of the fasl-node"
  (add-dependencies fasl-node (mapcar
                               (lambda (name) (create-dependency-node
                                               name
                                               (build-grain fasl-node)
                                               previous-nodes-map
                                               previous-nodes-list))
                               (load-depends-on grain)) :type :load)
  (add-dependencies fasl-node (mapcar
                               (lambda (name) (create-dependency-node
                                               name
                                               (if (typep grain 'build-grain)
                                                 grain
                                                 (build-grain grain))
                                               previous-nodes-map
                                               previous-nodes-list))
                               (compile-depends-on grain)) :type :compile)
  ;;Add dependency on the lisp source file
  (add-dependency fasl-node (create-source-file-node grain) :type :compile))

(defun set-cfasl-node-dependencies (cfasl-node
                                    grain
                                    previous-nodes-map
                                    previous-nodes-list)
  "This function takes a cfasl-node and its grain and builds nodes for all of
that grain's dependencies, and adds them as dependencies of the cfasl-node"
  (let ((dependencies (mapcar
                       (lambda (name) (create-dependency-node
                                       name
                                       (build-grain cfasl-node)
                                       previous-nodes-map
                                       previous-nodes-list))
                       (compile-depends-on grain))))
    (add-dependencies cfasl-node dependencies :type :compile)
    (add-dependencies cfasl-node dependencies :type :load)
    ;;Add dependency on the lisp source file
    (add-dependency cfasl-node (create-source-file-node grain) :type :compile)))

(defun create-fasl-node (grain previous-nodes-map previous-nodes-list)
  "This function constructs a fasl-node in the dependency graph.  It also
builds dependency-graph-nodes for any of its dependencies."
  (let ((fullname (namestring ;NUN
                   (make-pathname :type "fasl" :defaults (fullname grain)))))
    ;(format T "building fasl file node with name: ~a.~%" fullname)
    (with-catching-dependency-cycle
        (fullname previous-nodes-map previous-nodes-list)
      (let ((existing-node (gethash fullname *grains*)))
        (or existing-node ;If this node already exists, don't re-create it.
            (let* ((fasl-node (make-instance 'fasl-node
                                :fullname fullname
                                :build-grain (if (typep grain 'build-grain)
                                                grain
                                                (build-grain grain))
                                :source-filepath (grain-pathname grain))))
              (set-fasl-node-dependencies fasl-node
                                          grain
                                          previous-nodes-map
                                          previous-nodes-list)
              (setf (gethash fullname *grains*) fasl-node)))))))


(defun create-cfasl-node (grain previous-nodes-map previous-nodes-list)
  "This function constructs a cfasl-node in the dependency graph.  It also
builds dependency-graph-nodes for any of its dependencies."
  (let ((fullname (namestring ;NUN
                   (make-pathname :type "cfasl" :defaults (fullname grain)))))
    ;(format T "building cfasl file node with name: ~a.~%" fullname)
    (with-catching-dependency-cycle
        (fullname previous-nodes-map previous-nodes-list)
      (let ((existing-node (gethash fullname *grains*)))
        (or existing-node ;If this node already exists, don't re-create it.
            (let* ((cfasl-node (make-instance 'cfasl-node
                                 :fullname fullname
                                 :build-grain (if (typep grain 'build-grain)
                                                 grain
                                                 (build-grain grain))
                                 :source-filepath (grain-pathname grain))))
              (set-cfasl-node-dependencies cfasl-node
                                           grain
                                           previous-nodes-map
                                           previous-nodes-list)
              (setf (gethash fullname *grains*) cfasl-node)))))))


(defun create-dump-image-graph (imagepath sourcepath)
  "Constructs a dependency graph to dump a lisp image at imagepath with the
lisp file at sourcepath loaded"
  (create-image-dump-node (create-dependency-graph sourcepath) imagepath))

(defun create-dependency-graph (sourcepath)
  "Constructs a dependency graph with a lisp-image-node for a lisp image
with the given lisp file loaded as the root of the graph."
  (setf sourcepath (merge-pathnames sourcepath "BUILD.lisp"))
  (create-lisp-image-node (list (make-grain-from-file sourcepath))))

(defun xcvb-setup-dependencies ()
  (mapcar (lambda (x)
	    `(:load-source
	      ,(merge-pathnames x *xcvb-lisp-directory*)))
	  *xcvb-setup-dependencies*))
