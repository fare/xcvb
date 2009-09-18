#+xcvb (module (:depends-on
		("specials" "lisp-grain" "traversal" "logging")))
(in-package :xcvb)

;;; TODO: rewrite this file according to the new XCVB internals
;;; i.e. create a file asdf-backend based on makefile-backend,
;;; and based on the output of static-backend, create an asd.
;;; If any ASDF extensions are required, then
;;; (a) have a system "xcvb-extensions.asd" that provides them
;;; (b) push these extensions for inclusion in upstream ASDF, or
;;; (c) just punt and have ASDF delegate to our in-image backend (if/when implemented)

#|
OUT-OF-DATE: should be rewritten as part of our refactoring.

This file still contains bits from our v0.1-prototype
that used to take a dependency graph in the old format
and write an asd file that can be used by asdf
to compile the system specified by the graph.
The entry point for this file was the write-asd-file function.
The generated asd file would have one asdf module
to load all the files/systems in the build-requires slot
of the global build-module,
and another module -- which depends on the first module --
for all the other files/systems.

The conversion to ASDF is lossy and the Makefile target
should be preferred for development purposes,
but the ASDF target should be good enough for deployment purposes
when providing backwards compatibility with ASDF projects.

|#

(defclass asdf-traversal (xcvb-traversal)
  ())

(defmethod issue-load-command ((env asdf-traversal) command)
  (declare (ignorable env command))
  (values))

(defvar *asdf-systems-superseded* (make-hashset :test 'equal)
  "A list of asdf system we supersede")

(defvar *asdf-system-dependencies* nil
  "A list of asdf system we depend upon")

(defmethod issue-dependency ((env asdf-traversal) (grain lisp-grain))
  (let* ((build (build-grain-for grain)))
    (if (build-in-asdf-target-p build)
        (call-next-method)
        (dolist (system (supersedes-asdf build))
          (pushnew system *asdf-system-dependencies* :test 'equal))))
  (values))

(defmethod issue-dependency ((env asdf-traversal) (grain fasl-grain))
  (issue-dependency env (graph-for env `(:lisp ,(second (fullname grain))))))

(defmethod load-command-for-lisp ((env asdf-traversal) name)
  (load-command-for-fasl env name))

(define-graph-for :fasl ((env asdf-traversal) name)
  (check-type name string)
  (let* ((grain (resolve-absolute-module-name name))
	 (fullname (if grain (fullname grain) (error "Couldn't resolve ~S to a lisp module" name)))
	 (generator (gethash fullname *generators*)))
    (check-type grain lisp-grain)
    (handle-lisp-dependencies grain)
    (let ((dependencies
           (append (build-dependencies grain)
                   (when generator (generator-dependencies generator))
                   (compile-dependencies grain)
                   (load-dependencies grain))))
      (load-command-for* env dependencies)
      (make-grain 'fasl-grain :fullname `(:fasl ,name)
                  :load-dependencies (traversed-dependencies env)))))

(define-graph-for :lisp ((env asdf-traversal) name)
  (resolve-absolute-module-name name))

(defun build-in-asdf-target-p (build)
  (let ((asdfs (supersedes-asdf build)))
    (and asdfs (intersection asdfs *asdf-systems-superseded* :test 'equal) t)))

(defmethod graph-for-build-grain ((env asdf-traversal) grain)
  (let ((asdfs (supersedes-asdf grain)))
    (cond
      ((build-in-asdf-target-p grain)
       (load-command-for* env (compile-dependencies grain))
       (load-command-for* env (load-dependencies grain)))
      (asdfs
       (dolist (system asdfs)
         (pushnew system *asdf-system-dependencies* :test 'equal)))
      (t
       (error "Targets ~S depend on ~S but it isn't in an ASDF"
              (traversed-dependencies-r env) (fullname grain))))))

(define-graph-for :asdf ((env asdf-traversal) system-name)
  (unless (member system-name *asdf-systems-superseded* :test 'equal)
    (pushnew system-name *asdf-system-dependencies* :test 'equal))
  (make-asdf-grain :name system-name
                   :implementation *lisp-implementation-type*))


#|

(defvar *output-path* nil
  "The path that the asd file is being written to.
All filepaths that show up in the asd file will be relative to this path.")

(defun write-asdf-system-header (filestream asdf-systems
                                 &optional build-grain)
  "Writes the information from the build grain to the asdf file"
  (let* ((fullname (fullname build-grain))
         (pathname (portable-pathname-from-string fullname :allow-relative nil))
         (system-name (string-downcase (second (pathname-directory pathname)))))
    (format filestream "~&(asdf:defsystem :~A~%" system-name))
  (dolist (slot '(author maintainer version licence description long-description))
    (when (slot-boundp build-grain slot)
      (format filestream "~2,0T:~(~A~) ~S~%" slot (slot-value build-grain slot))))
  (if asdf-systems
    (format filestream "~2,0T:depends-on (~{:~A~^ ~})~%" asdf-systems)))


(defgeneric write-node-to-asd-file (filestream node)
  (:documentation "Writes information about the given node and its dependencies
to the filestream that can be put in the components section of an asd file"))

(defmethod write-node-to-asd-file (filestream node) ;;object-file-node
  (unless (or (gethash (namestring (make-pathname :type "fasl"
                                                  :defaults (fullname node)))
                       *written-nodes*) ;NUN
              (gethash (namestring (make-pathname :type "cfasl"
                                                  :defaults (fullname node)))
                       *written-nodes*));If this node has already been written to the makefile, don't write it again. ;NUN
    (setf (gethash (fullname node) *written-nodes*) t);Add this node to the map of nodes already written to the makefile
    (let ((dependencies (union (rest (compile-dependencies node))
                               (load-dependencies node))))
      (dolist (dep dependencies)
        (write-node-to-asd-file filestream dep))
      (format filestream "~13,0T(:file ~s~@[ :depends-on ~s~])~%"
              (namestring (make-pathname
                           :type nil
                           :defaults (enough-namestring (source-filepath node)
                                                        *output-path*))) ;NUN?
              (mapcar
               (lambda (node) (namestring (make-pathname
                                           :type nil
                                           :defaults (enough-namestring
                                                      (source-filepath node)
                                                      *output-path*)))) ;NUN?
               (remove-if-not (lambda (dep) (typep dep 'object-file-node))
                              dependencies))))))



(defun write-asd-file (&key build-names output-path asdf-name)
  "Writes an asd file to output-path
that can be used to compile the system of given fullname.
Declare asd system as ASDF-NAME."
  (let* ((*use-cfasls* nil)
         (asdf-name (coerce-asdf-system-name
                     (or asdf-name (first (supersedes-asdf
                                           (registered-build (first build-names))))))))
    (dolist (name build-names)
      (graph-for (make-instance 'asdf-traversal) name))
         (asdf-systems (append
                        (find-asdf-systems build-requires-graph)
                        (find-asdf-systems dependency-graph)))
         (*output-path* output-path))
      (with-open-file (out output-path :direction :output :if-exists :supersede)
      (write-asdf-system-header out asdf-systems)
      (format out
              "~2,0T:components~%~2,0T((:grain \"build-requires-files\"~%~
~12,0T:pathname #p\".\"~%~12,0T:components~%~12,0T(")
      (let ((*written-nodes* (make-hash-table :test #'equal)))
        (write-node-to-asd-file out build-requires-graph))
      (format out
              "~12,0T))~%~3,0T(:grain \"main-files\"~%~
~12,0T:pathname #p\".\"~%~12,0T:depends-on(\"build-requires-files\")~%~
~12,0T:components~%~12,0T(")
      ;; TODO: explain the issue with nodes that appear twice
      (let ((*written-nodes* (make-hash-table :test #'equal)))
        (write-node-to-asd-file out dependency-graph))
      (let* ((system-name output-path))
        (format out "~12,0T)))~%" system-name)))))
|#
