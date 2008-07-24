(in-package :xcvb)

(defvar *visited-nodes*)
(defvar *build-requires-written-nodes*)
(defvar *main-files-written-nodes*)
(defvar *writing-build-requires-module*)
(defvar *build-requires-graph*)

(defun find-asdf-systems (dependency-graph)
  "Returns a list of the names of all the asdf-systems that any node in the dependency-graph depends on"
  (find-asdf-systems-helper dependency-graph))


(defgeneric find-asdf-systems-helper (node)
  (:documentation "Helper generic function for find-asdf-systems.  Returns a list of the names of all the asdf-systems that this node depends on (or any of its dependencies depend on)"))

(defmethod find-asdf-systems-helper ((node asdf-system-node))
  (list (strcat ":" (name node))))

(defmethod find-asdf-systems-helper ((node dependency-graph-node-with-dependencies))
  (reduce 
   (lambda (dependency-node rest) (nunion (find-asdf-systems-helper dependency-node) rest :test #'equal)) 
   (nconc (compile-dependencies node) (load-dependencies node)) :initial-value nil :from-end T))

(defmethod find-asdf-systems-helper :around ((node dependency-graph-node))
  (unless (nth-value 1 (gethash (fullname node) *visited-nodes*));If this node has already been looked at, don't look at it again.
    (setf (gethash (fullname node) *visited-nodes*) nil);Add this node to the map of nodes already visited while looking for the asdf dependencies
    (call-next-method)))

(defmethod find-asdf-systems-helper ((node image-dump-node))
  (find-asdf-systems-helper (lisp-image node)))

(defmethod find-asdf-systems-helper ((node dependency-graph-node))
  nil)


(defun write-asdf-system-header (filestream dependency-graph &optional (build-module *build-module*))
  "Writes the information from the build module to the asdf file"
  (let* ((system-name (namestring (make-pathname :name nil :type nil :defaults (fullname build-module))))
         (system-name (subseq system-name 1 (- (length system-name) 1))))
    (format filestream "(asdf:defsystem :~a~%" system-name))
  (if (author build-module)
    (format filestream "~2,0T:author ~s~%" (author build-module)))
  (if (maintainer build-module)
    (format filestream "~2,0T:maintainer ~s~%" (maintainer build-module)))
  (if (version build-module)
    (format filestream "~2,0T:version ~s~%" (version build-module)))
  (if (licence build-module)
    (format filestream "~2,0T:licence ~s~%" (licence build-module)))
  (if (description build-module)
    (format filestream "~2,0T:description ~s~%" (description build-module)))
  (if (long-description build-module)
    (format filestream "~2,0T:long-description ~s~%" (long-description build-module)))

  (let* ((*visited-nodes* (make-hash-table :test #'equal))
         (asdf-systems (nconc 
                        (find-asdf-systems-helper *build-requires-graph*) 
                        (find-asdf-systems dependency-graph))))
    (if asdf-systems
      (format filestream "~2,0T:depends-on~a~%" asdf-systems))))


(defgeneric write-node-to-asd-file (filestream node)
  (:documentation "Writes information about the given node and its dependencies to the filestream that can be put in the components section of an asd file"))
  

(defmethod write-node-to-asd-file (filestream (node lisp-node))
  (dolist (dependency (nconc (compile-dependencies node) (load-dependencies node))) 
    (write-node-to-asd-file filestream dependency)))

(defmethod write-node-to-asd-file (filestream (node fasl-or-cfasl-node))
  (let ((written-nodes (if *writing-build-requires-module* *build-requires-written-nodes* *main-files-written-nodes*)))
    (unless (or (nth-value 1 (gethash (namestring (make-pathname :type "fasl" :defaults (fullname node))) written-nodes))
                (nth-value 1 (gethash (namestring (make-pathname :type "cfasl" :defaults (fullname node))) written-nodes)));If this node has already been written to the makefile, don't write it again.
      (setf (gethash (fullname node) written-nodes) nil);Add this node to the map of nodes already written to the makefile
      (let ((dependencies (if *writing-build-requires-module*
                            (nunion (rest (compile-dependencies node)) (load-dependencies node))
                            (remove-if 
                             (lambda (dep) (nth-value 1 (gethash (fullname dep) *build-requires-written-nodes*))) 
                             (nunion (rest (compile-dependencies node)) (load-dependencies node))))))
        (when dependencies
          (dolist (dep dependencies)
            (write-node-to-asd-file filestream dep)))
        (format filestream "~13,0T(:file ~s~@[ :depends-on ~s~])~%"
                (name node)
                (mapcar #'name (remove-if-not (lambda (dep) (typep dep 'fasl-or-cfasl-node)) dependencies)))))))


(defmethod write-node-to-asd-file (filestream (node dependency-graph-node))
  (declare (ignore filestream node)))


(defun write-asd-file (source-path output-path)
  "Writes an asd file to output-path that can be used to compile the file at source-path with asdf"
  (with-open-file (out output-path :direction :output :if-exists :supersede)
    (let ((dependency-graph (create-dependency-graph source-path))
          (*build-requires-written-nodes* (make-hash-table :test #'equal))
          (*main-files-written-nodes* (make-hash-table :test #'equal))
          (*build-requires-graph* (create-lisp-node (build-requires *build-module*))))
      (write-asdf-system-header out dependency-graph)
      (let ((*writing-build-requires-module* T))
        (format out "~2,0T:components~%~2,0T((:module \"build-requires-files\"~%~12,0T:components~%~12,0T(")
        (write-node-to-asd-file out *build-requires-graph*))
      (let ((*writing-build-requires-module* nil))
        (format out "~12,0T))~%~3,0T(:module \"main-files\"~%~12,0T:depends-on(\"build-requires-files\")~%~12,0T:components~%~12,0T(")
        (write-node-to-asd-file out dependency-graph))
      (let* ((system-name (namestring (make-pathname :name nil :type nil :defaults (fullname *build-module*))))
         (system-name (subseq system-name 1 (- (length system-name) 1))))
        (format out "~12,0T)))~%~%(cl:pushnew :~a *features*)" system-name)))))
