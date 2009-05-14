(in-package :xcvb)

(defvar *visited-nodes* nil
  "A map of nodes that have already been visited
when looking for asdf-systems in the dependency graph")

(defvar *written-nodes* nil
  "A map of nodes that have already been written to the asd file.")

(defvar *output-path* nil
  "The path that the asd file is being written to.
All filepaths that show up in the asd file will be relative to this path.")


(defgeneric find-asdf-systems (node)
  (:documentation "Helper generic function for find-asdf-systems.
Returns a list of the names of all the asdf-systems that this node depends on
(or that any of its dependencies transitively depends on)"))

(defmethod find-asdf-systems :around ((node dependency-graph-node))
  ;; If this node has already been looked at, don't look at it again.
  (unless (gethash (fullname node) *visited-nodes*)
    ;; Add this node to the map of nodes already visited.
    (setf (gethash (fullname node) *visited-nodes*) t)
    (call-next-method)))

(defmethod find-asdf-systems ((node asdf-system-node))
  (list (name node)))

(defmethod find-asdf-systems ((node dependency-graph-node-with-dependencies))
  (remove-duplicates
   (mapcan (lambda (dependency-node)
             (find-asdf-systems dependency-node))
           (append (compile-dependencies node) (load-dependencies node)))
   :test #'equal))

(defmethod find-asdf-systems ((node image-dump-node))
  (find-asdf-systems (lisp-image node)))

(defmethod find-asdf-systems ((node dependency-graph-node))
  nil)


(defun write-asdf-system-header (filestream asdf-systems
                                 &optional (build-grain *build-grain*))
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


(defmethod write-node-to-asd-file (filestream (node lisp-image-node))
  (dolist (dep (append (compile-dependencies node)
                       (load-dependencies node)))
    (write-node-to-asd-file filestream dep)))

(defmethod write-node-to-asd-file (filestream (node object-file-node))
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


(defmethod write-node-to-asd-file (filestream (node dependency-graph-node))
  (declare (ignore filestream node)))


(defun write-asd-file (source-path output-path)
  "Writes an asd file to output-path
that can be used to compile the file at source-path with asdf"
  (with-open-file (out output-path :direction :output :if-exists :supersede)
    (let* ((dependency-graph (create-dependency-graph source-path))
           (build-requires-graph (create-lisp-image-node
                                  (build-requires *build-grain*)))
           (*visited-nodes* (make-hash-table :test #'equal))
           (asdf-systems (append
                          (find-asdf-systems build-requires-graph)
                          (find-asdf-systems dependency-graph)))
           (*output-path* output-path))
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
      (let* ((system-name (namestring ;NUN
                           (make-pathname :name nil
                                          :type nil
                                          :defaults (fullname *build-grain*))))
             ;;This is fragile!
             (system-name (subseq system-name 1 (- (length system-name) 1))))
        (format out "~12,0T)))~%~%(cl:pushnew :~A *features*)" system-name)))))
