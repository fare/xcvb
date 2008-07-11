(in-package :xcvb)

(defvar *visited-nodes* nil)


(defgeneric find-asdf-systems (node)
  (:documentation "Returns a list of the names of all the asdf-systems that this node depends on (or any of its dependencies depend on)"))


(defmethod find-asdf-systems ((node asdf-system-node))
  (format T "found system \"~a\"~%" (name node))
  (list (strcat ":" (name node))))

(defmethod find-asdf-systems ((node dependency-graph-node-with-dependencies))
  (format T "Finding dependencies for node \"~a\"~%" (fullname node))
  (reduce (lambda (dependency-node rest) (nunion (find-asdf-systems dependency-node) rest :test #'equal)) (nconc (compile-dependencies node) (load-dependencies node)) :initial-value nil :from-end T))

(defmethod find-asdf-systems :around ((node dependency-graph-node))
  (unless (nth-value 1 (gethash (fullname node) *visited-nodes*));If this node has already been looked at, don't look at it again.
    (setf (gethash (fullname node) *visited-nodes*) nil);Add this node to the map of nodes already visited while looking for the asdf dependencies
    (call-next-method)))

(defmethod find-asdf-systems ((node dependency-graph-node))
  nil)


(defun write-asdf-system-header (filestream dependency-graph &optional (build-module *build-module*))
  (format filestream "(asdf:defsystem :~a~%" (subseq (fullname build-module) 1))
  (if (author build-module)
    (format filestream "~T:author ~s~%" (author build-module)))
  (if (maintainer build-module)
    (format filestream "~T:maintainer ~s~%" (maintainer build-module)))
  (if (version build-module)
    (format filestream "~T:version ~s~%" (version build-module)))
  (if (licence build-module)
    (format filestream "~T:licence ~s~%" (licence build-module)))
  (if (description build-module)
    (format filestream "~T:description ~s~%" (description build-module)))
  (if (long-description build-module)
    (format filestream "~T:long-description ~s~%" (long-description build-module)))

  (format T "finding asdf systems...")
  (let* ((*visited-nodes* (make-hash-table :test #'equal))
         (asdf-systems (find-asdf-systems dependency-graph)))
    (format T "done")
    (if asdf-systems
      (format filestream "~T:depends-on~a~%" asdf-systems))))

(defgeneric write-node-to-asd-file (filestream node written-nodes)
  (:documentation "Writes information about the given node and its depencendies to the filestream that can be put in the components section of an asd file"))
  

(defmethod write-node-to-asd-file (filestream (node lisp-node) written-nodes)
  (mapcar (lambda (dependency) (write-node-to-asd-file filestream dependency written-nodes)) (nconc (compile-dependencies node) (load-dependencies node))))

(defmethod write-node-to-asd-file (filestream (node fasl-node) written-nodes)
  (unless (nth-value 1 (gethash (fullname node) written-nodes));If this node has already been written to the asd file, don't write it again.
    (setf (gethash (fullname node) written-nodes) nil);Add this node to the map of nodes already written to the asd file
    ;(format T "Writing asd file line for node: ~a~%" (fullname node))
    (let ((dependencies (nunion (rest (compile-dependencies node)) (load-dependencies node))))
      (if dependencies
        (progn 
          (mapcar (lambda (dependency) (write-node-to-asd-file filestream dependency written-nodes)) dependencies)
          (format filestream "(:file ~s :depends-on ~s)~%~T~T" (name node) (reduce (lambda (dependency-node rest) (if (typep dependency-node 'fasl-node) (push (name dependency-node) rest) rest)) dependencies :initial-value nil :from-end T))) 
        (progn
          (format filestream "(:file ~s)~%~T~T" (name node)))))))
          ;(format T "Finished writing asd file line for: ~a~%" (fullname node)))))))

(defmethod write-node-to-asd-file (filestream (node dependency-graph-node) written-nodes)
  (declare (ignore filestream node written-nodes)))




(defun write-asd-file (source-path output-path)
  (with-open-file (out output-path :direction :output :if-exists :supersede)
    (let ((dependency-graph (create-dependency-graph source-path))
          (written-nodes (make-hash-table :test #'equal)))
      (write-asdf-system-header out dependency-graph)
      (format out "~T:components~%~T(")
      (write-node-to-asd-file out dependency-graph written-nodes)
      (format out "))~%~%(cl:pushnew :~a *features*)" (subseq (fullname *build-module*) 1)))))