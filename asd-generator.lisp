(in-package :xcvb)

(defvar *visited-nodes*)

(defun write-asdf-system-header (filestream &optional (build-module *build-module*))
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
    (format filestream "~T:long-description ~s~%" (long-description build-module))))

(defgeneric write-asdf-file (filestream node written-nodes)
  (:documentation "Writes the dependency graph to an asdf file that can be used to compile it in the proper order"))

(defmethod write-asdf-file (filestream (node lisp-node) written-nodes)
  (write-asdf-system-header filestream)
  (format T "finding asdf systems...")
  (let* ((*visited-nodes* (make-hash-table :test #'equal))
         (asdf-systems (find-asdf-systems node)))
    (format T "done")
    (if asdf-systems
      (format filestream "~T:depends-on~a~%" asdf-systems)))
  (format filestream "~T:components~%~T(")
  (mapcar (lambda (dependency) (write-asdf-file filestream dependency written-nodes)) (dependencies node))
  (format filestream "))~%~%(cl:pushnew :~a *features*)" (subseq (fullname *build-module*) 1)))
  
(defmethod write-asdf-file (filestream (node fasl-file-node) written-nodes)
  (unless (nth-value 1 (gethash (fullname node) written-nodes));If this node has already been written to the asd file, don't write it again.
    (setf (gethash (fullname node) written-nodes) nil);Add this node to the map of nodes already written to the asd file
    (if (rest (dependencies node))
      (progn 
        (mapcar (lambda (dependency) (write-asdf-file filestream dependency written-nodes)) (dependencies node))
        (format filestream "(:file ~s :depends-on ~s)~%~T~T" (name node) (reduce (lambda (dependency-node rest) (if (typep dependency-node 'fasl-file-node) (push (name dependency-node) rest) rest)) (dependencies node) :initial-value nil :from-end T)))
      (format filestream "(:file ~s)~%~T~T" (name node)))))

(defmethod write-asdf-file (filestream (node dependency-graph-node) written-nodes)
  (declare (ignore filestream node written-nodes)))


(defgeneric find-asdf-systems (node)
  (:documentation "Returns a list of the names of all the asdf-systems that this node depends on (or any of its dependencies depend on)"))


(defmethod find-asdf-systems ((node asdf-system-node))
  (format T "found system \"~a\"~%" (name node))
  (list (strcat ":" (name node))))

(defmethod find-asdf-systems ((node dependency-graph-node-with-dependencies))
  (format T "Finding dependencies for node \"~a\"~%" (fullname node))
  (reduce (lambda (dependency-node rest) (nunion (find-asdf-systems dependency-node) rest :test #'equal)) (dependencies node) :initial-value nil :from-end T))

(defmethod find-asdf-systems :around (node)
  (unless (nth-value 1 (gethash (fullname node) *visited-nodes*));If this node has already been looked at, don't look at it again.
    (setf (gethash (fullname node) *visited-nodes*) nil);Add this node to the map of nodes already visited while looking for the asdf dependencies
    (call-next-method)))

(defmethod find-asdf-systems ((node dependency-graph-node))
  nil)
