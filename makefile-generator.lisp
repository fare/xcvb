(in-package :xcvb)


(defun escape-string-for-Makefile (string)
  "Takes a string and excapes all the characters that need to be to be put into a makefile.  These characters are $ \\.  Raises an error if the string contains a newline"
  (with-output-to-string (out-string)
    (loop for c across string do
      (case c 
        (#\newline (error "Makefile line cannot contain a newline"))
        (#\$ (format out-string "$$"))
        (#\\ (format out-string "\\"));Not currently doing anything!
        (otherwise (format out-string "~a" c))))))

  
(defun escape-string-for-shell (string)
  "Takes a string and excapes all the characters that need to be to be run in the shell.  These characters are \" $ ` \\"
  (with-output-to-string (out-string)
    (loop for c across string do
	 (case c
	   ((#\" #\` #\$ #\\) (format out-string "\\~a" c))
	   (otherwise (format out-string "~a" c))))))
;  (reduce (lambda (c rest) (if (member c (list #\" #\$ #\` #\\)) (strcat "\\" (string c) rest) (strcat (string c) rest))) string :from-end T :initial-value ""))

#|
(defun command-list-to-shell-command (command-list)
  "Converts the list of commands to a single string that has been escape for the shell"
  (format nil "~{~A~^ ~}" (mapcar #'escape-string-for-shell command-list)))

(defun command-list-to-Makefile-line (command-list)
  "Converts the list of commands to a single string that can be used to execute shell commands through make"
  (shell-command-to-Makefile-line (command-list-to-shell-command command-list)))
|#

(defgeneric load-node-form-string (node)
  (:documentation "Returns the string corresponding to the proper lisp form to load the given node"))

(defmethod load-node-form-string ((node fasl-file-node))
  (format nil "(load \"~a\")" (namestring (merge-pathnames *buildpath* (target node)))))

(defmethod load-node-form-string ((node asdf-system-node))
  (format nil "(asdf:oos 'asdf:load-op :~a)" (namestring (name node))))


(defgeneric lisp-forms-string-for-node (node)
  (:documentation "Returns a string of the lisp forms that will create what the node represents.  For example, for a fasl-file-node, it will load all the node's dependencies, then compile the source file, thus creating the fasl."))

(defmethod lisp-forms-string-for-node ((node fasl-file-node))
  (format nil "~{~a ~^~}(compile-file \"~a\")" (mapcar #'load-node-form-string (rest (dependencies node))) (namestring (merge-pathnames *buildpath* (target (first (dependencies node)))))))

(defmethod lisp-forms-string-for-node ((node lisp-node))
  (format nil "~{~a~^ ~}" (mapcar #'load-node-form-string (dependencies node))))

(defmethod lisp-forms-string-for-node ((node image-dump-node))
  (lisp-forms-string-for-node (lisp-image node)))

(defmethod lisp-forms-string-for-node ((node asdf-system-node))
  (load-node-form-string node))
  

(defgeneric progn-form-string-for-node (node)
  (:documentation "Returns the progn of lisp forms that will create the target of the node"))

(defmethod progn-form-string-for-node ((node dependency-graph-node))
  (format nil "(progn ~a (sb-ext:quit))" (lisp-forms-string-for-node node)))

(defmethod progn-form-string-for-node ((node image-dump-node))
  (format nil "(progn ~a (save-lisp-and-die \"~a\"))" (lisp-forms-string-for-node node) (merge-pathnames *buildpath* (target node))))


(defgeneric eval-command-string (lisp-type lisp-form-string)
  (:documentation "Returns the command string required to eval the given lisp form in the given lisp implementation, with characters properly escaped for running a shell command in a makefile"))

(defmethod eval-command-string ((lisp-type (eql :sbcl)) lisp-form-string)
  (with-output-to-string (out-string)
    (format out-string "~a ~a ~{~a ~^~}~a" 
	    (escape-string-for-makefile (escape-string-for-shell *lisp-executable-pathname*))
	    (if *lisp-image-pathname* 
              (escape-string-for-makefile (escape-string-for-shell (list "--core" *lisp-image-pathname*)))
              "")
	    (mapcar (lambda (string) (escape-string-for-makefile (escape-string-for-shell string))) *lisp-options*)
	    (format nil "--eval \"~a\"" (escape-string-for-makefile (escape-string-for-shell lisp-form-string))))))


(defun makefile-line-for-node (node)
  "Returns the string of the line in the Makefile that can create the target of the given node"
  ;(command-list-to-Makefile-line (eval-command-list *lisp-implementation* (progn-form-string-for-node 
  (eval-command-string *lisp-implementation* (progn-form-string-for-node node)))

(defgeneric write-makefile (filestream node written-nodes)
  (:documentation "Writes the dependency graph to a makefile"))

(defmethod write-makefile (filestream (node fasl-file-node) written-nodes)
  (unless (nth-value 1 (gethash (fullname node) written-nodes));If this node has already been written to the makefile, don't write it again.
    (setf (gethash (fullname node) written-nodes) nil);Add this node to the map of nodes already written to the makefile
    (mapcar (lambda (dependency-node) (write-makefile filestream dependency-node written-nodes)) (dependencies node));Write the information for the node's depedencies first since they have to be compiled first anyway
    (format filestream "~%~%~a : ~{~a~^ ~}~%" (target node) (mapcar #'target (dependencies node)))
    (format filestream "~a~a" #\tab (makefile-line-for-node node))))
    ;(format filestream "~%~T~a ~a " (fasl-compiler) (target node))
    ;(mapcar (lambda (dependency-node) (format filestream " ~a" (target dependency-node))) (dependencies node))))
    

(defmethod write-makefile (filestream (node lisp-node) written-nodes)
  (mapcar (lambda (dependency-node) (write-makefile filestream dependency-node written-nodes)) (dependencies node))
  (format filestream "~%~%~a : ~{~a~^ ~}~%" (target node) (mapcar #'target (dependencies node)))
  (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node))
  (format filestream ".PHONY: ~a" (target node)))
  ;(format filestream "~T${lispbuilder} ~a" (target node))
  ;(mapcar (lambda (dependency-node) (format filestream " ~a" (target dependency-node))) (dependencies node)))
  

(defmethod write-makefile (filestream (node image-dump-node) written-nodes)
  (write-makefile filestream (lisp-image node) written-nodes)
  (format filestream "~%~%~a : ~a" (target node) (target (lisp-image node)))
  (format filestream "~%~a~a" #\tab (makefile-line-for-node node)))
  ;(format filestream "~%~T${imagedumper} ~a ~a" (target node) (target (lisp-image node))))
  

(defmethod write-makefile (filestream (node asdf-system-node) written-nodes)
  (unless (nth-value 1 (gethash (fullname node) written-nodes))
    (setf (gethash (fullname node) written-nodes) nil)
    (format filestream "~%~%~a :~%" (target node))
    (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node))
    (format filestream ".PHONY: ~a" (target node))))
    
    ;(format filestream "~%~T${asdfcompiler} ~a" (target node))))

(defmethod write-makefile (filestream (node source-file-node) written-nodes)
  (declare (ignore filestream node written-nodes)));Since source-file-nodes have no dependencies and should already exist, don't write anything about them to the makefile.

