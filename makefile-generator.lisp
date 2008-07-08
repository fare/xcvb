(in-package :xcvb)

(defparameter *build-depends-on-asdf-systems-p* nil
  "Flag to specify if the build file has asdf dependencies, and therefore whether or not to have a core-with-asdf-systems-loaded target")

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

(defun escape-string (string)
  (escape-string-for-makefile (escape-string-for-shell string)))




(defgeneric eval-command-string (lisp-type &key lisp-executable lisp-image lisp-options cwasl-as-core)
  (:documentation "Returns the command string required to eval the given lisp form in the given lisp implementation, with characters properly escaped for running a shell command in a makefile"))

(defmethod eval-command-string ((lisp-type (eql :sbcl)) &key (lisp-executable *lisp-executable-pathname*) (lisp-image *lisp-image-pathname*) (lisp-options *lisp-options*) cwasl-as-core)
  (format nil "~a~a ~{~a ~^~}~a" 
          (escape-string (namestring lisp-executable))
          (cond
            (cwasl-as-core " --core ${CWASL}")
            (lisp-image (escape-string (strcat " --core " (namestring lisp-image))))
            (T ""))
          (mapcar #'escape-string lisp-options)
          (format nil "--eval")))

(defun get-asdf-dependencies-from-build-module ()
  "Returns a list of the names of all of the asdf systems that the build module depends on"
  (remove-if-not (lambda (dependency) 
                   (and (listp dependency) 
                        (destructuring-bind (type dep) dependency
                          (eql type :asdf))))
                 (nconc 
                  (compile-depends-on *build-module*) 
                  (build-depends-on *build-module*) 
                  (load-depends-on *build-module*))))
                 
#|  (reduce (lambda (dependency rest) 
            (if (listp dependency)
              (destructuring-bind (type dep) dependency
                (if (eql type :asdf)
                  (push dep rest)
                  rest))
              rest))
          (nconc 
           (compile-depends-on *build-module*) 
           (build-depends-on *build-module*) 
           (load-depends-on *build-module*)) 
          :from-end T :initial-value nil))|#


(defun makefile-setup (output-path)
  (with-output-to-string (outstring)
    (format outstring "LISPRUN = ~a~%~%" (eval-command-string *lisp-implementation*))
    (let ((asdf-dependencies (get-build-asdf-dependencies)))
      (when asdf-dependencies
        (setf *build-depends-on-asdf-systems-p* T)
        (format outstring "CWASL = ~a~%~%" (escape-string (namestring (make-pathname :name "core-with-asdf-systems" :type "core-xcvb" :defaults output-path))))
        (format outstring "CWASLRUN = ~a~%~%" (eval-command-string *lisp-implementation* :cwasl-as-core T))
        (format outstring ".PHONY: core-with-asdf-systems-loaded~%~%core-with-asdf-systems-loaded : ~%~a" #\tab)
        (format outstring "if ! ( [ -f ${CWASL} ] && (${CWASLRUN} \"(xcvb:asdf-systems-are-up-to-date-p ~{:~a~^ ~})\")) ; then \\~%" (mapcar #'escape-string asdf-dependencies))
        (format outstring "~a~a${LISPRUN} \"(progn (asdf:oos 'asdf:load-op :xcvb) ~{(asdf:oos 'asdf:load-op :~a) ~}(save-lisp-and-die \\\"${CWASL}\\\"))\" ; \\~%" #\tab #\tab (mapcar #'escape-string asdf-dependencies))
        (format outstring "~afi~%" #\tab)))))
        ;(format outstring "LISPRUN = ${CWASLRUN}~%")))))




(defun write-makefile (source-path output-path &optional (graph-type :image-dump))
  "Writes a makefile to output-path with information about how to compile the file at source-path.  What the makefile is designed to do can be specified by graph-type" 
  (with-open-file (out output-path :direction :output :if-exists :supersede)
    (let ((dependency-graph
           (case graph-type
             (:image-dump (build-dump-image-graph (make-pathname :name "lisp-image" :type "core" :defaults output-path) source-path))
             (otherwise (error "Unknown graph-type")))))
      (format out "~a" (makefile-setup output-path))
      (mapcar (lambda (node) (write-node-to-makefile out node)) (traverse dependency-graph)))))
      ;(write-node-to-makefile out dependency-graph (make-hash-table :test #'equal)))))


(defun makefile-line-for-node (node)
  "Returns the string of the line in the Makefile that can create the target of the given node"
  ;(command-list-to-Makefile-line (eval-command-list *lisp-implementation* (progn-form-string-for-node 
  (format nil "${~:[LISPRUN~;CWASLRUN~]} \"~a\"" 
          *build-depends-on-asdf-systems-p* 
          (escape-string-for-makefile 
           (escape-string-for-shell 
            (case *lisp-implementation*
              (:sbcl (format nil "(progn ~{~a~^ ~} (sb-ext:quit))" (mapcar #'form-string-for-node (traverse node))))
              (:ccl (format nil "(progn ~{~a~^ ~} (ccl:quit))" (mapcar #'form-string-for-node (traverse node))))
              (otherwise (error "unsupported lisp-implementation")))))))


(defgeneric form-string-for-node (node)
  (:documentation "stuff"))

(defmethod form-string-for-node ((node fasl-file-node))
  (format nil "(cl:load \"~a\")" (namestring (merge-pathnames (target node) *buildpath*))))

(defmethod form-string-for-node ((node asdf-system-node))
  (format nil "(asdf:oos 'asdf:load-op :~a)" (namestring (name node))))

(defmethod form-string-for-node ((node image-dump-node))
  (case *lisp-implementation*
    (:sbcl (format nil "(sb-ext:save-lisp-and-die \"~a\")" (merge-pathnames *buildpath* (target node))))
    (:ccl (format nil "(ccl:save-application \"~a\")" (merge-pathnames *buildpath* (target node))))
    (otherwise (error "unsupported lisp-implementation"))))

(defmethod form-string-for-node ((node source-file-node))
  (format nil "(cl:compile-file \"~a\")" (namestring (merge-pathnames (target node) *buildpath*))))

(defmethod form-string-for-node ((node dependency-graph-node))
  (declare (ignore node)))

(defgeneric write-node-to-makefile (filestream node)
  (:documentation "stuff"))

(defmethod write-node-to-makefile (filestream (node fasl-file-node))
  (format filestream "~a : ~{~a~^ ~}~%" (target node) (mapcar #'target (compile-dependencies node)))
  (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node)))

(defmethod write-node-to-makefile (filestream (node lisp-node))
  (format filestream "~a : ~{~a~^ ~}~%" (target node) (mapcar #'target (compile-dependencies node)))
  (format filestream "~a~a~%" #\tab (makefile-line-for-node node))
  (format filestream ".PHONY: ~a~%~%" (target node)))

(defmethod write-node-to-makefile (filestream (node image-dump-node))
  (format filestream "~a : ~{~a~^ ~}~%" (target node) (mapcar #'target (compile-dependencies (lisp-image node))))
  (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node)))

(defmethod write-node-to-makefile (filestream (node asdf-system-node))
  (format filestream "~a : ~%" (target node))
  (format filestream "~a~a~%" #\tab (makefile-line-for-node node))
  (format filestream ".PHONY: ~a~%~%" (target node)))

(defmethod write-node-to-makefile (filestream node)
  (declare (ignore filestream node)))


#|
(defun command-list-to-shell-command (command-list)
  "Converts the list of commands to a single string that has been escape for the shell"
  (format nil "~{~A~^ ~}" (mapcar #'escape-string-for-shell command-list)))

(defun command-list-to-Makefile-line (command-list)
  "Converts the list of commands to a single string that can be used to execute shell commands through make"
  (shell-command-to-Makefile-line (command-list-to-shell-command command-list)))
|#


#|
(defgeneric load-node-form-string (node)
  (:documentation "Returns the string corresponding to the proper lisp form to load the given node"))

(defmethod load-node-form-string ((node fasl-file-node))
  (format nil "(load \"~a\")" (namestring (merge-pathnames (target node) *buildpath*))))

(defmethod load-node-form-string ((node asdf-system-node))
  (format nil "(asdf:oos 'asdf:load-op :~a)" (namestring (name node))))


(defgeneric lisp-forms-string-for-node (node)
  (:documentation "Returns a string of the lisp forms that will create what the node represents.  For example, for a fasl-file-node, it will load all the node's dependencies, then compile the source file, thus creating the fasl."))

(defmethod lisp-forms-string-for-node ((node fasl-file-node))
  (format nil "~{~a ~^~}(compile-file \"~a\")" (mapcar #'load-node-form-string (rest (dependencies node))) (namestring (merge-pathnames (target (first (dependencies node))) *buildpath*))))

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


(defun makefile-line-for-node (node)
  "Returns the string of the line in the Makefile that can create the target of the given node"
  ;(command-list-to-Makefile-line (eval-command-list *lisp-implementation* (progn-form-string-for-node 
  (format nil "${~:[LISPRUN~;CWASLRUN~]} \"~a\"" *build-depends-on-asdf-systems-p* (escape-string-for-makefile (escape-string-for-shell (progn-form-string-for-node node)))))


(defgeneric write-node-to-makefile (filestream node written-nodes)
  (:documentation "Writes the makefile information for the given node and its dependencies to the filestream"))

(defmethod write-node-to-makefile (filestream (node fasl-file-node) written-nodes)
  (unless (nth-value 1 (gethash (fullname node) written-nodes));If this node has already been written to the makefile, don't write it again.
    (setf (gethash (fullname node) written-nodes) nil);Add this node to the map of nodes already written to the makefile
    (format T "Writing info for dependencies of fasl-node \"~a\"..." (target node))
    (mapcar (lambda (dependency-node) (write-node-to-makefile filestream dependency-node written-nodes)) (dependencies node));Write the information for the node's depedencies first since they have to be compiled first anyway
    (format T "done.~%Writing what fasl-node \"~a\" depends on..." (target node))
    (format filestream "~%~%~a : ~{~a~^ ~}~%" (target node) (mapcar #'target (dependencies node)))
    (format T "done.~%Writing how to build fasl-node \"~a\"..." (target node))
    (format filestream "~a~a" #\tab (makefile-line-for-node node))
    (format T "done.~%")))
    ;(format filestream "~%~T~a ~a " (fasl-compiler) (target node))
    ;(mapcar (lambda (dependency-node) (format filestream " ~a" (target dependency-node))) (dependencies node))))
    

(defmethod write-node-to-makefile (filestream (node lisp-node) written-nodes)
  (format T "Writing info for dependencies of lisp-node...")
  (mapcar (lambda (dependency-node) (write-node-to-makefile filestream dependency-node written-nodes)) (dependencies node))
  (format T "done.~%Writing what lisp-node depends on...")
  (format filestream "~%~%~a : ~a~{~a~^ ~}~%" 
          (target node)
          (if *build-depends-on-asdf-systems-p*
            "core-with-asdf-systems-loaded "
            "")          
          (mapcar #'target (dependencies node)))
  (format T "done.~%Writing how to build lisp-node...")
  (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node))
  (format T "done.~%")
  (format filestream ".PHONY: ~a" (target node)))

  ;(format filestream "~T${lispbuilder} ~a" (target node))
  ;(mapcar (lambda (dependency-node) (format filestream " ~a" (target dependency-node))) (dependencies node)))
  

(defmethod write-node-to-makefile (filestream (node image-dump-node) written-nodes)
  (format T "Writing info for lisp-image of image-dump-node...")
  (write-node-to-makefile filestream (lisp-image node) written-nodes)
  (format filestream "~%~%~a : ~a~{~a~^ ~}~%" 
          (target node)
          (if *build-depends-on-asdf-systems-p*
            "core-with-asdf-systems-loaded "
            "")
          (mapcar #'target (dependencies (lisp-image node))))
  (format T "done.~%Writing how to build image-dump-node...") 
 (format filestream "~a~a" #\tab (makefile-line-for-node node))
  (format T "done.~%"))
  ;(format filestream "~%~T${imagedumper} ~a ~a" (target node) (target (lisp-image node))))
  

(defmethod write-node-to-makefile (filestream (node asdf-system-node) written-nodes)
  (unless (nth-value 1 (gethash (fullname node) written-nodes))
    (setf (gethash (fullname node) written-nodes) nil)
    (format filestream "~%~%~a :~%" (target node))
    (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node))
    (format filestream ".PHONY: ~a" (target node))))
    
    ;(format filestream "~%~T${asdfcompiler} ~a" (target node))))

(defmethod write-node-to-makefile (filestream (node source-file-node) written-nodes)
  (declare (ignore filestream node written-nodes)));Since source-file-nodes have no dependencies and should already exist, don't write anything about them to the makefile.

|#
