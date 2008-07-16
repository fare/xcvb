(in-package :xcvb)

(defparameter *build-depends-on-asdf-systems-p* nil
  "Flag to specify if the build file has asdf dependencies, and therefore whether or not to have a core-with-asdf-systems-loaded target")
(defvar *written-nodes*)


(defun quit-form ()
  "Returns the correct form to quit lisp, based on the value of *lisp-implementation*"
  (quit-form-helper *lisp-implementation*))

(defgeneric quit-form-helper (lisp-impl)
  (:documentation "Helper generic function for quit-form function"))

(defmethod quit-form-helper ((lisp-impl (eql :sbcl)))
  (declare (ignore lisp-impl))
  (format nil "(sb-ext:quit)"))

(defmethod quit-form-helper ((lisp-impl (eql :ccl)))
  (declare (ignore lisp-impl))
  (format nil "(ccl:quit)"))


(defun save-image-form (filepath)
  "Returns the lisp form to save the lisp image to the given filepath"
  (save-image-form-helper filepath *lisp-implementation*))

(defgeneric save-image-form-helper (filepath lisp-impl)
  (:documentation "Helper generic function for save-image-form function"))

(defmethod save-image-form-helper (filepath (lisp-impl (eql :sbcl)))
  (declare (ignore lisp-impl))
  (format nil "(sb-ext:save-lisp-and-die \"~a\")" (namestring filepath)))

(defmethod save-image-form-helper (filepath (lisp-impl (eql :ccl)))
  (declare (ignore lisp-impl))
  (format nil "(ccl:save-application \"~a\")" (namestring filepath)))


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




(defgeneric eval-command-string (lisp-type &key lisp-executable lisp-image lisp-options cwbrl-as-core)
  (:documentation "Returns the command string required to eval the given lisp form in the given lisp implementation, with characters properly escaped for running a shell command in a makefile"))

(defmethod eval-command-string ((lisp-type (eql :sbcl)) &key (lisp-executable *lisp-executable-pathname*) (lisp-image *lisp-image-pathname*) (lisp-options *lisp-options*) cwbrl-as-core)
  (format nil "~a~a ~{~a ~^~}~a" 
          (escape-string (namestring lisp-executable))
          (cond
            (cwbrl-as-core " --core ${CWBRL}")
            (lisp-image (escape-string (strcat " --core " (namestring lisp-image))))
            (T ""))
          (mapcar #'escape-string lisp-options)
          (format nil "--eval")))



(defgeneric form-string-for-node (node)
  (:documentation "Returns the string for the lisp form that will build the given node (does not take dependencies into account)"))

(defmethod form-string-for-node ((node fasl-node))
  (format nil "(cl:load \"~a\")" (namestring (merge-pathnames (target node) *buildpath*))))

(defmethod form-string-for-node ((node asdf-system-node))
  (format nil "(asdf:oos 'asdf:load-op :~a)" (namestring (name node))))

(defmethod form-string-for-node ((node image-dump-node))
  (save-image-form (merge-pathnames *buildpath* (target node))))

(defmethod form-string-for-node ((node source-file-node))
  (format nil "(cl:compile-file \"~a\")" (namestring (merge-pathnames (target node) *buildpath*))))

(defmethod form-string-for-node ((node dependency-graph-node))
  (declare (ignore node)))


(defun makefile-line-for-node (node operation)
  "Returns the string of the line in the Makefile that can create the target of the given node"
  ;(command-list-to-Makefile-line (eval-command-list *lisp-implementation* (progn-form-string-for-node 
  (format nil "${~:[LISPRUN~;CWBRLRUN~]} \"~a\"" 
          *build-depends-on-asdf-systems-p* 
          (escape-string
           (format nil "(progn ~{~@[~a~^ ~]~} ~a)" (mapcar #'form-string-for-node (traverse node operation)) (quit-form)))))



(defgeneric write-node-to-makefile (filestream node)
  (:documentation "Writes a makefile target to the filestream for building the given node"))

(defmethod write-node-to-makefile (filestream (node fasl-node))
  (unless (nth-value 1 (gethash (fullname node) *written-nodes*));If this node has already been written to the makefile, don't write it again.
    (setf (gethash (fullname node) *written-nodes*) nil);Add this node to the map of nodes already written to the makefile
;  (format filestream "~a : ~{~a~^ ~}~%" (target node) (mapcar #'target (compile-dependencies node)))
    (format filestream "~a : ~:[~;core-with-build-requires-loaded ~]~{~a~^ ~}~%" (target node) *build-depends-on-asdf-systems-p* (mapcar #'target (traverse node :create)))
    (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node :create))))

(defmethod write-node-to-makefile (filestream (node lisp-node))
  ;(format filestream "~a : ~{~a~^ ~}~%" (target node) (mapcar #'target (compile-dependencies node)))
  (format filestream "~a : ~:[~;core-with-build-requires-loaded ~]~{~a~^ ~}~%" (target node) *build-depends-on-asdf-systems-p* (mapcar #'target (traverse node :create)))
  (format filestream "~a~a~%" #\tab (makefile-line-for-node node :create))
  (format filestream ".PHONY: ~a~%~%" (target node)))

(defmethod write-node-to-makefile (filestream (node image-dump-node))
  ;(format filestream "~a : ~{~a~^ ~}~%" (target node) (mapcar #'target (compile-dependencies (lisp-image node))))
  (format filestream "~a : ~:[~;core-with-build-requires-loaded ~]~{~a~^ ~}~%" (target node) *build-depends-on-asdf-systems-p* (mapcar #'target (traverse node :create)))
  (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node :load)))

(defmethod write-node-to-makefile (filestream (node asdf-system-node))
  (unless (nth-value 1 (gethash (fullname node) *written-nodes*));If this node has already been written to the makefile, don't write it again.
    (setf (gethash (fullname node) *written-nodes*) nil);Add this node to the map of nodes already written to the makefile
    (format filestream "~a : ~:[~;core-with-build-requires-loaded ~]~%" (target node) *build-depends-on-asdf-systems-p*)
    (format filestream "~a~a~%" #\tab (makefile-line-for-node node :load))
    (format filestream ".PHONY: ~a~%~%" (target node))))

(defmethod write-node-to-makefile (filestream node)
  (declare (ignore filestream node)))


#|(defun get-asdf-dependencies-from-build-module ()
  "Returns a list of the names of all of the asdf systems that the build module depends on"
  #|(remove-if-not (lambda (dependency) 
                   (and (listp dependency) 
                        (destructuring-bind (type dep) dependency
                          (declare (ignore dep))
                          (eql type :asdf))))
                 (nconc
                  (compile-depends-on *build-module*) 
                  (build-depends-on *build-module*) 
                  (load-depends-on *build-module*))))|#
                 
  (reduce (lambda (dependency rest) 
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


(defun makefile-setup (output-path filestream)
  (format filestream "LISPRUN = ~a~%~%" (eval-command-string *lisp-implementation*))
  (when (build-requires *build-module*)
    (let* ((cwbrlpath (escape-string (namestring (make-pathname :name "core-with-build-requires" :type "core-xcvb" :defaults output-path))))
           (core-with-build-requires-graph 
            (create-image-dump-node (create-lisp-node (build-requires *build-module*)) cwbrlpath))
           (core-deps (traverse core-with-build-requires-graph :create)))
      (format filestream "CWBRL = ~a~%~%" cwbrlpath)
      (format filestream "CWBRLRUN = ~a~%~%" (eval-command-string *lisp-implementation* :cwbrl-as-core T))
      ;(format filestream ".PHONY: core-with-asdf-systems-loaded~%~%core-with-asdf-systems-loaded : ~{~a~^ ~}~%~a" (mapcar #'escape-string asdf-dependencies) #\tab)
      (format filestream "CHECK_ASDFS = $(shell if ! ( [ -f ${CWBRL} ] && (${CWBRLRUN} \"(xcvb:asdf-systems-are-up-to-date-p ~{:~a~^ ~})\")) ; then echo force ; fi )~%~%force : ~%~%" (mapcar (lambda (dep) (escape-string (namestring (name dep)))) (remove-if-not (lambda (dep) (typep dep 'asdf-system-node)) core-deps)))
      (mapcar (lambda (node) (write-node-to-makefile filestream node)) core-deps)
      (format filestream ".PHONY: core-with-build-requires-loaded~%~%")
      (format filestream "core-with-build-requires-loaded : $(CHECK_ASDFS) ~{~a~^ ~}~%" (mapcar (lambda (dep) (escape-string (target dep))) (remove-if (lambda (dep) (typep dep 'asdf-system-node)) core-deps)))
      ;(format filestream "if ! ( [ -f ${CWASL} ] && (${CWASLRUN} \"(xcvb:asdf-systems-are-up-to-date-p ~{:~a~^ ~})\")) ; then \\~%" (mapcar #'escape-string asdf-dependencies))
      ;(format filestream "if ! ( [ -f ${CWASL} ] && (${CWASLRUN} \"(xcvb:asdf-systems-are-up-to-date-p ~{:~a~^ ~})\")) ; then \\~%" (mapcar (lambda (dep) (escape-string (namestring (target dep)))) core-deps))
      (format filestream "~a~a~%~%" #\tab (makefile-line-for-node core-with-build-requires-graph :load))
      (setf *build-depends-on-asdf-systems-p* T))))
        ;(format filestream "~a~a${LISPRUN} \"(progn (asdf:oos 'asdf:load-op :xcvb) ~{(asdf:oos 'asdf:load-op :~a) ~}(save-lisp-and-die \\\"${CWASL}\\\"))\" ; \\~%" #\tab #\tab (mapcar #'escape-string asdf-dependencies))
        ;(format filestream "~afi~%~%" #\tab)))))


(defun write-makefile (source-path output-path &optional (graph-type :image-dump))
  "Writes a makefile to output-path with information about how to compile the file at source-path.  What the makefile is designed to do can be specified by graph-type" 
  (with-open-file (out output-path :direction :output :if-exists :supersede)
    (let ((dependency-graph
           (case graph-type
             (:image-dump (create-dump-image-graph (make-pathname :name "lisp-image" :type "core" :defaults output-path) source-path))
             (otherwise (error "Unknown graph-type")))))
      (let ((*written-nodes* (make-hash-table :test #'equal)))
        (setf *build-depends-on-asdf-systems-p* nil)
        (makefile-setup output-path out)
        (mapcar (lambda (node) (write-node-to-makefile out node)) (traverse dependency-graph :load))))))
      ;(write-node-to-makefile out dependency-graph (make-hash-table :test #'equal)))))




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
