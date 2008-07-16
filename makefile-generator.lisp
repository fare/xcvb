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
  (format nil "${~:[LISPRUN~;CWBRLRUN~]} \"~a\"" 
          *build-depends-on-asdf-systems-p* 
          (escape-string
           (format nil "(progn ~{~@[~a~^ ~]~} ~a)" (mapcar #'form-string-for-node (traverse node operation)) (quit-form)))))



(defgeneric write-node-to-makefile (filestream node)
  (:documentation "Writes a makefile target to the filestream for building the given node"))

(defmethod write-node-to-makefile (filestream (node fasl-node))
  (unless (nth-value 1 (gethash (fullname node) *written-nodes*));If this node has already been written to the makefile, don't write it again.
    (setf (gethash (fullname node) *written-nodes*) nil);Add this node to the map of nodes already written to the makefile
    (format filestream "~a : ~:[~;core-with-build-requires-loaded ~]~{~a~^ ~}~%" (target node) *build-depends-on-asdf-systems-p* (mapcar #'target (traverse node :create)))
    (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node :create))))

(defmethod write-node-to-makefile (filestream (node lisp-node))
  (format filestream "~a : ~:[~;core-with-build-requires-loaded ~]~{~a~^ ~}~%" (target node) *build-depends-on-asdf-systems-p* (mapcar #'target (traverse node :create)))
  (format filestream "~a~a~%" #\tab (makefile-line-for-node node :create))
  (format filestream ".PHONY: ~a~%~%" (target node)))

(defmethod write-node-to-makefile (filestream (node image-dump-node))
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


(defun makefile-setup (output-path filestream)
  (format filestream "LISPRUN = ~a~%~%" (eval-command-string *lisp-implementation*))
  (when (build-requires *build-module*)
    (let* ((cwbrlpath (escape-string (namestring (make-pathname :name "core-with-build-requires" :type "core-xcvb" :defaults output-path))))
           (core-with-build-requires-graph 
            (create-image-dump-node (create-lisp-node (build-requires *build-module*)) cwbrlpath))
           (core-deps (traverse core-with-build-requires-graph :create)))
      (format filestream "CWBRL = ~a~%~%" cwbrlpath)
      (format filestream "CWBRLRUN = ~a~%~%" (eval-command-string *lisp-implementation* :cwbrl-as-core T))
      (format filestream "CHECK_ASDFS = $(shell if ! ( [ -f ${CWBRL} ] && (${CWBRLRUN} \"(xcvb:asdf-systems-are-up-to-date-p ~{:~a~^ ~})\")) ; then echo force ; fi )~%~%force : ~%~%" (mapcar (lambda (dep) (escape-string (namestring (name dep)))) (remove-if-not (lambda (dep) (typep dep 'asdf-system-node)) core-deps)))
      (mapcar (lambda (node) (write-node-to-makefile filestream node)) core-deps)
      (format filestream ".PHONY: core-with-build-requires-loaded~%~%")
      (format filestream "core-with-build-requires-loaded : $(CHECK_ASDFS) ~{~a~^ ~}~%" (mapcar (lambda (dep) (escape-string (target dep))) (remove-if (lambda (dep) (typep dep 'asdf-system-node)) core-deps)))
      (format filestream "~a~a~%~%" #\tab (makefile-line-for-node core-with-build-requires-graph :load))
      (setf *build-depends-on-asdf-systems-p* T))))


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