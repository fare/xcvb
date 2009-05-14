(in-package :xcvb)

(defparameter *build-requires-p* nil
  "Flag to specify if the build grain's build-requires slot has been set --
and therefore whether or not to have a build-stage1.image
target")

(defvar *written-nodes* nil
  "A map of the nodes that have already been written
to the makefile, to avoid writing any node twice")

(defvar *targets-dependent-on-cwbrl* nil
  "A list of the makefile targets that have a dependency on
the build-stage1.image target")

(defvar *output-path* nil
  "The path that the Makefile is being written to.
The Makefile targets will be relative to this path.")

(defun simplify-target-path (x)
  (enough-namestring x *output-path*))

(defgeneric target-for-node (node)
  (:documentation "Returns the name of the makefile target for the given node"))

(defmethod target-for-node ((node object-file-node))
  (simplify-target-path
   (make-pathname
    :type "fasl"
    :defaults (source-filepath node))))

(defmethod target-for-node ((node source-file-node))
  (simplify-target-path (source-filepath node)))

(defmethod target-for-node ((node image-dump-node))
  (simplify-target-path (namestring (dump-path node))))

(defmethod target-for-node ((node asdf-system-node))
  (fullname node))

(defmethod target-for-node ((node lisp-image-node))
  (fullname node))

(defmethod target-for-node ((node load-source-node))
  (simplify-target-path (namestring (source-filepath node))))

;;TODO: Simplify with calls to function in xcvb.
(defgeneric form-string-for-node (node)
  (:documentation "Returns a string of the lisp form that will perform the
action that the node represents"))

(defmethod form-string-for-node ((node asdf-system-node))
   (format nil
          "(asdf:oos 'asdf:load-op ~(~S~))"
     (intern (string-upcase (name node)) :keyword)))

(defmethod form-string-for-node ((node load-source-node))
  (format nil
          "(load ~S)"
	  (target-for-node node)))

(defmethod form-string-for-node ((node image-dump-node))
  (save-image-form
   (namestring (dump-path node))))

(defmethod form-string-for-node ((node source-file-node))
  (format nil "(cl:compile-file ~S~:[~; :emit-cfasl T~])"
	  (source-filepath node) *use-cfasls*))

(defmethod form-string-for-node ((node fasl-node))
  (format nil
          "(cl:load ~S)"
          (make-pathname
           :type "fasl"
           :defaults (source-filepath node))))

(defmethod form-string-for-node ((node cfasl-node))
  (if *use-cfasls*
    (format nil "(cl:load ~S)"
            (make-pathname :type "cfasl"
                           :defaults (source-filepath node)))
    (format nil "(cl:compile-file ~S)"
            (source-filepath node))))


(defmethod form-string-for-node ((node dependency-graph-node))
  (declare (ignore node)))


(defun makefile-line-for-node (node operation &optional out)
  "Returns the string of the line in the Makefile
that will be used to build the target"
  (with-output (out)
    (format out "${~:[LISPRUN~;CWBRLRUN~]} " *build-requires-p*)
    (escape-shell-token-for-Makefile
     (format nil "(progn ~{~@[~A~^ ~]~} ~A)"
	     (mapcar #'form-string-for-node (traverse node operation))
	     (quit-form))
     out)))

(defgeneric write-node-to-makefile (filestream node)
  (:documentation "Writes a makefile target to the filestream for building the
given node"))

(defmethod write-node-to-makefile :around (filestream 
                                           (node dependency-graph-node))
  (let ((target (target-for-node node)))
    ;;If this node has already been written to the makefile, don't write it again.
    (unless (gethash target *written-nodes*)
    (when *build-requires-p*
      (pushnew target
               *targets-dependent-on-cwbrl* 
               :test 'string-equal))
    ;;Add this node to the map of nodes already written to the makefile
    (setf (gethash target *written-nodes*) t)
    (call-next-method))))

(defmethod write-node-to-makefile (filestream (node object-file-node))
    (format T "writing (c)fasl node: ~A~%" (fullname node))
    (format filestream "~A : ~{~A~^ ~}~%"
            (target-for-node node)
            (mapcar #'target-for-node (traverse node :create)))
    (format filestream "~A~A~%~%" #\tab (makefile-line-for-node node :create)))

(defmethod write-node-to-makefile (filestream (node image-dump-node))
  (format filestream "~A : ~{~A~^ ~}~%"
          (target-for-node node)
          (mapcar #'target-for-node (traverse node :create)))
  (format filestream "~A~A~%~%" #\tab (makefile-line-for-node node :load)))

(defmethod write-node-to-makefile (filestream (node lisp-image-node))
  (format filestream ".PHONY: ~A~%~:*~A : ~{~A~^ ~}~%"
          (target-for-node node)
          (mapcar #'target-for-node (traverse node :create)))
  (format filestream "~A~A~%~%" #\tab (makefile-line-for-node node :create)))

(defmethod write-node-to-makefile (filestream (node asdf-system-node))
  (format filestream ".PHONY: ~A~%~:*~A : ~%" (target-for-node node))
  (format filestream "~A~A~%~%" #\tab (makefile-line-for-node node :load)))

(defmethod write-node-to-makefile (filestream node)
  (declare (ignore filestream node)))

(defun Makefile-lisp-invocation (out &rest keys)
  (shell-tokens-to-Makefile (apply #'lisp-invocation-arglist keys) out))

(defun makefile-setup (out)
  "Writes information to the top of the makefile about how to actually run lisp
to compile files, and also how to create an image to use that contains all the
dependencies from the build-requires slot of the build grain loaded"
  (with-output (out)
    (format out "lisp.image:~%")
    (format out "export PATH := .:${PATH}~%")
    (format out "LISPRUN := ~A~%~%"
	    (Makefile-lisp-invocation nil
				      :eval '(:makefile)))
    (let* ((cwbrlpath (make-pathname
		       :name "build-stage1"
		       :type "image"
		       :defaults *output-path*))
	   (core-with-build-requires-graph
	    (create-image-dump-node (create-lisp-image-node
				     (append (xcvb-setup-dependencies)
					     *lisp-setup-dependencies*
					     (build-requires *build-grain*)))
				    cwbrlpath)))
      (format out "CWBRL := ./~A~%~%" (simplify-target-path cwbrlpath))
      (format out "CWBRLRUN := ~A~%~%"
	      (Makefile-lisp-invocation nil :image-path '(:makefile "${CWBRL}") :eval '(:makefile)))
      #|(format out "FASL := $(shell ${LISPRUN} ~A)~%~%"
	      (escape-shell-token-for-Makefile
	       (format nil "(progn (princ (pathname-type (compile-file-pathname \"test.lisp\"))) ~A)"
		       (quit-form))))
      (format out "CFASL := cfasl~%~%")|#
      (format out
	      "CHECK_ASDFS := $(shell if ! ( [ -f ${CWBRL} ] && (${CWBRLRUN} ~
\"(asdf::asdf-systems-are-up-to-date-p ~{:~A~^ ~})\") ) ; ~
then echo force ; fi )~%~%force : ~%.PHONY: force~%~%"
	      (mapcar (lambda (dep) (escape-shell-token-for-Makefile (name dep)))
		      (remove-if-not
		       (lambda (dep) (typep dep 'asdf-system-node))
		       (traverse core-with-build-requires-graph :create))))
      (dolist (node
		(traverse (lisp-image core-with-build-requires-graph) :create))
	(write-node-to-makefile out node))
      (format out
	      "build-stage1.image : $(CHECK_ASDFS) ~{~A~^ ~}~%"
	      (mapcar #'target-for-node
		      (remove-if
		       (lambda (dep) (typep dep 'asdf-system-node))
		       (traverse core-with-build-requires-graph :create))))
      (format out "~A~A~%~%"
	      #\tab
	      (makefile-line-for-node core-with-build-requires-graph :load))
      (setf *build-requires-p* T))))

(defun write-makefile (source-path
                       &key
		       (output-path (pathname-directory-pathname source-path))
                       (makefile-name "xcvb.mk")
                       (image-name "lisp.image"))
  "Writes a makefile to output-path with information about how to compile the file at source-path.  What the makefile is designed to do can be specified by graph-type"
  (let* ((*default-pathname-defaults* (pathname-directory-pathname output-path))
	 (*targets-dependent-on-cwbrl* nil)
	 (*build-requires-p* nil)
	 (*output-path* (namestring output-path))
	 (*written-nodes* (make-hash-table :test #'equal))
	 (dependency-graph
	  (create-dump-image-graph
	   (merge-pathnames image-name output-path)
	   source-path))
	 (all-nodes (traverse dependency-graph :all)))
    (with-open-file (out (merge-pathnames makefile-name output-path)
                         :direction :output
                         :if-exists :supersede)
      (makefile-setup out)
      (dolist (node all-nodes) (write-node-to-makefile out node))
      (format out "~@[~{~A~^ ~} : build-stage1.image~%~]"
              *targets-dependent-on-cwbrl*))))
