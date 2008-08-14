(in-package :xcvb)

(defparameter *build-requires-p* nil
  "Flag to specify if the build module's build-requires slot has been set -
and therefore whether or not to have a core-with-build-requires.core-xcvb
target")

(defvar *written-nodes* nil
  "A map of the nodes that have already been written
to the makefile, to avoid writing any node twice")

(defvar *targets-dependent-on-cwbrl* nil "A list of the makefile targets that
have a dependency on the core-with-build-requires.core-xcvb target")

(defvar *escaped-output-path* nil "The path that the Makefile is being written
to. The Makefile targets will be relative to this path.  It has already been
escaped for the shell and for a Makefile")


(defun escape-string-for-Makefile (string)
  "Takes a string and excapes all the characters that need to be to be put into
a makefile.  The only such character right now is $.  Raises an error if the
string contains a newline."
  (with-output-to-string (out-string)
    (loop for c across string do
      (case c
        ;;TODO - instead of erroring, should this insert a "\" to escape the newline?
        (#\newline (error "Makefile line cannot contain a newline"))
        (#\$ (format out-string "$$"))
        (#\\ (format out-string "\\"));Not currently doing anything!
        (otherwise (format out-string "~a" c))))))


(defun escape-string-for-shell (string)
  "Takes a string and excapes all the characters that need to be to be run in
the shell.  These characters are \" $ ` \\"
  (with-output-to-string (out-string)
    (loop for c across string do
	 (case c
	   ((#\" #\` #\$ #\\) (format out-string "\\~a" c))
	   (otherwise (write-char c out-string))))))


(defun escape-string (string)
  "Takes a string and escapes it first for the shell, then for a makefile"
  (escape-string-for-makefile (escape-string-for-shell string)))


(defun eval-command-string (&key
                            (lisp-type *lisp-implementation*)
                            (lisp-executable *lisp-executable-pathname*)
                            (lisp-image *lisp-image-pathname*)
                            (lisp-options *lisp-options*)
                            cwbrl-as-core-p)
  "Returns the string of the command required to eval the given lisp
form in the given lisp implementation, with characters properly escaped for
running a shell command in a makefile"
  (eval-command-string-helper
   lisp-type
   :lisp-executable lisp-executable
   :lisp-image lisp-image
   :lisp-options lisp-options
   :cwbrl-as-core-p cwbrl-as-core-p))

(defgeneric eval-command-string-helper
    (lisp-type &key lisp-executable lisp-image lisp-options cwbrl-as-core-p)
  (:documentation "Helper generic functions for eval-command-string"))

(defmethod eval-command-string-helper ((lisp-type (eql :sbcl)) &key
                                       (lisp-executable
                                        *lisp-executable-pathname*)
                                       (lisp-image *lisp-image-pathname*)
                                       (lisp-options *lisp-options*)
                                       cwbrl-as-core-p)
  (format nil "~a~@[~a~] ~{~a ~}~a"
          (escape-string (namestring lisp-executable))
          (cond
            (cwbrl-as-core-p " --core ${CWBRL}")
            (lisp-image
             (format nil " --core ~a" (escape-string (namestring lisp-image))))
            (T nil))
          (mapcar #'escape-string lisp-options)
          "--eval"))

(defmethod eval-command-string-helper ((lisp-type (eql :ccl)) &key
                                       (lisp-executable
                                        *lisp-executable-pathname*)
                                       (lisp-image *lisp-image-pathname*)
                                       (lisp-options *lisp-options*)
                                       cwbrl-as-core-p)
  (format nil "~a~@[~a~] ~{~a ~}~a"
          (escape-string (namestring lisp-executable))
          (cond
            (cwbrl-as-core-p " --image-name ${CWBRL}")
            (lisp-image (escape-string
                         (format nil " --image-name ~a" (namestring lisp-image))))
            (T nil))
          (mapcar #'escape-string lisp-options)
          "--eval"))

(defgeneric target-for-node (node)
  (:documentation "Returns the name of the makefile target for the given node"))

(defmethod target-for-node ((node object-file-node))
  (enough-namestring
   (make-pathname
    :type "${FASL}"
    :defaults (escaped-source-filepath node))
   *escaped-output-path*))

(defmethod target-for-node ((node source-file-node))
  (enough-namestring
   (escaped-source-filepath node)
   *escaped-output-path*))

(defmethod target-for-node ((node image-dump-node))
  (enough-namestring (escape-string (namestring (dump-path node)))
                     *escaped-output-path*))

(defmethod target-for-node ((node asdf-system-node))
  (fullname node))

(defmethod target-for-node ((node lisp-image-node))
  (fullname node))

;;TODO: Simplify with calls to function in xcvb.
(defgeneric form-string-for-node (node)
  (:documentation "Returns a string of the lisp form that will perform the
action that the node represents"))

(defmethod form-string-for-node ((node asdf-system-node))
  (format nil
          "(asdf:oos 'asdf:load-op :~a)"
          (escape-string (name node))))

(defmethod form-string-for-node ((node image-dump-node))
  (save-image-form
   (escape-string (namestring (dump-path node)))))

(defmethod form-string-for-node ((node source-file-node))
  (if *use-cfasls*
    (format nil
            "#+cfasls (cl:compile-file \\\"~a\\\" ~
:emit-cfasl T)#-cfasls (cl:compile-file \\\"~:*~a\\\")"
            (escaped-source-filepath node))
    (format nil "(cl:compile-file \\\"~a\\\")"
            (escaped-source-filepath node))))

(defmethod form-string-for-node ((node fasl-node))
  (format nil
          "(cl:load \\\"~a\\\")"
          (make-pathname
           :type "${FASL}"
           :defaults (escaped-source-filepath node))))

(defmethod form-string-for-node ((node cfasl-node))
  (if *use-cfasls*
    (format nil
            "#+cfasls (cl:load \\\"~a\\\")#-cfasls (cl:compile-file \\\"~a\\\")"
            (make-pathname :type "${CFASL}"
                           :defaults (escaped-source-filepath node))
            (escaped-source-filepath node))
    (format nil "(cl:compile-file \"~a\")"
            (escaped-source-filepath node))))


(defmethod form-string-for-node ((node dependency-graph-node))
  (declare (ignore node)))


(defun makefile-line-for-node (node operation)
  "Returns the string of the line in the Makefile that will be used to build the
 target"
  (format nil "${~:[LISPRUN~;CWBRLRUN~]} \"~a\""
          *build-requires-p*
          (format nil "(progn ~{~@[~a~^ ~]~} ~a)"
                  (mapcar #'form-string-for-node (traverse node operation))
                  (quit-form))))

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
    (format T "writing (c)fasl node: ~a~%" (fullname node))
    (format filestream "~a : ~{~a~^ ~}~%"
            (target-for-node node)
            (mapcar #'target-for-node (traverse node :create)))
    (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node :create)))


(defmethod write-node-to-makefile (filestream (node image-dump-node))
  (format filestream "~a : ~{~a~^ ~}~%"
          (target-for-node node)
          (mapcar #'target-for-node (traverse node :create)))
  (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node :load)))

(defmethod write-node-to-makefile (filestream (node lisp-image-node))
  (format filestream ".PHONY: ~a~%~:*~a : ~{~a~^ ~}~%"
          (target-for-node node)
          (mapcar #'target-for-node (traverse node :create)))
  (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node :create)))

(defmethod write-node-to-makefile (filestream (node asdf-system-node))
  (format filestream ".PHONY: ~a~%~:*~a : ~%" (target-for-node node))
  (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node :load)))

(defmethod write-node-to-makefile (filestream node)
  (declare (ignore filestream node)))


(defun makefile-setup (output-path filestream)
  "Writes information to the top of the makefile about how to actually run lisp
to compile files, and also how to create an image to use that contains all the
dependencies from the build-requires slot of the build module loaded"
  (format filestream "LISPRUN := ~a~%~%" (eval-command-string))
  (let* ((cwbrlpath (escape-string (namestring
                                    (make-pathname
                                     :name "core-with-build-requires"
                                     :type "core-xcvb"
                                     :defaults output-path))))
         (core-with-build-requires-graph
          (create-image-dump-node (create-lisp-image-node
                                   (pushnew (list :asdf "xcvb")
                                            (build-requires *build-module*)))
                                  cwbrlpath)))
    (format filestream "CWBRL := ~a~%~%" cwbrlpath)
    (format filestream "CWBRLRUN := ~a~%~%"
            (eval-command-string :cwbrl-as-core-p T))
    (format filestream "FASL := $(shell ${LISPRUN} \"(progn (format t \\\"~~a\\\" (pathname-type (compile-file-pathname \\\"test.lisp\\\"))) (sb-ext:quit))\")~%~%")
    (format filestream "CFASL := cfasl~%~%")
    (format filestream
            "CHECK_ASDFS := $(shell if ! ( [ -f ${CWBRL} ] && [(${CWBRLRUN} ~
\"(asdf::asdf-systems-are-up-to-date-p ~{:~a~^ ~})\")]) ; ~
then echo force ; fi )~%~%force : ~%.PHONY: force~%~%"
            (mapcar (lambda (dep) (escape-string (name dep)))
                    (remove-if-not
                     (lambda (dep) (typep dep 'asdf-system-node))
                     (traverse core-with-build-requires-graph :create))))
    (dolist (node
              (traverse (lisp-image core-with-build-requires-graph) :create))
      (write-node-to-makefile filestream node))
    (format filestream
            "core-with-build-requires.core-xcvb : $(CHECK_ASDFS) ~{~a~^ ~}~%"
            (mapcar #'target-for-node
                    (remove-if
                     (lambda (dep) (typep dep 'asdf-system-node))
                     (traverse core-with-build-requires-graph :create))))
    (format filestream "~a~a~%~%"
            #\tab
            (makefile-line-for-node core-with-build-requires-graph :load))
    (setf *build-requires-p* T)))



(defun write-makefile (source-path
                       output-path &key
                       (makefile-name "Makefile.xcvb")
                       (image-name "lisp-image.core"))
  "Writes a makefile to output-path with information about how to compile the file at source-path.  What the makefile is designed to do can be specified by graph-type"
  (let* ((dependency-graph
         (create-dump-image-graph
          (merge-pathnames image-name output-path)
          source-path))
        (all-nodes (traverse dependency-graph :all))
        (*written-nodes* (make-hash-table :test #'equal))
        (*targets-dependent-on-cwbrl* nil)
        (*build-requires-p* nil)
        (*escaped-output-path* (escape-string (namestring output-path))))
    (with-open-file (out (merge-pathnames makefile-name output-path)
                         :direction :output
                         :if-exists :supersede)
      (makefile-setup output-path out)
      (dolist (node all-nodes) (write-node-to-makefile out node))
      (format out "~@[~{~a~^ ~} : core-with-build-requires.core-xcvb~%~]"
              *targets-dependent-on-cwbrl*))))
