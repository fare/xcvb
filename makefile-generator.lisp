(in-package :xcvb)

(defparameter *build-requires-p* nil
  "Flag to specify if the build module's build-requires slot has been set - and
 therefore whether or not to have a core-with-build-requires.core-xcvb target")
(defvar *written-nodes* nil "A map of the nodes that have already been written 
to the makefile, to avoid writing any node twice")
(defvar *targets-dependent-on-cwbrl* nil "A list of the makefile targets that 
have a dependency on the core-with-build-requires.core-xcvb target")


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
	   (otherwise (format out-string "~a" c))))))


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


(defgeneric form-string-for-node (node)
  (:documentation "Returns a string of the lisp form that will perform the 
action that the node represents"))

(defmethod form-string-for-node ((node asdf-system-node))
  (format nil 
          "(asdf:oos 'asdf:load-op :~a)" 
          (escape-string (namestring (name node)))))

(defmethod form-string-for-node ((node image-dump-node))
  (save-image-form 
   (escape-string (namestring (merge-pathnames *buildpath* (target node))))))

(defmethod form-string-for-node ((node source-file-node))
  (let ((source-file-path 
         (escape-string (namestring (merge-pathnames (target node) *buildpath*)))))
    (if *use-cfasls*
      (format nil 
              "#+cfasls (cl:compile-file \\\"~a\\\" ~
:emit-cfasl T)#-cfasls (cl:compile-file \\\"~:*~a\\\")" 
              source-file-path)
      (format nil "(cl:compile-file \\\"~a\\\")"
              source-file-path))))

#|(defmethod form-string-for-node ((node cfasl-node))
  (format nil "#+cfasls (cl:load \"~a\")#-cfasls (progn ~{~@[~a~^ ~]~})"
          (namestring (merge-pathnames (target node) *buildpath*)) 
          (mapcar #'form-string-for-node (traverse node :create))))
          ;(namestring (merge-pathnames (target (first (compile-dependencies node))) *buildpath*))))|#

(defmethod form-string-for-node ((node fasl-node))
  (format nil 
          "(cl:load \\\"~a\\\")" 
          (make-pathname :type "${FASL}"
                         :defaults (escape-string (namestring (merge-pathnames 
                                                               (target node) 
                                                               *buildpath*))))))

(defmethod form-string-for-node ((node cfasl-node))
  (let ((source-file-path 
         (escape-string 
          (namestring 
           (merge-pathnames 
            (target (first (compile-dependencies node))) 
            *buildpath*)))))
    (if *use-cfasls*
      (format nil 
              "#+cfasls (cl:load \\\"~a\\\")#-cfasls (cl:compile-file \\\"~a\\\")"
              (make-pathname :type "${CFASL}" 
                             :defaults (escape-string (namestring 
                                                       (merge-pathnames 
                                                        (target node) 
                                                        *buildpath*))))
              source-file-path)              
      (format nil "(cl:compile-file \"~a\")"
              source-file-path))))


(defmethod form-string-for-node ((node dependency-graph-node))
  (declare (ignore node)))


(defun makefile-line-for-node (node operation)
  "Returns the string of the line in the Makefile that will be used to build the
 target"
  (let* ((traversal (traverse node operation)))
    (format nil "${~:[LISPRUN~;CWBRLRUN~]} \"~a\"" 
            *build-requires-p* 
            ;(escape-string
            (format nil "(progn ~{~@[~a~^ ~]~} ~a)"
                    (mapcar #'form-string-for-node traversal) 
                    (quit-form)))));)


(defgeneric write-node-to-makefile (filestream node)
  (:documentation "Writes a makefile target to the filestream for building the 
given node"))
  

(defmethod write-node-to-makefile (filestream (node fasl-or-cfasl-node))
  (format T "writing (c)fasl node: ~a~%" (fullname node))
  ;;If this node has already been written to the makefile, don't write it again.
  (unless (or (nth-value 1 (gethash 
                            (namestring (make-pathname 
                                         :type "${FASL}" 
                                         :defaults (fullname node))) 
                            *written-nodes*))
              (nth-value 1 (gethash 
                            (namestring (make-pathname 
                                         :type "${CFASL}" 
                                         :defaults (fullname node))) 
                            *written-nodes*)))
    ;;Add this node to the map of nodes already written to the makefile
    (setf (gethash (fullname node) *written-nodes*) nil)
    (when *build-requires-p*
      (push (namestring (make-pathname :type "${FASL}" :defaults (target node))) 
            *targets-dependent-on-cwbrl*))
      ;;(push (namestring (make-pathname :type "cfasl" :defaults (target node))) *targets-dependent-on-cwbrl*))
    (format filestream "~a : ~{~a~^ ~}~%" 
            (make-pathname :type "${FASL}" :defaults (target node))
            ;(make-pathname :type "cfasl" :defaults (target node))
            (mapcar (lambda (x) (if (typep x 'cfasl-node) 
                                  (make-pathname 
                                   :type "${FASL}" 
                                   :defaults (target x)) 
                                  (target x)))
                    (traverse node :create)))
            ;(mapcar #'target (traverse node :create)))
    (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node :create))))

(defmethod write-node-to-makefile (filestream (node image-dump-node))
  (format filestream "~a : ~{~a~^ ~}~%" 
          (target node) 
          (mapcar (lambda (x) (if (typep x 'cfasl-node) 
                                (make-pathname 
                                 :type "${FASL}" 
                                 :defaults (target x)) 
                                (target x)))
                  (traverse node :load)))
          ;(mapcar #'target (traverse node :load)))
  (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node :create)))

(defmethod write-node-to-makefile (filestream (node lisp-node))
  (format filestream ".PHONY: ~a~%~:*~a : ~{~a~^ ~}~%" 
          (target node) 
          (mapcar (lambda (x) (if (typep x 'cfasl-node) 
                                (make-pathname 
                                 :type "${FASL}" 
                                 :defaults (target x))
                                (target x)))
                  (traverse node :create)))
          ;(mapcar #'target (traverse node :create)))
  (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node :create)))

(defmethod write-node-to-makefile (filestream (node asdf-system-node))
  ;;If this node has already been written to the makefile, don't write it again.
  (unless (nth-value 1 (gethash (fullname node) *written-nodes*))
    ;;Add this node to the map of nodes already written to the makefile
    (setf (gethash (fullname node) *written-nodes*) nil)
    (format filestream ".PHONY: ~a~%~:*~a : ~%" (target node))
    (format filestream "~a~a~%~%" #\tab (makefile-line-for-node node :load))))
;    (format filestream ".PHONY: ~a~%~%" (target node))))
  
(defmethod write-node-to-makefile (filestream node)
  (declare (ignore filestream node)))


(defun makefile-setup (output-path filestream)
  "Writes information to the top of the makefile about how to actually run lisp 
to compile files, and also how to create an image to use that contains all the 
dependencies from the build-requires slot of the build module loaded"
  (format filestream "LISPRUN = ~a~%~%" (eval-command-string))
  (when (build-requires *build-module*)
    (let* ((cwbrlpath (escape-string (namestring 
                                      (make-pathname 
                                       :name "core-with-build-requires" 
                                       :type "core-xcvb" 
                                       :defaults output-path))))
           (core-with-build-requires-graph 
            (create-image-dump-node (create-lisp-node 
                                     (pushnew (list :asdf "xcvb") 
                                              (build-requires *build-module*)))
                                    cwbrlpath)))
      (format filestream "CWBRL = ~a~%~%" cwbrlpath)
      (format filestream "CWBRLRUN = ~a~%~%" 
              (eval-command-string :cwbrl-as-core-p T))
      (format filestream "FASL := $(shell ${LISPRUN} \"(progn (format t \\\"~~a\\\" (pathname-type (compile-file-pathname \\\"test.lisp\\\"))) (sb-ext:quit))\")~%~%")
      (format filestream "CFASL := cfasl~%~%")
      (format filestream 
              "CHECK_ASDFS = $(shell if ! ( [ -f ${CWBRL} ] && [(${CWBRLRUN} ~
\"(asdf::asdf-systems-are-up-to-date-p ~{:~a~^ ~})\")]) ; ~
then echo force ; fi )~%~%force : ~%.PHONY: force~%~%"
              (mapcar (lambda (dep) (escape-string (namestring (name dep)))) 
                      (remove-if-not 
                       (lambda (dep) (typep dep 'asdf-system-node)) 
                       (traverse core-with-build-requires-graph :create))))
      (dolist (node 
                (traverse (lisp-image core-with-build-requires-graph) :create))
        (write-node-to-makefile filestream node))
      (format filestream 
              "core-with-build-requires.core-xcvb : $(CHECK_ASDFS) ~{~a~^ ~}~%" 
              (mapcar (lambda (dep) (escape-string (target dep))) 
                      (remove-if 
                       (lambda (dep) (typep dep 'asdf-system-node)) 
                       (traverse core-with-build-requires-graph :load))))
      (format filestream "~a~a~%~%" 
              #\tab 
              (makefile-line-for-node core-with-build-requires-graph :create))
      (setf *build-requires-p* T))))



(defun write-makefile (source-path 
                       output-path &key
                       (makefile-name "Makefile.xcvb") 
                       (image-name "lisp-image.core"))
  "Writes a makefile to output-path with information about how to compile the file at source-path.  What the makefile is designed to do can be specified by graph-type" 
  (with-open-file (out (merge-pathnames output-path makefile-name) 
                       :direction :output 
                       :if-exists :supersede)
    (let ((dependency-graph
           (create-dump-image-graph 
            (merge-pathnames output-path image-name)
            source-path)))
      (let ((*written-nodes* (make-hash-table :test #'equal))
            (all-nodes (traverse dependency-graph :all))
            (*targets-dependent-on-cwbrl* nil)
            (*build-requires-p* nil))
        ;(setf *build-requires-p* nil)
        (makefile-setup output-path out)
        (dolist (node all-nodes) (write-node-to-makefile out node))
        (format out "~@[~{~a~^ ~} : core-with-build-requires.core-xcvb~%~]" 
                *targets-dependent-on-cwbrl*)))))




#|(defun write-makefile (source-path output-path &optional (graph-type :image-dump))
  "Writes a makefile to output-path with information about how to compile the file at source-path.  What the makefile is designed to do can be specified by graph-type" 
  (with-open-file (out output-path :direction :output :if-exists :supersede)
    (let ((dependency-graph
           (case graph-type
             (:image-dump (create-dump-image-graph 
                           (make-pathname :name "lisp-image" :type "core" :defaults output-path) source-path))
             (otherwise (error "Unknown graph-type")))))
      (let ((*written-nodes* (make-hash-table :test #'equal))
            (all-nodes (traverse dependency-graph :all))
            (*targets-dependent-on-cwbrl* nil))
        (setf *build-requires-p* nil)
        (makefile-setup output-path out)
        (dolist (node all-nodes) (write-node-to-makefile out node))
        (format out "~@[~{~a~^ ~} : core-with-build-requires.core-xcvb~%~]" *targets-dependent-on-cwbrl*)))))|#
