;;;; XCVB module name resolution
#+xcvb (module (:depends-on ("grain-registry" "grain-interface")))

(in-package :xcvb)

(defvar +build-path+
  (make-pathname :name "build" :type "xcvb"))

(defvar +lisp-path+
  (make-pathname :type "lisp"))

(defun probe-file-grain (path &key build-p)
  (let* ((path (ensure-pathname-absolute path))
         (string (namestring path)))
    (multiple-value-bind (cached found)
        (gethash string *pathname-grain-cache*)
      (if found
	  cached
	  (let* ((probed (probe-file path))
		 (module (when probed
			     (make-grain-from-file
			      path ;; Use path instead of probed so symlinks still work.
			      :build-p build-p))))
	    (setf (gethash string *pathname-grain-cache*) module)
	    module)))))

(defmethod specified-fullname ((module lisp-module-grain))
  nil)

(defgeneric compute-fullname (grain))

(defun ensure-valid-fullname (name &key type (original-name name))
  (unless (or (and (null type) (valid-fullname-p name))
              (and (consp name) (eq type (car name))
                   (consp (cdr name)) (valid-fullname-p (cadr name))
                   (null (cddr name))))
    (error "~S is not a valid XCVB fullname~@[ for grain type ~A~]"
		original-name type))
  name)

(defun canonicalize-fullname (name)
  "This function makes sure the fullname is canonical:
* prepends a #\/ to the beginning of the module's fullname if there isn't one there already
* strips any tailing #\/"
  ;; should also:
  ;; * bork if it isn't a portable-pathname
  ;; * remove extraneous #\/'s
  (let* ((name-no/
	  (if (eql #\/ (last-char name))
	      (subseq name 0 (1- (length name)))
	      name))
	 (/name-no/
	  (if (eql #\/ (first-char name-no/))
	      name-no/
	      (strcat "/" name-no/))))
    (ensure-valid-fullname /name-no/ :original-name name)
    /name-no/))

(defun valid-fullname-p (name)
  (ignore-errors (equal name (portable-pathname-output (portable-pathname-from-string name)))))


(defgeneric validate-fullname (grain))

(defmethod validate-fullname ((grain lisp-module-grain))
  (ensure-valid-fullname (fullname grain) :type :lisp))
(defmethod validate-fullname ((grain fasl-grain))
  (ensure-valid-fullname (fullname grain) :type :fasl))
(defmethod validate-fullname ((grain cfasl-grain))
  (ensure-valid-fullname (fullname grain) :type :cfasl))
(defmethod validate-fullname ((grain lisp-object-grain))
  (ensure-valid-fullname (fullname grain) :type :lisp-object))
(defmethod validate-fullname ((grain static-library-grain))
  (ensure-valid-fullname (fullname grain) :type :static-library))
(defmethod validate-fullname ((grain dynamic-library-grain))
  (ensure-valid-fullname (fullname grain) :type :dynamic-library))
(defmethod validate-fullname ((grain build-module-grain))
  (ensure-valid-fullname (fullname grain)))

(defmethod compute-fullname ((grain build-module-grain))
  (unless (slot-boundp grain 'fullname)
    (if (specified-fullname grain)
        (setf (fullname grain) (canonicalize-fullname (specified-fullname grain))
              (grain-parent grain) nil)
        (setf (fullname grain) (inherited-fullname grain :build-p t))))
  (values))

(defmethod compute-fullname ((grain lisp-file-grain))
  (unless (slot-boundp grain 'fullname)
    (setf (fullname grain)
          `(:lisp ,(inherited-fullname grain :build-p nil)))))

(defun inherited-fullname (grain &key build-p)
  (check-type grain lisp-module-grain)
  (let* ((pathname (ensure-pathname-absolute (grain-pathname grain)))
         (rdirectory (reverse (pathname-directory pathname))))
    (log-format 20 "   ~:[~;build ~]grain at ~A is missing a fullname; computing it"
                build-p pathname)
    (labels ((maybe-inherit-from (rdir subnames)
               (let ((ancestor (probe-file
                                (make-pathname :defaults pathname
                                               :name "build" :type "xcvb"
                                               :directory (reverse rdir)))))
                 (if ancestor
                     (let ((ancestor-fullname (fullname-from-truename ancestor)))
                       (log-format 20 "    found ancestor ~A~@[ with fullname ~A~]"
                                   ancestor ancestor-fullname)
                       (if (null ancestor-fullname)
                           (error "grain ~A has unregistered ancestor at ~A"
                                  (grain-pathname grain) ancestor)
                           (let ((ancestor-build (registered-build ancestor-fullname)))
                             (setf (grain-parent grain) ancestor-build)
                             (join-strings (cons ancestor-fullname subnames)
                                           :separator "/"))))
                     (recurse rdir subnames))))
             (recurse (rdir subnames)
               (let ((dir (car rdir)))
                 (if (stringp dir)
                   (maybe-inherit-from (cdr rdir) (cons dir subnames))
                   (error "grain ~A is lacking an explicit or implicit fullname"
                          (grain-pathname grain))))))
      (if build-p
        (recurse rdirectory nil)
        (maybe-inherit-from rdirectory (list (pathname-name pathname)))))))

(defun resolve-module-name (name grain)
  "Resolve module NAME in the context of build into an appropriate grain, if any"
  (check-type name string)
  (if (portable-pathname-absolute-p name)
    (resolve-absolute-module-name name)
    (loop
      :for b = (build-module-grain-for grain) :then (grain-parent b)
      :for g = (and b (resolve-absolute-module-name
                       (strcat (fullname b) "/" name)))
      :while b
      :when (typep g 'grain) :do (return g)
      :finally (return (resolve-absolute-module-name
                        (canonicalize-fullname name))))))

(defun module-subpathname (path name)
  (subpathname path (strcat name ".lisp")))

(defun walk-build-ancestry (name description build-handler)
  "Call BUILD-HANDLER on each build the fullname of which is a prefix of NAME,
with the SUFFIX from that fullname to NAME as second argument, in order
of decreasing fullname length"
  (unless (absolute-portable-namestring-p name)
    (error "~S isn't a valid ~A" name description))
  (loop
    :for p = (length name) then (position #\/ name :from-end t :end p)
    :for prefix = (if (and p (plusp p))
                      (subseq name 0 p)
                      (return nil))
    :for suffix = nil then (subseq name (1+ p))
    :for build = (when prefix (registered-build prefix)) :do
    (etypecase build
      (null
       nil)
      (build-module-grain
       (funcall build-handler build suffix))
      (invalid-build-registry-entry
       (error "~:@<Trying to use invalid build name ~S while resolving ~A ~S (~A)~:>"
              prefix description suffix (invalid-build-reason build)))))
  (values))

(defun resolve-build-relative-name (name &optional (description "build-relative name"))
  "Resolve absolute NAME into a build and relative name"
  (block nil
    (walk-build-ancestry
     name description
     (lambda (build suffix)
       (return (values build suffix))))))

(defun resolve-asdf-name (name)
  (registered-build `(:asdf ,name)))

;; TODO: say if we resolve as a build, lisp, executable, image, etc. ?
(defun resolve-absolute-module-name (name &key error-p)
  "Resolve absolute NAME into an appropriate grain, if any"
  (multiple-value-bind (build suffix)
      (resolve-build-relative-name (canonicalize-fullname name) "module name")
    (if build
        (let ((grain (resolve-module-name-at suffix build)))
          (if (typep grain 'grain)
              grain
              (when error-p
                (error "No grain ~S under build ~A" suffix (fullname build)))))
        (when error-p
          (error "No build for name ~A" name)))))

(defun resolve-module-name-at (suffix build)
  (check-type build build-module-grain)
  (if (null suffix)
      build
      (let ((fullname (strcat (fullname build) "/" suffix)))
	(finalize-grain build)
        (or (registered-grain fullname)
            (registered-grain `(:lisp ,fullname))
            (registered-grain `(:executable ,fullname))
            (probe-file-grain
             (module-subpathname (grain-pathname build) suffix))))))

(defun ensure-name-within-build (build name)
  (let* ((build-name (fullname build))
         (bn/ (strcat build-name "/")))
    (multiple-value-bind (relative-name absolute-name)
        (cond
          ((portable-pathname-absolute-p name)
           (unless (string-prefix-p bn/ name)
             (error "Specified name ~A isn't relative to build ~A" name build-name))
           (values (subseq name (length bn/)) name))
          (t
           (values name (strcat bn/ name))))
      (multiple-value-bind (actual-build actual-name)
          (resolve-build-relative-name absolute-name)
        (unless (and (eq build actual-build) (equal relative-name actual-name))
          (error "Specified name ~A isn't under build ~A but under build ~A"
                 name build-name (fullname actual-build))))))
  t)
