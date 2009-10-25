;;;; XCVB module name resolution
#+xcvb (module (:depends-on ("registry" "specials")))

(in-package :xcvb)

(defvar +build-path+
  (make-pathname :name "build" :type "xcvb"))

(defvar +lisp-path+
  (make-pathname :type "lisp"))

(defun probe-file-grain (path &key build-p)
  (let* ((path (ensure-absolute-pathname path))
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

(defmethod fullname ((module lisp-grain))
  (unless (slot-boundp module 'fullname)
    (compute-fullname module))
  (slot-value module 'fullname))

(defmethod specified-fullname ((module lisp-grain))
  nil)

(defgeneric compute-fullname (grain))

(defun canonicalize-fullname (name)
  "This function makes sure the fullname is canonical:
* prepends a #\/ to the beginning of the module's fullname if there isn't one there already
* strips any tailing #\/"
  ;; should also:
  ;; * bork if it isn't a portable-pathname
  ;; * remove extraneous #\/'s
  (when (eql #\/ (last-char name))
    (setf name (subseq name 0 (1- (length name)))))
  (unless (eql #\/ (first-char name))
    (setf name (strcat "/" name)))
  (validate-fullname name)
  name)

#|
(defun canonicalize-fullname (name)
  "This function *removes* any #\/ from the beginning of the module's fullname."
  (subseq name (position-if-not (lambda (x) (eql x #\/)) name) (length name)))
|#

(defmethod compute-fullname :after ((grain grain))
  (validate-fullname (slot-value grain 'fullname)))

(defun valid-fullname-p (name)
  (ignore-errors (equal name (portable-pathname-output (portable-pathname-from-string name)))))

(defun validate-fullname (name)
  (unless (valid-fullname-p name)
    (error "Invalid grain fullname ~A" name))
  name)

(defmethod compute-fullname ((grain build-grain))
  (if (specified-fullname grain)
    (setf (fullname grain) (canonicalize-fullname (specified-fullname grain))
          (grain-parent grain) nil)
    (compute-inherited-fullname grain :build-p t))
  (values))

(defmethod compute-fullname ((grain lisp-grain))
  (compute-inherited-fullname grain :build-p nil))

(defun compute-inherited-fullname (grain &key build-p)
  (check-type grain lisp-grain)
  (let* ((pathname (ensure-absolute-pathname (grain-pathname grain)))
         (host (pathname-host pathname))
         (device (pathname-device pathname))
         (rdirectory (reverse (pathname-directory pathname))))
    (labels ((err ()
               (error "grain ~A is lacking an explicit or implicit fullname"
                      (grain-pathname grain)))
             (maybe-inherit-from (rdir subnames)
               (let ((ancestor (probe-file-grain
                                (make-pathname :host host :device device
                                               :name "build" :type "xcvb"
                                               :directory (reverse rdir))
                                :build-p t)))
                 (if ancestor
                     (let ((relname (join-strings "/" subnames)))
                       (setf (grain-parent grain) ancestor
                             (fullname grain) (strcat (fullname ancestor) "/" relname)))
                     (recurse rdir subnames))))
             (recurse (rdir subnames)
               (let ((dir (car rdir)))
                 (if (stringp dir)
                   (maybe-inherit-from (cdr rdir) (cons dir subnames))
                   (err)))))
      (if build-p
        (recurse rdirectory nil)
        (maybe-inherit-from rdirectory (list (pathname-name pathname)))))))

(defun resolve-module-name (name grain)
  "Resolve module NAME in the context of build into an appropriate grain, if any"
  (if (portable-pathname-absolute-p name)
    (resolve-absolute-module-name name)
    (loop :for b = (build-grain-for grain) :then (grain-parent b)
          :for g = (and b (resolve-absolute-module-name (strcat (fullname b) "/" name)))
          :while b
          :when (typep g 'grain) :do (return g)
          :finally (return (resolve-absolute-module-name (canonicalize-fullname name))))))

(defun module-subpathname (path name)
  (subpathname path  (strcat name ".lisp")))

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
    :for build = (if prefix (registered-grain prefix)) :do
    (typecase build
      (build-grain
       (funcall build-handler build suffix))
      (build-registry-conflict
       (error "Trying to use conflicted build name ~S while resolving ~A ~S"
              prefix description suffix))))
  nil)

(defun resolve-absolute-module-name (name)
  "Resolve absolute NAME into an appropriate grain, if any"
  (or (registered-grain name)
      (walk-build-ancestry
       name "module name"
       (lambda (build suffix)
         (let ((grain (resolve-module-name-at suffix build)))
           (when (typep grain 'grain)
             (return-from resolve-absolute-module-name grain)))))))

(defun resolve-module-name-at (name build)
  (check-type build build-grain)
  (if name
      (probe-file-grain (module-subpathname (grain-pathname build) name))
      build))

(defun resolve-build-relative-name (name)
  "Resolve absolute NAME into a build and relative name"
  (walk-build-ancestry
   name "build-relative filename"
   (lambda (build suffix)
     (return-from resolve-build-relative-name (values build suffix)))))
