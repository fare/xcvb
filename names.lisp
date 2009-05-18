;;;; XCVB module name resolution

(in-package :xcvb)

(defvar +BUILD-path+
  (make-pathname :name "BUILD" :type "lisp"))

(defvar +lisp-path+
  (make-pathname :type "lisp"))

(defvar *pathname-grain-cache*
  (make-hash-table :test 'equal)
  "Registry of known files, indexed by namestring.
Negatives are stored as NIL. Positives as grains.")

(defun probe-file-grain (path &key build-p)
  (unless (absolute-pathname-p path)
    (setf path (truename path)))
  (let ((string (namestring path)))
    (multiple-value-bind (cached found)
        (gethash string *pathname-grain-cache*)
      (if found
        cached
        (let* ((probed (probe-file path))
               (module (when probed (make-grain-from-file probed :build-p build-p))))
          (setf (gethash string *pathname-grain-cache*) module)
          (when probed
            (setf (gethash (namestring probed) *pathname-grain-cache*) module))
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
  ;; * bork if it isn't a portablish-pathname
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
          (grain-parent grain) nil
          (grain-relative-name grain) nil)
    (compute-inherited-fullname grain :build-p t))
  (values))

(defmethod compute-fullname ((grain lisp-grain))
  (compute-inherited-fullname grain :build-p nil))

(defun compute-inherited-fullname (grain &key build-p)
  (check-type grain lisp-grain)
  (let* ((truename (truename (grain-pathname grain)))
         (host (pathname-host truename))
         (device (pathname-device truename))
         (rdirectory (reverse (pathname-directory truename))))
    (labels ((err ()
               (error "grain ~A is lacking an explicit or implicit fullname"
                      (grain-pathname grain)))
             (maybe-inherit-from (rdir subnames)
               (let ((ancestor (probe-file-grain
                                (make-pathname :host host :device device
                                               :name "BUILD" :type "lisp"
                                               :directory (reverse rdir))
                                :build-p t)))
                 (if ancestor
                     (let ((relname (join-strings "/" subnames)))
                       (setf (grain-parent grain) ancestor
                             (grain-relative-name grain) relname
                             (fullname grain) (strcat (fullname ancestor) "/" relname)))
                     (recurse rdir subnames))))
             (recurse (rdir subnames)
               (let ((dir (car rdir)))
                 (if (stringp dir)
                   (maybe-inherit-from (cdr rdir) (cons dir subnames))
                   (err)))))
      (if build-p
        (recurse rdirectory nil)
        (maybe-inherit-from rdirectory (list (pathname-name truename)))))))

(defun build-grain-for (grain)
  (etypecase grain
    (build-grain grain)
    (lisp-grain (grain-parent grain))))

(defun resolve-module-name (name grain)
  "Resolve module NAME in the context of BUILD into an appropriate grain, if any"
  (if (portablish-pathname-absolute-p name)
    (resolve-absolute-module-name name)
    (loop :for b = (build-grain-for grain) :then (grain-parent b)
          :for g = (and b (resolve-module-name-at name b))
          :while b
          :when (typep g 'grain) :do (return g)
          :finally (return (resolve-absolute-module-name (canonicalize-fullname name))))))

(defun module-subpathname (path name)
  (subpathname (merge-pathnames +lisp-path+ path) name))

(defun resolve-absolute-module-name (name)
  "Resolve absolute NAME into an appropriate grain, if any"
  (unless (absolute-portablish-namestring-p name)
    (error "~S isn't a valid module name" name))
  (loop :for p = (length name) then (position #\/ name :from-end t :end p)
        :for prefix = (if (and p (plusp p))
                        (subseq name 0 p)
                        (return nil))
        :for postfix = nil then (subseq name (1+ p))
        :for build = (if prefix (registered-grain prefix))
        :for grain = (resolve-module-name-at postfix build)
        :when (typep grain 'grain) :do (return grain)))

(defun resolve-module-name-at (name build)
  (typecase build
    (null ;; no entry -- look further
     nil)
    (build-grain
     (if name
       (probe-file-grain (module-subpathname (grain-pathname build) name))
       build))
    (build-registry-conflict
     (error "Trying to use component ~S for conflicted build name ~S"
            name (fullname build)))
    (t ;; some entry that isn't a build -- ignore. Or should we error out or at least warn?
     nil)))