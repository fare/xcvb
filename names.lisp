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
  "This function prepends a #\/ to the beginning of the module's fullname,
if there isn't one there already, and strips any tailing #\/"
  (when (eql #\/ (last-char name))
    (setf name (subseq name 0 (1- (length name)))))
  (unless (eql #\/ (first-char name))
    (setf name (strcat "/" name)))
  name)

#|
(defun canonicalize-fullname (name)
  "This function *removes* any #\/ from the beginning of the module's fullname."
  (subseq name (position-if-not (lambda (x) (eql x #\/)) name) (length name)))
|#

(defmethod compute-fullname :after ((grain grain))
  (validate-fullname (slot-value grain 'fullname)))

(defun valid-fullname-p (name)
  (ignore-errors (portable-pathname-from-string name :allow-relative nil)))

(defun validate-fullname (name)
  (unless (valid-fullname-p name)
    (error "Invalid grain fullname ~A" name)))

(defmethod compute-fullname ((build build-grain))
  (if (specified-fullname build)
    (setf (fullname build) (canonicalize-fullname (specified-fullname build))
          (grain-parent build) nil
          (grain-relative-name build) nil)
    (call-next-method)))

(defmethod compute-fullname ((grain lisp-grain))
  (loop :with truename = (truename (grain-pathname grain))
        :with directory = (pathname-directory truename)
        :with subnames = ()
        :for (dir . rdir) :on (reverse directory) :do
          (cond
            ((stringp dir)
             (push dir subnames)
             ;;(DBG :cf grain subnames)
             (let ((ancestor (probe-file-grain
                              (make-pathname :name "BUILD" :type "lisp"
                                             :directory (reverse rdir))
                              :build-p t)))
               (when ancestor
                 (setf (grain-parent grain)
                       ancestor
                       (grain-relative-name grain)
                       (join-strings "/" subnames)
                       (fullname grain)
                       (strcat (fullname ancestor) "/" (grain-relative-name grain)))
                 ;;(DBG :cf2 ancestor (grain-relative-name grain) (slot-value grain 'fullname))
                 (return))))
            (t
             (error "grain ~A is lacking an explicit or implicit fullname" (grain-pathname grain))))))

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
    (t ;; some entry that isn't a build -- ignore
     nil)))
