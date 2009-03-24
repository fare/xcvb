;;;; XCVB module name resolution

(in-package :xcvb)

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

#|
(defun canonicalize-fullname (name)
  "This function prepends a #\/ to the beginning of the module's fullname,
if there isn't one there already"
  (if (eql #\/ (first-char name))
    name
    (strcat "/" name)))
;; use :allow-relative nil instead of :allow-absolute nil in valid-fullname-p
|#
(defun canonicalize-fullname (name)
  "This function *removes* any #\/ from the beginning of the module's fullname."
  (subseq name (position-if-not (lambda (x) (eql x #\/)) name) (length name)))

(defmethod compute-fullname :after ((grain grain))
  (validate-fullname (slot-value grain 'fullname)))

(defun valid-fullname-p (name)
  (ignore-errors (portable-pathname-from-string name :allow-absolute nil)))

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
