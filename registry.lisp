;;;;; Registry mapping names to grains, particularly BUILD files.

(in-package :xcvb)


;;; The registry itself

(defparameter *grains*
  (make-hash-table :test 'equal)
  "A registry of known grains,
indexed by normalized name, either fullname of a module,
nickname, or SEXP representing a computed entity.
Initially populated with all BUILD.lisp files from the search path,
then enriched as we build the graph from the main BUILD file.")

(defun registered-grain (name)
  (gethash name *grains*))

(defun (setf registered-grain) (grain name)
  (setf (gethash name *grains*) grain))

(defun call-with-grain-registration (fullname function &rest args)
  (let ((previous (registered-grain fullname)))
    (or previous
        (let ((grain (apply function args)))
          (setf (registered-grain fullname) grain)
          grain))))

(defun make-grain (class &rest args &key fullname &allow-other-keys)
  (apply #'call-with-grain-registration fullname #'make-instance class args))

;;; Special magic for build entries in the registry

(defclass build-registry-entry ()
  ((root
    :initarg :root :accessor bre-root
    :documentation "root path under which the entry was found")))

(defclass build-registry-conflict (build-registry-entry simple-print-object-mixin)
  ((fullname
    :initarg :fullname :reader fullname)
   (pathnames
    :initarg :pathnames :reader brc-pathnames
    :documentation "pathnames of conflicting build files with the same name")))

(defmethod brc-pathnames ((build build-grain))
  (list (grain-pathname build)))


;;; Registering a build

(defun register-build-file (build root)
  "Registers build file BUILD (given as pathname)
as having found under root path ROOT (another pathname),
for each of its registered names."
  ;;(format *error-output* "~&Found build file ~S in ~S~%" build root)
  (let* ((build-grain (make-grain-from-file build :build-p t))
         (fullname (when build-grain (fullname build-grain))))
    (when fullname
      (setf (bre-root build-grain) root)
      (dolist (name (cons fullname (nicknames build-grain)))
        (register-build-named name build-grain root))))
  (values))

(defun register-build-named (name build-grain root)
  "Register under NAME pathname BUILD found in user-specified ROOT."
  ;; Detect ambiguities.
  ;; If the name has already been registered, then
  ;; * if the previous entry is from a previous root, it has precedence
  ;; * if the previous entry is from same root and is in an ancestor directory, it has precedence
  ;; * otherwise, it's a conflict, and the name shall be marked as conflicted and an error be printed
  ;;  if/when it is used.
  ;; Note: to do that in a more functional way, have some mechanism
  ;; that applies a modify-function to a gethash value, allowing (values NIL NIL) to specify remhash.
  (let ((previous-build (registered-grain name)))
    (assert (typep previous-build '(or null build-registry-conflict build-grain)))
    (cond
      ((null previous-build)
       ;; we're the first entry with that name. Bingo!
       (setf (registered-grain name) build-grain))
      ((equal (bre-root previous-build) root)
       ;; There was a previous entry in same root:
       ;; there's an ambiguity, let's register a conflict!
       (setf (registered-grain name)
             (make-instance 'build-registry-conflict
               :fullname name
               :pathnames (cons (grain-pathname build-grain) (brc-pathnames previous-build))
               :root root)))
      (t
       ;; There was a previous entry in a previous root,
       ;; the previous entry takes precedence -- do nothing.
       )))
  (values))
