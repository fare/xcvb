;;;;; Registry mapping names to grains, particularly BUILD files.
#+xcvb (module (:depends-on ("portablish-pathnames" "grains")))

(in-package :xcvb)


;;; The registry itself

(defparameter *grains*
  (make-hash-table :test 'equal)
  "A registry of known grains,
indexed by normalized name, either fullname of a module,
nickname, or SEXP representing a computed entity.
Initially populated with all build.xcvb files from the search path,
then enriched as we build the graph from the main build.xcvb file.")

(defparameter *superseded-asdf*
  (make-hash-table :test 'equalp))

(defun registered-grain (name)
  (gethash name *grains*))

(defun (setf registered-grain) (grain name)
  (setf (gethash name *grains*) grain))

(defun registered-build (name)
  (let ((grain (registered-grain name)))
    (unless (build-grain-p grain)
      (error "Could not find a build with requested fullname ~A. Try xcvb show-search-path" name))
    (check-type grain build-grain)
    grain))

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

(defun supersedes-asdf-name (x)
  `(:supersedes-asdf ,(coerce-asdf-system-name x)))


(defun register-build-file (build root)
  "Registers build file build.xcvb (given as pathname)
as having found under root path ROOT (another pathname),
for each of its registered names."
  ;;(format *error-output* "~&Found build file ~S in ~S~%" build root)
  (let* ((build-grain (make-grain-from-file build :build-p t))
         (fullname (when build-grain (fullname build-grain))))
    (when (and fullname (not (slot-boundp build-grain 'root)))
      (setf (bre-root build-grain) root)
      (register-build-named fullname build-grain root)))
  (values))

(defun register-build-nicknames-under (root)
  (dolist (b (remove-duplicates
              (loop :for b :being :the :hash-values :of *grains*
                :when (and (build-grain-p b) (equal (bre-root b) root)) :collect b)))
    (dolist (name (append (mapcar #'canonicalize-fullname (nicknames b))
                          (mapcar #'supersedes-asdf-name (supersedes-asdf b))))
      (register-build-named name b root))))

(defun merge-build (previous-build new-build name root)
  ;; Detect ambiguities.
  ;; If the name has already been registered, then
  ;; * if the previous entry is from a previous root, it has precedence
  ;; * if the previous entry is from same root and is in an ancestor directory, it has precedence
  ;; * otherwise, it's a conflict, and the name shall be marked as conflicted and an error be printed
  ;;  if/when it is used.
  ;; Note: to do that in a more functional way, have some mechanism
  ;; that applies a modify-function to a gethash value, allowing (values NIL NIL) to specify remhash.
  (check-type previous-build (or null build-registry-conflict build-grain))
  (cond
    ((null previous-build)
     ;; we're the first entry with that name. Bingo!
     new-build)
    ((equal (bre-root previous-build) root)
     ;; There was a previous entry in same root:
     ;; there's an ambiguity, so that's a conflict!
     (make-instance 'build-registry-conflict
                    :fullname name
                    :pathnames (cons (grain-pathname new-build) (brc-pathnames previous-build))
                    :root root))
    (t
     ;; There was a previous entry in a previous root,
     ;; the previous entry takes precedence -- do nothing.
     previous-build)))

(defun register-build-named (name build-grain root)
  "Register under NAME pathname BUILD found in user-specified ROOT."
  (funcallf (registered-grain name) #'merge-build build-grain name root))

