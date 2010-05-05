;;; Handle the Search Path for XCVB modules.
#+xcvb (module (:depends-on ("main" "registry")))
(in-package :xcvb)

;;; The Source Registry itself.
;;; We directly use the code from ASDF, therefore ensuring 100% compatibility.

(defparameter *source-registry* ()
  "Either NIL (for uninitialized), or a list of one element,
said element itself being a list of directory pathnames where to look for build.xcvb files")

(defparameter *source-registry-searched-p* nil
  "Has the source registry been searched yet?")

(defparameter *builds*
  (make-hash-table :test 'equal)
  "A registry of known builds, indexed by canonical name.
Initially populated with all build.xcvb files from the search path.")

(defparameter *truename-build-fullnames*
  (make-hash-table :test 'equal)
  "A registry of known build fullnames, indexed by truename.
Initially populated with all build.xcvb files from the search path.")

;;; Special magic for build entries in the registry

(defmethod brc-pathnames ((build build-module-grain))
  (list (grain-pathname build)))
(defmethod brc-pathnames ((build invalid-build-file))
  (list (grain-pathname build)))

(defun make-invalid-ancestor-build-file (&key fullname pathname ancestor root)
  (make-instance 'invalid-build-file
                 :root root
                 :fullname fullname
                 :pathname pathname
                 :reason (format nil "ancestor ~A is invalid because~%~A~&"
                                 (fullname ancestor)
                                 (make-invalid-build-reason ancestor))))

(defgeneric make-invalid-build-reason (x &key &allow-other-keys))

(defmethod make-invalid-build-reason ((x build-registry-conflict) &key &allow-other-keys)
  (format nil "it conflicts with same-named builds at~%~{~S~&~}"
          (brc-pathnames x)))

(defmethod make-invalid-build-reason ((x invalid-build-file) &key &allow-other-keys)
  (invalid-build-reason x))

(defun pathname-build (pathname)
  (loop :with truename = (truename pathname)
    :for build :being :the :hash-values :of *builds* :do
    (flet ((same-truename-p (pathname)
             (equal (truename pathname) truename)))
      (etypecase build
        (pathname
         (when (same-truename-p (grain-pathname build))
           (return build)))
        (build-registry-conflict
         (when (find-if #'same-truename-p (brc-pathnames build))
           (error 'build-conflict
                  :fullname (fullname build)
                  :pathname pathname
                  :conflicts (remove-if #'same-truename-p (brc-pathnames build)))))))))

(defun compute-source-registry (&optional parameter)
  (let ((*default-pathname-defaults* *xcvb-lisp-directory*))
    (asdf::flatten-source-registry parameter)))

(defun initialize-source-registry (&optional parameter)
  (clrhash *builds*)
  (let ((source-registry (compute-source-registry parameter)))
    (setf *source-registry* (list source-registry))
    *source-registry*))

(defun ensure-source-registry ()
  (unless *source-registry*
    (error "You should have already initialized the source registry by now!")))

;;; Now for actually searching the source registry!

(defun verify-path-element (element)
  (let* ((absolute-path (ensure-absolute-pathname element)))
    (cond
      ((ignore-errors (truename absolute-path))
       absolute-path)
      (t
       (log-format 7 "Discarding invalid path element ~S~%" element)
       nil))))

(defun finalize-source-registry ()
  (setf *source-registry*
        (list
         (loop :for (path . flags) :in (car *source-registry*)
           :for v = (verify-path-element path)
           :when v :collect (cons v flags)))))

(defvar +all-builds-path+
  (make-pathname :directory '(:relative :wild-inferiors)
                 :name "build"
                 :type "xcvb"
                 :version :newest))

(defun pathname-newest-version-p (x)
  (or
   (member (pathname-version x) '(nil :newest :unspecific))
   (and (integerp (pathname-version x))
        (equal (truename x) (truename (make-pathname :version :newest :defaults x))))))

(defun pathname-is-build.xcvb-p (x)
  (and (equal (pathname-name x) "build")
       (equal (pathname-type x) "xcvb")
       #+genera (pathname-newest-version-p x)))


(defun find-build-files-under (root)
  (destructuring-bind (pathname &key recurse exclude) root
    (if (not recurse)
        (let ((path (probe-file (merge-pathnames "build.xcvb" pathname))))
          (when path (list path)))
        ;; This is what we want, but too slow with SBCL.
        ;; It took 5.8 seconds on my machine, whereas what's below takes .56 seconds
        ;; I haven't timed it with other implementations
        ;; -- they might or might not need the same hack.
        ;; TODO: profile it and fix SBCL.
        #-sbcl
        (loop
          :for file :in (ignore-errors
                          (directory (merge-pathnames +all-builds-path+ pathname)
                                     #+sbcl #+sbcl :resolve-symlinks nil
                                     #+clisp #+clisp :circle t))
          :unless (loop :for x :in exclude
                    :thereis (find x (pathname-directory file) :test #'equal))
          :collect file)
        #+sbcl
        (run-program/read-output-lines
         `("find" "-H" ,(escape-shell-token (namestring pathname))
           "(" "(" "-false" ,@(loop :for x :in exclude :append `("-o" "-name" ,x)) ")" "-prune"
           "-o" "-name" "build.xcvb" ")" "-type" "f" "-print")))))

(defun map-build-files-under (root fn)
  "Call FN for all BUILD files under ROOT"
  (let* ((builds (find-build-files-under root))
         ;; depth first traversal
         (builds (sort (mapcar #'truename builds) #'<
                       :key (compose #'length #'pathname-directory))))
    (map () fn builds)))

(defun search-source-registry ()
  (finalize-source-registry)
  (dolist (root (car *source-registry*))
    (map-build-files-under root #'(lambda (x) (register-build-file x root)))
    (confirm-builds-under root)))

(defun ensure-source-registry-searched ()
  (unless *source-registry-searched-p*
    (search-source-registry)))

;;;; Registering a build

(defun supersedes-asdf-name (x)
  `(:supersedes-asdf ,(coerce-asdf-system-name x)))

(defun registered-build (name &key ensure-build)
  (let ((build (gethash name *builds*)))
    (when ensure-build
      (unless (build-module-grain-p build)
        (error "Could not find a build with requested fullname ~A. Try xcvb show-source-registry"
               name)))
    build))

(defun (setf registered-build) (build name &key ensure-build)
  (when ensure-build
    (unless (build-module-grain-p build)
      (error "Cannot register build ~S to non-build grain ~S" name build)))
  (setf (gethash name *builds*) build))

(defun fullname-from-truename (truename)
  (gethash truename *truename-build-fullnames*))

(defun (setf fullname-from-truename) (fullname truename)
  (setf (gethash truename *truename-build-fullnames*) fullname))

(defun register-build-file (build root)
  "Registers build file build.xcvb (given as truename)
as having found under root path ROOT (as pathname),
for each of its registered names."
  ;;(format *error-output* "~&Found build file ~S in ~S~%" build root)
  (let* ((build-module-grain
          (make-grain-from-file build :build-p t))
         (fullname (when build-module-grain (fullname build-module-grain))))
    (flet ((register-entry (entry)
             (setf (gethash build *truename-build-fullnames*) (fullname entry))
             (register-build-named fullname entry root)))
      (cond
        ((null fullname)
         (log-format 5 "Failed to parse build file at ~S" build))
        ((slot-boundp build-module-grain 'root)
         (log-format 7 "Already visited build at ~S" build))
        ((typep (grain-parent build-module-grain) 'invalid-build-registry-entry)
         (register-entry
          (make-invalid-ancestor-build-file
           :fullname fullname :pathname build :root root
           :ancestor (grain-parent build-module-grain))))
        (t
         (setf (bre-root build-module-grain) root)
         (register-entry build-module-grain))))
    (values)))

(defun confirm-builds-under (root)
  (loop
    :with builds-under-root = (loop :for b :being :the :hash-values :of *builds*
                                :when (and (build-module-grain-p b) (equal (bre-root b) root))
                                :collect b)
    :for b :in (sort builds-under-root #'< :key (compose #'length #'fullname))
    :for p = (grain-parent b) :do
    (if (or (null p) (eq p (registered-build (fullname p))))
        ;; The parent has already been visited and not been invalidated,
        ;; so the current build is valid, and we register its secondary names.
        (dolist (name (append (mapcar #'canonicalize-fullname (nicknames b))
                              (mapcar #'supersedes-asdf-name (supersedes-asdf b))))
          (register-build-named name b root))
        (let* ((fullname (fullname b))
               (invalid
                (make-invalid-ancestor-build-file
                 :fullname fullname :pathname (grain-pathname b) :ancestor p :root root)))
          (setf (registered-build fullname) invalid)))))

(defun merge-build (previous-build new-build name root)
   ;; Detect ambiguities.
  ;; If the name has already been registered, then
  ;; * if the previous entry is from a previous root, it has precedence
  ;; * else if the previous entry is from same root and is in an ancestor directory,
  ;;   it has precedence
  ;; * otherwise, it's a conflict, and the name shall be marked as conflicted and
  ;;   an error be printed if/when it is used.
  ;; Note: to do that in a more functional way, have some mechanism
  ;; that applies a modify-function to a gethash value, allowing (values NIL NIL) to specify remhash.
  (check-type previous-build (or null invalid-build-registry-entry build-module-grain))
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

(defun register-build-named (name build-module-grain root)
  "Register under NAME pathname BUILD found in user-specified ROOT."
  (funcallf (registered-build name) #'merge-build build-module-grain name root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Show Search Path ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-source-registry ()
  "Show registered builds"
  (format t "~&Registered search paths:~{~% ~S~}~%" (car *source-registry*))
  (format t "~%Builds found in the search paths:~%")
  (flet ((entry-string (x)
           (destructuring-bind (fullname . entry) x
             (etypecase entry
               (build-module-grain
                (if (and (list-of-length-p 2 fullname) (eq (first fullname) :supersedes-asdf))
                    (format nil " (:ASDF ~S) superseded by (:BUILD ~S)~%"
                            (second fullname) (fullname entry))
                    (format nil " (:BUILD ~S) in ~S~%"
                            fullname (namestring (grain-pathname entry)))))
               (invalid-build-file
                (format nil " INVALID for ~S at ~S (ancestor conflict)~%"
                        fullname (grain-pathname entry)))
               (build-registry-conflict
                (format nil " CONFLICT for ~S between ~S~%"
                        fullname (mapcar 'namestring (brc-pathnames entry))))))))
    (map () #'princ (sort (mapcar #'entry-string (hash-table->alist *builds*)) #'string<))))

(defun show-source-registry-command (&key source-registry)
  (handle-global-options :source-registry source-registry)
  (show-source-registry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Find Module ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-module (&key source-registry name short)
  "find modules of given full name"
  (handle-global-options :source-registry source-registry)
  (let ((all-good t))
    (dolist (fullname name)
      (let ((grain (resolve-absolute-module-name fullname)))
        (cond
          (grain
           (if short
             (format t "~A~%" (namestring (grain-pathname grain)))
             (format t "Found ~S at ~S~%" (fullname grain) (namestring (grain-pathname grain)))))
          (t
           (format *error-output* "Could not find ~S. Check your paths with xcvb ssr.~%" fullname)
           (setf all-good nil)))))
    (exit (if all-good 0 1))))

(defparameter +find-module-option-spec+
  `((("name" #\n) :type string :optional nil :list t :documentation "name to search for")
    (("short" #\s) :type boolean :optional t :documentation "short output")
    ,@+source-registry-option-spec+))
