;;; Handle the Search Path for XCVB modules.
#+xcvb (module (:depends-on ("specials" "utilities")))
(in-package :xcvb)

(defun default-search-path ()
  (list
   *default-pathname-defaults*
   (subpathname (user-homedir-pathname) ".local/share/common-lisp/source/")
   *xcvb-lisp-directory*
   #p"/usr/local/share/common-lisp/source/"
   #p"/usr/share/common-lisp/source/"))

(defun verify-path-element (element)
  (let* ((absolute-path (ensure-absolute-pathname (ensure-pathname-is-directory element))))
    (cond
      ((ignore-errors (truename absolute-path))
       absolute-path)
      (t
       (format *error-output* "~&Discarding invalid path element ~S~%"
               (namestring element))
       nil))))

(defun expand-search-path-string (string &optional (previous-path *search-path*))
  (cond
    ((or (null string) (equal string ""))
     previous-path)
    ((stringp string)
     (loop
	 :with path = ()
	 :with start = 0
	 :with end = (length string)
	 :for i = (or (position #\: string :start start) end) :do
	 (let ((s (subseq string start i)))
	   (cond
	     ((equal "" s) ; empty element
	      nil)
	     ((equal "!" s) ; previous path
	      (setf path (append (reverse previous-path) path)))
	     (t
	      (push s path))))
	 (setf start (1+ i))
	 (when (>= start end) (return (nreverse path)))))))

(defun set-search-path! (string)
  (setf *search-path* (expand-search-path-string string)))

(defun initialize-search-path ()
  (setf *search-path-searched-p* nil)
  (setf *search-path* (default-search-path))
  (set-search-path! (cl-launch:getenv "XCVB_PATH")))

(defun finalize-search-path ()
  (setf *search-path*
	(loop :for elt :in *search-path*
	      :for v = (verify-path-element elt)
	      :when v :collect v)))

(defun pathname-newest-version-p (x)
  (or
   (member (pathname-version x) '(nil :newest :unspecific))
   (and (integerp (pathname-version x))
        (equal (truename x) (truename (make-pathname :version :newest :defaults x))))))

(defun pathname-is-build.xcvb-p (x)
  (and (equal (pathname-name x) "build")
       (equal (pathname-type x) "xcvb")
       #+genera (pathname-newest-version-p x)))

(defvar *archive-directory-names* '("_darcs" ".svn")
  "names of archive directories inside which we should not look for BUILD files")

(defun in-archive-directory-p (x)
  (loop :for d :in (pathname-directory x)
        :thereis (member d *archive-directory-names* :test #'equal)))

(defvar +all-builds-path+
  (make-pathname :directory '(:relative :wild-inferiors)
                 :name "build"
                 :type "xcvb"
                 :version :newest))

(defun underscore-for-non-alphanum-chars (x)
  (map 'base-string
       (lambda (c) (if (or (char<= #\a c #\z) (char<= #\A c #\Z) (char<= #\0 c #\9)) c #\_))
       x))

(defun find-build-files-under (root)
  ;;; This is what we want, but too slow with SBCL.
  ;; It took 5.8 seconds on my machine, whereas what's below takes .56 seconds
  ;; I haven't timed it with other implementations -- they might or might not need the same hack.
  ;; TODO: profile it and fix SBCL.
  #-sbcl
  (directory (merge-pathnames +all-builds-path+ root)
                    #+sbcl #+sbcl :resolve-symlinks nil
                    #+clisp #+clisp :circle t)
  #+sbcl
  (run-program/read-output-lines
   (list "find" "-H" (escape-shell-token (namestring root)) "-type" "f" "-name" "build.xcvb")))

(defun map-build-files-under (root fn)
  "Call FN for all BUILD files under ROOT"
  (let* ((builds (find-build-files-under root))
         (builds (remove-if #'in-archive-directory-p builds))
         #+(or) ;; uncomment it for depth first traversal
         (builds (sort builds #'<
                       :key #'(lambda (p) (length (pathname-directory p))))))
    (map () fn builds)))

(defun search-search-path ()
  (setf *search-path-searched-p* t)
  (finalize-search-path)
  (dolist (root *search-path*)
    (map-build-files-under root #'(lambda (x) (register-build-file x root)))
    (register-build-nicknames-under root)))

(defun ensure-search-path-searched ()
  (unless *search-path-searched-p*
    (search-search-path)))

;;;; Registering a build

(defun supersedes-asdf-name (x)
  `(:supersedes-asdf ,(coerce-asdf-system-name x)))

(defun registered-build (name &key ensure-build)
  (let ((build (gethash name *builds*)))
    (when ensure-build
      (unless (build-grain-p build)
        (error "Could not find a build with requested fullname ~A. Try xcvb show-search-path"
               name)))
    build))

(defun (setf registered-build) (build name &key ensure-build)
  (when ensure-build
    (unless (build-grain-p build)
      (error "Cannot register build ~S to non-build grain ~S" name build)))
  (setf (gethash name *builds*) build))

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
              (loop :for b :being :the :hash-values :of *builds*
                :when (and (build-grain-p b) (equal (bre-root b) root)) :collect b)))
    (dolist (name (append (mapcar #'canonicalize-fullname (nicknames b))
                          (mapcar #'supersedes-asdf-name (supersedes-asdf b))))
      (register-build-named name b root))))

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
  (funcallf (registered-build name) #'merge-build build-grain name root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Show Search Path ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-search-path ()
  "Show registered builds"
  (format t "~&Registered search paths:~{~% ~S~}~%" *search-path*)
  (format t "~%Builds found in the search paths:~%")
  (flet ((entry-string (x)
           (destructuring-bind (fullname . entry) x
             (etypecase entry
               (build-grain
                (if (and (list-of-length-p 2 fullname) (eq (first fullname) :supersedes-asdf))
                    (format nil " (:ASDF ~S) superseded by (:BUILD ~S)~%"
                            (second fullname) (fullname entry))
                    (format nil " (:BUILD ~S) in ~S~%"
                            fullname (namestring (grain-pathname entry)))))
               (build-registry-conflict
                (format nil " CONFLICT for ~S between ~S~%"
                        fullname (mapcar 'namestring (brc-pathnames entry))))))))
    (map () #'princ (sort (mapcar #'entry-string (hash-table->alist *builds*)) #'string<))))

(defparameter +show-search-path-option-spec+
  '((("xcvb-path" #\x) :type string :optional t :documentation "override your XCVB_PATH")))

(defun show-search-path-command (&key xcvb-path)
  (handle-global-options :xcvb-path xcvb-path)
  (show-search-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Find Module ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-module (&key xcvb-path name short)
  "find modules of given full name"
  (handle-global-options :xcvb-path xcvb-path)
  (let ((all-good t))
    (dolist (fullname name)
      (let ((grain (resolve-absolute-module-name fullname)))
        (cond
          (grain
           (if short
             (format t "~A~%" (namestring (grain-pathname grain)))
             (format t "Found ~S at ~S~%" (fullname grain) (namestring (grain-pathname grain)))))
          (t
           (format *error-output* "Could not find ~S. Check your paths with xcvb ssp.~%" fullname)
           (setf all-good nil)))))
    (exit (if all-good 0 1))))

(defparameter +find-module-option-spec+
  (append
   '((("name" #\n) :type string :optional nil :list t :documentation "name to search for")
     (("short" #\s) :type boolean :optional t :documentation "short output"))
   +show-search-path-option-spec+))
