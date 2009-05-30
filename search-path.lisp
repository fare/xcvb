;;; Handle the Search Path for XCVB modules.

(in-package :xcvb)

(defvar *search-path-searched-p* nil
  "Did we search the search path?")

(defun default-search-path ()
  (list
   *default-pathname-defaults*
   (subpathname (user-homedir-pathname) "lisp/")
   *xcvb-lisp-directory*
   #p"/usr/local/share/common-lisp/modules/"
   #p"/usr/share/common-lisp/modules/"))

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

(defun ensure-pathname-is-directory (x)
  (etypecase x
    (string
     (cond
       ((equal x "")
	(error "empty namestring"))
       ((eql (last-char x) #\/)
	(pathname x))
       (t
	(pathname (strcat x "/")))))
    (pathname
     (if (or (pathname-name x)
             (pathname-type x)
             (not (member (pathname-version x) '(nil :unspecific :newest))))
       (error "pathname ~S isn't a directory" x)
       x))))

(defun initialize-search-path ()
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

(defun pathname-is-BUILD.lisp-p (x)
  (and (equal (pathname-name x) "BUILD")
       (equal (pathname-type x) "lisp")
       #+genera (pathname-newest-version-p x)))

(defvar *archive-directory-names* '("_darcs" ".svn")
  "names of archive directories inside which we should not look for BUILD files")

(defun in-archive-directory-p (x)
  (loop :for d :in (pathname-directory x)
        :thereis (member d *archive-directory-names* :test #'equal)))

(defvar +all-builds-path+
  (make-pathname :directory '(:relative :wild-inferiors)
                 :name "BUILD"
                 :type "lisp"
                 :version :newest))

(defun map-build-files-under (root fn)
  "Call FN for all BUILD files under ROOT"
  (let* ((builds (directory (merge-pathnames +all-builds-path+ root)
                            #+sbcl #+sbcl :resolve-symlinks nil))
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
