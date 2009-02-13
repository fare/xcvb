;;; Handle the Search Path for XCVB modules.

(in-package :xcvb)

(defvar *search-path* '()
  "Path to search for XCVB modules")

(defvar *search-path-searched-p* nil
  "Did we search the search path?")

(defun default-search-path ()
  (list
   *default-pathname-defaults*
   (user-homedir-pathname)
   #p"/usr/share/common-lisp/modules/"))

(defun verify-path-element (element)
  (let ((truepath (ignore-errors (truename (ensure-pathname-is-directory element)))))
    (or truepath
	(progn
	  (format *error-output* "~&Discarding invalid path element ~S~%" element)
	  nil))))

(defun expand-search-path-string (string &optional (previous-path *search-path*))
  (cond
    ((or (null string) (equal string ""))
     previous-path)
    ((stringp string)
     (loop
	 with path = ()
	 with start = 0
	 with end = (length string)
	 for i = (or (position #\: string :start start) end) do
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
       ((eql (char x (1- (length x))) #\/)
	(pathname x))
       (t
	(pathname (concatenate 'string x "/")))))
    (pathname
     (if (or (pathname-name x) (pathname-type x) (pathname-version x))
	 (error "pathname ~S isn't a directory" x)
	 x))))

(defun initialize-search-path ()
  (setf *search-path* (default-search-path))
  (set-search-path! (cl-launch:getenv "XCVB_PATH")))

(defun finalize-search-path ()
  (setf *search-path*
	(loop for elt in *search-path*
	      for v = (verify-path-element elt)
	      when v collect v)))

(defun pathname-newest-version-p (x)
  (equal (truename x) (truename (make-pathname :version :newest :defaults x))))

(defun pathname-is-BUILD.lisp-p (x)
  (and (equal (pathname-name x) "BUILD")
       (equal (pathname-type x) "lisp")
       #+genera (pathname-newest-version-p x)))

#|
(defun map-build-files-under (root fn)
  (walk-directory
   root
   fn
   :test #'pathname-is-BUILD.lisp-p
   :directories :depth-first
   :if-does-not-exist :ignore))
|#

(defun register-BUILD-file (build root)
  ;;TODO: do something about it.
  ;;i.e. analyze the BUILD file, its :name and :nicknames.
  ;; if the name has already been registered, then
  ;; * if the previous entry is from a previous root, it has precedence
  ;; * if the previous entry is from same root and is in an ancestor directory, it has precedence
  ;; * otherwise, it's a conflict, and the name shall be marked as conflicted and an error be printed
  ;;  if/when it is used.
  (format *error-output* "~&Found build file ~S in ~S~%" build root))

(defun search-search-path ()
  (setf *search-path-searched-p* t)
  (finalize-search-path)
  (dolist (root *search-path*)
    (map-build-files-under root #'(lambda (x) (register-BUILD-file x root)))))

(defun ensure-search-path-searched ()
  (unless *search-path-searched-p*
    (search-search-path)))
