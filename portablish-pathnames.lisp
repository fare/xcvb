#+xcvb (module (:depends-on ("utilities")))

(in-package :xcvb)

;;; Handling filenames,
;;; exchanging between implementation-dependent pathname objects (made with make-pathname)
;;; and reasonably portable string representation.

(defvar +root-path+ (make-pathname :directory '(:absolute))
  "pathname for the file hierarchy root")

(defvar +up-path+ (make-pathname :directory '(:relative :up))
  "logical parent path")

(defun pathname-directory-pathname (pathname)
  (make-pathname :type nil :name nil :defaults pathname))

(defun pathname-parent (pathname)
  "Takes a pathname and returns the pathname of the parent directory
of the directory of the given pathname"
  (cond
    ;; no pathname, no parent
    ((null pathname)
     nil)
    ;; / is its own parent.
    ((equal (pathname-directory pathname) '(:absolute))
     +root-path+)
    (t
     (merge-pathnames +up-path+
		      (pathname-directory-pathname pathname)))))

(defun top-level-name (name)
  "This function takes a name, and returns everything up to the first \"/\" in the name"
  (subseq name 0 (position #\/ (namestring name))))

(defun directory-name-p (name)
  (and (stringp name)
       (eql #\/ (last-char name))))

(defun portable-pathname-string-component-char-p (c)
  ;; Assumes ASCII
  (and (or (char<= #\a c #\z)
	   (char<= #\A c #\Z)
	   (char<= #\0 c #\9)
	   (find c ".,-+_"))
       t))

(defun portable-pathname-string-component-p (x)
  (and (stringp x)
       (every #'portable-pathname-string-component-char-p x)
       (not (member x '("" "." "..") :test 'equal))))

(defun portable-pathname-type-component-p (x)
  (and (portable-pathname-string-component-p x)
       (not (find #\. x))))

(defun portable-pathname-directory-output
    (directory &key out (allow-absolute t) (allow-relative t))
  "DIRECTORY being the directory component of a pathname,
output to OUT a portable representation of it,
erroring out if some source of non-portability is found"
  (with-output (out)
    (labels ((d2s (x)
	       (dolist (c x)
		 (unless (portable-pathname-string-component-p c)
		   (error "Non-portable component ~S in directory ~S" c directory))
		 (write-string c out)
		 (write-char #\/ out))))
      (cond
        ((null directory) ;; accept the previous representation, not the latter
         (setf directory '(:relative)))
        ((equal directory '(:relative))
         (error "Invalid directory (:relative)")))
      (cond
	((member directory '(:wild :unspecific nil))
	 (error "Cannot portably stringify directory ~S" directory))
	((stringp directory)
	 (error "xcvb doesn't support non-hierarchical filesystems"))
	((and (consp directory) (eq (car directory) :absolute))
	 (unless allow-absolute
	   (error "absolute directory ~S not allowed" directory))
	 (write-char #\/ out)
	 (d2s (cdr directory)))
	((and (consp directory) (eq (car directory) :relative))
	 (unless allow-relative
	   (error "relative directory ~S not allowed" directory))
	 (if (eq :up (third directory))
	     (d2s (cdddr directory))
	     (d2s (cdr directory))))
	(t
	 (error "Invalid directory ~S" directory))))))

(defun portable-pathname-name-output (name &key out)
  (with-output (out)
    (unless (portable-pathname-string-component-p name)
      (error "Non-portable pathname name ~S" name))
    (write-string name out)))

(defun portable-pathname-type-output (type &key out)
  (with-output (out)
    (unless (portable-pathname-type-component-p type)
      (error "Non-portable pathname type ~S" type))
    (write-string type out)))

(defun portable-pathname-output (pathname &key out (allow-absolute t) (allow-relative t))
  (with-output (out)
    (let* ((p (pathname pathname))
	   (directory (pathname-directory p))
	   (name (pathname-name p))
	   (type (pathname-type p))
	   (version (pathname-version p)))
      (unless (member version '(nil :unspecific :newest))
	(error "Non-portable pathname version ~S in ~S" version pathname))
      (portable-pathname-directory-output
       directory
       :out out :allow-absolute allow-absolute :allow-relative allow-relative)
      (when name
	(portable-pathname-name-output name :out out)
	(cond
	  ((stringp type)
	   (write-char #\. out)
	   (portable-pathname-type-output type :out out))
	  ((member type '(nil :unspecific :newest))
	   (when (find #\. name)
	     (error "Non-portable pathname ~S with a dot in name but no type" pathname)))
	  (t
	   (error "Non-portable pathname type ~S" type)))))))

(defun portable-namestring (pathname)
  (portable-pathname-output pathname))

(defun portable-pathname-from-string (string &key
					     (start 0) (end (length string))
					     (allow-absolute t) (allow-relative t))
  (let (r name type)
    (unless (< start end)
      (error "cannot parse beyond the end of string ~S (start: ~S, end: ~S)" string start end))
    (cond
      ((eql (char string start) #\/)
       (unless allow-absolute
	 (error "unexpected absolute pathname ~S (start: ~S, end: ~S)" string start end))
       (setf r (list :absolute)) (incf start))
      (t
       (unless allow-relative
	 (error "unexpected relative pathname ~S (start: ~S, end: ~S)" string start end))
       (setf r (list :relative))))
    (loop :for p = (and (< start end) (position #\/ string :start start :end end))
	  :while p :do
	  (let ((dir (subseq string start p)))
;; 	    (unless (portable-pathname-string-component-p dir)
;; 	      (error "non-portable pathname directory ~S" dir))
	    (push dir r)
	    (setf start (1+ p))))
    (when (< start end)
      (let ((ldp (position #\. string :start start :end end :from-end t)))
	(setf name (subseq string start (or ldp end))
	      type (and ldp (subseq string (1+ ldp) end)))
	(unless (portable-pathname-string-component-p name)
	  (error "non-portable pathname name ~S" name))
	(when type
	  (unless (portable-pathname-type-component-p type)
	    (error "non-portable pathname type ~S" type)))))
    (make-pathname :directory (nreverse r) :name name :type type)))

(defun subpathname (path string)
  (merge-pathnames
   (portable-pathname-from-string string :allow-absolute nil)
   path))

(defun pathname-absolute-p (path)
  (let ((directory (pathname-directory path)))
    (and (consp directory) (eq (car directory) :absolute))))

(defun absolute-pathname-p (path)
  (and (pathnamep path)
       (pathname-absolute-p path)))

(defun portable-namestring-absolute-p (namestring)
  (eql (first-char namestring) #\/))

(defun portable-pathname-absolute-p (name)
  (etypecase name
    (pathname (pathname-absolute-p name))
    (string (portable-namestring-absolute-p name))))

(defun absolute-portable-namestring-p (namestring)
  (and (portable-namestring-p namestring)
       (portable-namestring-absolute-p namestring)))

(defun portable-namestring-p (x)
  (and (stringp x)
       (ignore-errors (portable-pathname-from-string x))
       t))

(defun ensure-absolute-pathname (x)
  (let ((path (pathname x)))
    (cond
      ((absolute-pathname-p path)
       path)
      ((absolute-pathname-p *default-pathname-defaults*)
       (merge-pathnames path))
      (t
       (merge-pathnames path (truename *default-pathname-defaults*))))))

(defun portable-namestring-prefix<= (x y)
  (and (string-prefix<= x y)
       (or (= (length x) (length y))
           (eql #\/ (char y (length x))))))

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
