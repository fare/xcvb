(in-package :xcvb)

;;; Conditions

(define-condition no-build-file-found (simple-error)
  ;; This condition is signaled by the find-build-file function if no BUILD.lisp file can be found
  ())

(define-condition dependency-cycle (simple-error)
  ;; This condition is signaled if the dependency graph has any cycles in it.
  ())

(define-condition syntax-error (simple-error)
  ;; Condition is signaled if there is some syntax error in some user-specified data
  ())

(defun simply-error (simple-error control &rest args)
  (error (or simple-error 'simple-error)
         :format-control control :format-arguments args))


;;; String functions

(defun strcat (&rest strings)
  "String concatenation function"
  (apply 'concatenate 'string strings))


;;; Filename handling

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
     (make-pathname :directory '(:absolute)))
    (t
     (merge-pathnames (make-pathname :directory '(:relative :up))
		      (make-pathname :name nil :type nil :defaults pathname) nil))))

(defun top-level-name (name)
  "This function takes a name, and returns everything up to the first \"/\" in the name"
  (subseq name 0 (position #\/ (namestring name))))

(defun make-fullname-absolute (module) ;; TODO: WHY DO WE NEED THIS?
  "This function prepends a \"/\" to the beginning of the module's fullname,
if there isn't one there already"
  (if (eql 0 (position #\/ (fullname module)))
    (fullname module)
    (setf (fullname module) (strcat "/" (fullname module)))))

(defun coerce-asdf-system-name (name)
  "This function take the name of an asdf-system, and
converts it to a string representation that can universally be used to refer to that system.
Modeled after the asdf function coerce-name"
  (string-downcase
   (typecase name
     (asdf:component (asdf:component-name name))
     (symbol (symbol-name name))
     (string name)
     (t (simply-error 'syntax-error "~@<invalid asdf system designator ~A~@:>" name)))))

(defun portable-pathname-string-component-char-p (c)
  ;; Assumes ASCII
  (and (or (char<= #\a c #\z)
	   (char<= #\A c #\Z)
	   (char<= #\0 c #\9)
	   (find c ".,-_"))
       t))

(defun portable-pathname-string-component-p (x)
  (and (stringp x)
       (not (zerop (length x)))
       (every #'portable-pathname-string-component-char-p x)))

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
	 (d2s (cdr directory)))
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
	 (error "unexpected absolute pathname ~S (start: ~S, end: ~S)" string start end))
       (setf r (list :relative))))
    (loop for p = (and (< start end) (position #\/ string :start start :end end))
	  while p do
	  (let ((dir (subseq string start p)))
	    (unless (portable-pathname-string-component-p dir)
	      (error "non-portable pathname directory ~S" dir))
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


;;; Escaping strings

(defun escape-string-for-Makefile (string &optional out)
  "Takes a string and excapes all the characters that need to be put into a makefile.
The only such character right now is $.
Raises an error if the string contains a newline."
  (with-output (out)
    (loop for c across string do
      (case c
        ;;TODO - instead of erroring, should this insert a "\" to escape the newline?
        (#\newline (error "Makefile line cannot contain a newline"))
        (#\$ (write-string "$$" out))
        (otherwise (write-char c out))))))

(defparameter *need-shell-double-quote-escape* "\"`$\\"
  "characters that need be escaped in a shell double-quote context")

(defun char-needs-shell-double-quote-escape-p (c)
  (find c *need-shell-double-quote-escape*))

(defun escape-string-for-shell-double-quote (string &optional out quotes)
  "Takes a string and escapes all the characters that need to be to be run in the shell."
  (with-output (out)
    (when quotes
      (write-char #\" out))
    (loop for c across string do
	  (when (char-needs-shell-double-quote-escape-p c)
	    (write-char #\\ out))
	  (write-char c out))
    (when quotes
      (write-char #\" out))))

(defun char-needs-shell-quoting-p (c)
  "characters may require some shell quoting to be included as a shell argument"
  ;; Assumes ASCII
  (not (or (char<= #\a c #\z)
	   (char<= #\A c #\Z)
	   (char<= #\0 c #\9)
	   (find c "%.,-_:/"))))

(defun escape-shell-token (string &optional out)
  "Takes a string and if needed, includes the string in double quotes to use as shell argument"
  (cond
    ((some #'char-needs-shell-quoting-p string)
     (escape-string-for-shell-double-quote string out t))
    ((null out)
     string)
    (t
     (with-output (out)
       (write-string string out)))))

(defun escape-shell-token-for-Makefile (string &optional out)
  "Takes a string and escapes it first for the shell, then for a makefile"
  (escape-string-for-Makefile (escape-shell-token string) out))

(defun shell-tokens-to-Makefile (tokens &optional out)
  "Transforms an list of tokens into something to print on a Makefile line
for the Makefile-invoked shell to execute the process specified by these tokens.
List elements can be either strings that will be escaped
or escapes of the form (:makefile string) that won't be escaped."
  (with-output (out)
    (loop for tok in tokens
	  for () = () then (when tok (write-char #\space out))
	  do (cond
	       ((null tok))
	       ((stringp tok)
		(escape-shell-token-for-Makefile tok out))
	       ((and (consp tok) (eq :makefile (car tok)))
		(dolist (s (cdr tok))
		  (write-string s out)))
	       ((consp tok)
		(shell-tokens-to-Makefile tok out))
	       (t
		(error "Invalid token member ~S" tok))))))
