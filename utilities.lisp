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


;;; Module forms

(defun read-first-file-form (filepath)
  "Reads the first form from the top of a file"
  (with-open-file (in filepath) (read in)))

(defun module-form-p (form)
  "Returns whether or not the given form is an xcvb:module form"
  (eql (first form) 'xcvb:module))
#|  (destructuring-bind (module-decl &rest rest) form
    (declare (ignore rest))
    (eql module-decl 'xcvb:module)))|#


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


;;; Escaping strings

(defun escape-shell-command-for-Makefile (string &optional out)
  "Takes a string and excapes all the characters that need to be put into a makefile.
The only such character right now is $.  Raises an error if the string contains a newline."
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

(defun escape-string-for-shell (string &optional out)
  "Takes a string and if needed, includes the string in double quotes to use as shell argument"
  (cond
    ((some #'char-needs-shell-quoting-p string)
     (escape-string-for-shell-double-quote string out t))
    (out
     (with-output (out)
       (write-string string out)))
    (t
     string)))

(defun escape-string-for-Makefile (string &optional out)
  "Takes a string and escapes it first for the shell, then for a makefile"
  (escape-shell-command-for-Makefile (escape-string-for-shell string) out))

(defun arglist-to-Makefile (arglist &optional out)
  "Transforms an arglist into something to print on a Makefile line
to execute the specified process.
List elements can be either strings that will be escaped
or escapes of the form (:makefile string) that won't be escaped."
  (with-output (out)
    (loop for arg in arglist
	  for () = () then (when arg (write-char #\space out))
	  do (cond
	       ((null arg))
	       ((stringp arg)
		(escape-string-for-Makefile arg out))
	       ((and (consp arg) (eq :makefile (car arg)))
		(dolist (s (cdr arg))
		  (write-string s out)))
	       ((consp arg)
		(arglist-to-Makefile arg out))
	       (t
		(error "Invalid arglist member ~S" arg))))))
