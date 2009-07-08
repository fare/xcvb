;;;;; Escaping strings
#+xcvb (module (:depends-on ("utilities")))

(in-package :xcvb)

(defun escape-string-for-Makefile (string &optional out)
  "Takes a string and excapes all the characters that need to be put into a makefile.
The only such character right now is $.
Raises an error if the string contains a newline."
  (with-output (out)
    (loop :for c :across string :do
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
    (loop :for c :across string :do
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
	   (find c "%.,-+_:/"))))

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
    (loop :for tok :in tokens
	  :for () = () :then (when tok (write-char #\space out))
	  :do (cond
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

(defun shell-tokens-to-string (tokens &optional out)
  (with-output (out)
    (loop :for tok :in tokens
	  :for () = () :then (when tok (write-char #\space out))
	  :do (cond
	       ((null tok))
	       ((stringp tok)
		(escape-shell-token tok out))
	       (t
		(error "Invalid token ~S" tok))))))

(defun normalize-name-for-makefile (x)
  (map 'base-string (lambda (c) (if (char-needs-shell-quoting-p c) #\_ c)) x))
