;;;;; Escaping strings
#+xcvb (module (:depends-on ("utilities")))

(in-package :xcvb)

;;; Escaping for a Makefile

(defun escape-string-hashes (string &optional out)
  "Escapes only the hashes in a string.
This is required on the right side of a variable assignment in a Makefile. Go figure."
  (with-output (out)
    (loop :for c :across string :do
      (case c
        (#\# (write-string "\\#" out))
        (otherwise (write-char c out))))))

(defun escape-string-for-Makefile (string &optional out)
  "Takes a string and excapes all the characters that need to be put into a makefile.
The only such character right now is $.
Raises an error if the string contains a newline."
  (with-output (out)
    (loop :for c :across string :do
      (case c
        ;; Q: Instead of erroring, should this insert a "\\n" or something to escape the newline???
        ;; Q: Do we need to handle #\\ specially? Apparently, no. Make quoting is misdesigned.
        (#\newline (error "Makefile line cannot contain a newline"))
        (#\$ (write-string "$$" out))
        (otherwise (write-char c out))))))

(defun escape-sh-token-for-Makefile (string &optional out)
  "Takes a string and escapes it first for the shell, then for a makefile"
  (escape-string-for-Makefile (escape-sh-token string) out))

(defun shell-tokens-to-Makefile (tokens &optional out)
  "Transforms an list of tokens into something to print on a Makefile line
for the Makefile-invoked shell to execute the process specified by these tokens.
List elements can be either strings that will be escaped
or escapes of the form (:makefile string) that won't be escaped."
  (with-output (out)
    (loop :for (tok . rest) :on tokens :do
      (cond
        ((null tok))
        ((stringp tok)
         (escape-sh-token-for-Makefile tok out))
        ((and (consp tok) (eq :makefile (car tok)))
         (dolist (s (cdr tok))
           (write-string s out)))
        ((consp tok)
         (shell-tokens-to-Makefile tok out))
        (t
         (error "Invalid token member ~S" tok)))
      (when rest (write-char #\space out)))))

(defun shell-tokens-to-string (tokens &optional out)
  (with-output (out)
    (loop :for (tok . rest) :on tokens :do
      (cond
        ((null tok))
        ((stringp tok) (escape-sh-token tok out))
        (t (error "Invalid token ~S" tok)))
      (when rest (write-char #\space out)))))

(defun normalize-name-for-makefile (x)
  (map 'base-string (lambda (c) (if (find c "$`\\\" ()[]{};	") #\_ c)) x))
