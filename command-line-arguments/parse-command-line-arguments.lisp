;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Confidential and proprietary information of ITA Software, Inc.   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2003-2009 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original author: Francois-Rene Rideau                            ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+xcvb (module (:depends-on ("get-command-line-arguments")))

(in-package :command-line-arguments)


(defvar *command-line-arguments* nil
  "a list of strings, the arguments passed to the program on its command-line,
or what's currently left of them as they are processed")

(defvar *command-line-options* nil
  "command-line options as parsed into a plist")

(defvar *command-line-option-specification* nil
  "the (prepared) specification for how to parse command-line options")

;; A raw specification is a list of individual option specifications.
;; An individual option specification is:
;; A single option name or a list of option names, and a keyword/value list of option options.
;; An option name is a single character #\x for short option -x,
;; or a string "foo" for long option --foo.
;; option options are:

;; :type for specifying a parameter type for the option.
;;  A type may be any of:
;;  NIL - the option takes no parameter.
;;  BOOLEAN - the option takes a boolean parameter.  The value can be true, false, yes, no, t, nil, y, n.
;;       If it's a long option, --no-foo is defined, too.
;;  STRING - the option takes a string as parameter
;;  INTEGER - the option takes an integer as parameter, interpreted in decimal.

;; :optional for allowing the option to have no parameter
;;  for a list, it allows the final list to be empty.

;; :action for specifying an action to do when the option is found
;;  an action may be a symbol to set, a function to call, nil to do nothing,
;;  or a keyword to push on the option plist.
;;  default action is to make a keyword from the first name.

;; :list The value is a plist with keywords :initial-contents and :symbol.
;;  The :type must be integer or string.
;;  :symbol is a special variable and :initial-contents is a list.
;;  While the options are being processed, the special variable is bound to the
;;  initial contents, reversed.
;;  At the end of option processing, the finalizer reverses the list.

;; :negation  Creates string called "no-XXX", or "disable-XXX" if the original name
;;  is "enable-XXX".

;; A *prepared* specification is an EQUAL-hash-table that maps option names to
;; a simple-vector #(action type optional) that specifies what to do when the option
;; is encountered in the command-line. It also includes three special entries for
;; keywords :local-symbol :local-values :finalizers that specify the local symbols
;; to bind when parsing options for this specification, the values to which to bind them,
;; and a list of finalizers to run after the parsing is done.

(defun make-option-action (p name &key (action nil actionp) list optional &allow-other-keys)

  "This is called for one option specification.
   P is the hash table of actions.  NAME is the first name of this option, a string
   or a character.  The keywords are option options for this option specification."

  (let ((actual-action
         ;; This is usually the same as ACTION, but if ACTION is #'FOO,
         ;; then it's the symbol-function of FOO, and if no action is
         ;; provided, it's a keyword named NAME.
         (cond
           ((and (consp action) (eq 'function (car action))
                 (consp (cdr action)) (null (cddr action)))
            (symbol-function (cadr action)))
           (actionp
            action)
           (t
	    (intern (string-upcase name) :keyword)))))
    ;; If the :LIST option is not specified, just return the actual-action.
    (if list
      (destructuring-bind (&key initial-contents (symbol (gensym (string-upcase name))))
	  (and (listp list) list)
	(let ((final-action #'(lambda ()
				(let ((value (symbol-value symbol)))
				  (unless (or optional value)
				    (error "No option ~A defined" (option-name name)))
				  (command-line-action actual-action (reverse value))))))
	  (push symbol (gethash :local-symbols p))
	  (push (reverse initial-contents) (gethash :local-values p))
	  (push final-action (gethash :finalizers p))
	  #'(lambda (value)
	      (case value
		((nil) (set symbol nil))
		((t)   (error "Option ~A requires a parameter" (option-name name)))
		(otherwise (push value (symbol-value symbol)))))))
      actual-action)))


(defun prepare-command-line-options-specification (specification)

  "Given a SPECIFICATION, return a hash table with one entry
   whose key is the name and whose value is a vector of the action,
   the type, and whether it's optional."

  (etypecase specification
    (hash-table specification)
    (list
     (let ((p (make-hash-table :test 'equal)))
       (dolist (spec specification)
         (destructuring-bind (names &rest option-options
                                    &key action type optional negation list)
             spec
           (declare (ignorable action))
           (when list
             (unless (member type '(integer string))
               (error "option specification wants list but doesn't specify string or integer")))
           (let* ((namelist (if (listp names) names (list names)))
                  (firstname (car namelist))
                  (pos-action (apply 'make-option-action p firstname option-options)))
             ;; For each name of this spec, put an entry into the hash table
             ;; mapping that name to a vector of the action, the type, and
             ;; whether it's optional.
             (loop with spec = (vector pos-action type (and optional (not list)))
                   for name in namelist do
                   (setf (gethash name p) spec))
             ;; Deal with negation.
             (when (or (eq type 'boolean) list optional)
               (let ((neg-action #'(lambda (value)
                                     (command-line-action pos-action (not value))))
                     (negation-list (if (listp negation) negation (list negation))))
                 (loop for name in namelist
                       when (stringp name) do
                       (push (concatenate 'string "no-" name) negation-list)
                       (when (and (<= 7 (length name))
                                  (string= "enable-" (subseq name 0 7)))
                         (push (concatenate 'string "disable-" (subseq name 7 nil))
                               negation-list)))
                 (loop with spec = (vector neg-action nil nil)
                       for name in negation-list do
                       (setf (gethash name p) spec)))))))
       p))))

(defun command-line-option-specification (option)
  (let ((v (gethash option *command-line-option-specification*)))
    (if v (values t (svref v 0) (svref v 1) (svref v 2)) (values nil nil nil nil))))

(defun command-line-action (action &optional value)
  (etypecase action
    (null nil)
    (keyword  (setf *command-line-options*
		    (list* action value *command-line-options*)))
    (symbol   (set action value))
    (function (funcall action value))))

(defun short-option-p (arg)

  "ARG is a string.  Is it like -A, but not --?"

  (check-type arg simple-string)
  (and (<= 2 (length arg))
       (char= #\- (schar arg 0))
       (char/= #\- (schar arg 1))))

(defun negated-short-option-p (arg)

  "ARG is a string.  Is it like +A?"

  (check-type arg simple-string)
  (and (<= 2 (length arg))
       (char= #\+ (schar arg 0))))

(defun long-option-p (arg)

  "ARG is a string.  Is it like --A?"

  (check-type arg simple-string)
  (and (<= 3 (length arg))
       (char= #\- (schar arg 0) (schar arg 1))))

(defun option-end-p (arg)
  (check-type arg simple-string)
  (string= arg "--"))

(defun option-like-p (arg)
  (check-type arg simple-string)
  (and (<= 2 (length arg))
       (or (char= #\- (schar arg 0))
           (char= #\+ (schar arg 0)))))

(defun option-name (option-designator)
  (etypecase option-designator
    (character (format nil "-~A" option-designator))
    (string    (format nil "--~A" option-designator))))

(defun coerce-option-parameter (option string type)

  "Given a STRING option value and a TYPE, return the value as
   a Lisp object.  OPTION is the name of the option, just for
   error messages."

  (flet ((fail ()
           (error "parameter for option ~A not of type ~A" (option-name option) type)))
    (ecase type
      ((nil)
       (error "option ~A does not take a parameter" (option-name option)))
      ((string)
       string)
      ((boolean)
       (cond
         ((member string '("true" "t" "1" "yes" "y") :test #'string-equal)
          t)
         ((member string '("false" "nil" "0" "no" "n") :test #'string-equal)
          nil)
         (t
          (fail))))
      ((integer)
       (multiple-value-bind (value end) (parse-integer string :junk-allowed t)
         (unless (and (integerp value) (= end (length string))) (fail))
         value)))))

(defun get-option-parameter (option type optional)
  (cond
    ((member type '(boolean t nil))
     t)
    ((and optional
          (or (null *command-line-arguments*)
              (option-like-p (car *command-line-arguments*))))
     t)
    (t
     (coerce-option-parameter option (pop *command-line-arguments*) type))))

(defun process-option (option validp action parameter type optional)
  (unless validp (error "Undefined option ~A" (option-name option)))
  (typecase parameter
    (null
     (unless (or (eq type 'boolean) optional)
       (error "Option ~A cannot be negated" (option-name option))))
    (string
     (setf parameter (coerce-option-parameter option parameter type)))
    (t
     (setf parameter (get-option-parameter option type optional))))
  (command-line-action action parameter))

(defun process-short-option (c &key negated)
  (multiple-value-bind (validp action type optional)
      (command-line-option-specification c)
    (process-option c validp action (not negated) type optional)))

(defun decompose-long-option-string (string)
  (let* ((separator (position #\= string :start 2))
         (name (subseq string 2 separator))
         (parameter (if separator (subseq string (1+ separator)) t)))
    (values name parameter)))

(defun process-long-option (s)
  (multiple-value-bind (name parameter) (decompose-long-option-string s)
    (multiple-value-bind (validp action type optional)
        (command-line-option-specification name)
      (process-option name validp action parameter type optional))))

(defun do-process-command-line-options ()

  "Remove all the options and values from *COMMAND-LINE-ARGUMENTS*.
   Process each option."

  (progv
      (gethash :local-symbols *command-line-option-specification*)
      (gethash :local-values *command-line-option-specification*)
    (loop for arg = (pop *command-line-arguments*) do
      (cond
	((or (null arg) (option-end-p arg))
	 (return))
	((short-option-p arg)
	 (loop for c across (subseq arg 1 nil) do
	   (process-short-option c)))
	((negated-short-option-p arg)
	 (loop for c across (subseq arg 1 nil) do
	   (process-short-option c :negated t)))
	((long-option-p arg)
	 (process-long-option arg))
	(t
         (push arg *command-line-arguments*)
	 (return))))
    (loop for f in (gethash :finalizers *command-line-option-specification*)
          do (funcall f))))

(defun process-command-line-options (specification command-line)

  "SPECIFICATION is a list as described above.  COMMAND-LINE
   is the list of tokens to be parsed.  Return two values:
   a list of alternating actions and values,
   and a list of the rest of the arguments after the
   various options and their values (a tail of the
   COMMAND-LINE argument)."

  (let*
      ((*command-line-option-specification*
        ;; The hash table describing each name.
	(prepare-command-line-options-specification specification))
       (*command-line-arguments*
        command-line)
       (*command-line-options* nil))
    (do-process-command-line-options)
    (values *command-line-options* *command-line-arguments*)))

(defun compute-and-process-command-line-options (specification)
  (process-command-line-options specification (get-command-line-arguments)))


#| Testing:

(defparameter *opt-spec*
 '((("all" #\a) :type boolean)
   (("verbose" #\v) :type boolean)
   (("file" #\f) :type string)
   (("xml-port" #\x) :type integer :optional t)
   ("enable-qpx-cache" :type boolean)
   ("path" :type string :list t :optional t)
   ("port" :type integer :list (:initial-contents (1 2)) :optional t)))

(defun foo (args &key all verbose file xml-port enable-qpx-cache port path)
  (list args :all all :verbose verbose :file file :xml-port xml-port
        :enable-qpx-cache enable-qpx-cache :port port :path path))

(multiple-value-bind (options arguments)
    (process-command-line-options
     *opt-spec*
     '("--all" "--no-verbose" "--file" "foo" "-f" "-v" "-v"
       "-x" "--disable-qpx-cache"
       "--no-port" "--port" "3" "--port=4"
       "--path" "/foo" "--path" "/bar"
       "--" "--foo" "bar" "baz"))
  (write arguments :pretty nil) (terpri)
  (write options :pretty nil) (terpri)
  (write (apply 'foo arguments options) :pretty nil)
  (terpri))

|#
