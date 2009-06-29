;;; Shell command-line interface for XCVB

#+xcvb (module (:depends-on ("static-backends" "search-path" "computations")))

(in-package :xcvb)

(defun reset-variables ()
  ;; TODO: have some macro define notable variables
  ;; so they will be reset here.
  (setf *grains* (make-hash-table :test 'equal)
        *computations* nil
        *target-system-features* nil
        *search-path-searched-p* nil
        *lisp-setup-dependencies* +xcvb-setup-dependencies+
        *pathname-grain-cache* (make-hash-table :test 'equal))
  (initialize-search-path)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Command Spec ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spec for XCVB command-line commands.  Each item of the list takes the form
;; (aliases function short-help long-help) where aliases is a list of strings
;; that can invoke the command, function is the function to call when the
;; command is invoked, short-help is a short help string for the user, and
;; long-help is a longer help string for the user.
(defparameter +xcvb-commands+
  '((("eval") eval-command "Eval some Lisp form"
     "TODO write long help for eval command")
    (("help" "-?" "--help" "-h") program-help "Output this help message"
"With no additional arguments, the 'help' command provides general help on
using XCVB.  Using 'xcvb help COMMAND' where COMMAND is the name of an XCVB
command gives specific help on that command.")
    (("load") load-command "Load a Lisp file"
     "TODO write long help for load command")
    (("make-makefile" "mkmk" "mm") make-makefile "Create some Makefile"
     "TODO write long help for mkmk command")
    (("repl") repl-command "Start a REPL"
"The 'repl' command takes no additional arguments.  Using 'xcvb repl' launches
a Lisp REPL.")
    (("usage" nil) program-usage "Output usage info"
"The 'usage' command ignores all arguments, and prints out a simple usage
string for XCVB.")
    (("version" "-V" "--version") show-version "Show version"
"The 'version' command ignores all arguments, and prints out the version number
for this version of XCVB.")))

;; Lookup the command spec for the given command name, or return nil if the
;; given command name is invalid.
(defun lookup-command (command-name)
  (flet ((member-equalp (x l) (member x l :test #'equalp)))
    (assoc command-name +xcvb-commands+ :test #'member-equalp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simple Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Command to print out a usage string.
(defun program-usage (args)
  (declare (ignore args))
  (format t "~&Usage: xcvb COMMAND ARGS~%  ~
	  where COMMAND is one of the following:~%   ~{ ~A~}~%~
          See 'xcvb help' for more information.~%"
          (mapcar #'caar +xcvb-commands+)))

;; Command to print out more detailed help.
(defun program-help (args)
  (if (null args)
      ;; If user typed "xcvb help", give general help, summarizing commands.
      (format t "~&Usage: xcvb COMMAND ARGS~%  ~
       where COMMAND is one of the following:~%~%~
       ~:{    ~1{~16A~}~*~A~%~}~%~
       See 'xcvb help COMMAND' for more information on a specific command.~%"
              +xcvb-commands+)
      ;; Else if user typed "xcvb help COMMAND", give specific help on that
      ;; command.
      (let* ((command (car args))
             (command-spec (lookup-command command)))
        (unless (null (cdr args))
          (errexit 2 "~&Too many arguments -- try 'xcvb help'.~%"))
        (if command-spec
            (format t "~&~1{Command: ~A~%Aliases:~{ ~A~^ ~}~%~%~2*~A~}~%"
                    (cons command command-spec))
            (errexit 2 "~&Invalid XCVB command ~S -- try 'xcvb help'.~%"
                     command)))))

;; Command to print out a version string.
(defun show-version (args)
  (declare (ignore args))
  (format t "~&XCVB version ~A~%" *xcvb-version*))

;; Command to load a file.
(defun load-command (args)
  (unless (list-of-length-p 1 args)
    (error "load requires exactly 1 argument, a file to load"))
    (load (car args)))

;; Command to eval a file.
(defun eval-command (args)
  (unless (list-of-length-p 1 args)
    (error "eval requires exactly 1 argument, a file to load"))
    (eval (read-from-string (car args))))

;; Command to start a REPL.
(defun repl-command (args)
  (unless (null args)
    (error "repl doesn't take any argument"))
  #-(or sbcl) (error "REPL unimplemented")
  (throw :repl nil))

(defun repl ()
  #+sbcl (sb-ext:enable-debugger)
  #+sbcl (sb-impl::toplevel-repl nil)
  #-(or sbcl) (error "REPL unimplemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make-Makefile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +make-makefile-option-spec+
 '((("xcvb-path" #\x) :type string :optional t)
   (("setup" #\s) :type string :optional t)
   (("verbosity" #\v) :type integer :optional t)
   (("output-path" #\o) :type string :optional t)
   (("build" #\b) :type string :optional nil)
   (("target-lisp-impl" #\i) :type string :optional t)  ;; 'i' for 'implementation'
   (("target-lisp-bin" #\p) :type string :optional t))) ;; 'p' for 'path' of binary.

;; Create obj/target-features.lisp-expr, which contains the target Lisp's *features*
(defun make-target-features-lisp-expr ()
  (ensure-directories-exist "obj/")
  (asdf:run-shell-command
   (format nil
	   "~A > obj/target-features.lisp-expr"
	   (shell-tokens-to-string
	    (lisp-invocation-arglist
	     ;; Let the user somehow provide additional features,
	     ;; perhaps in a setup-features.lisp file
	     :eval (format nil "(progn (write *features* :readably t) (terpri) ~A)"
			   (format nil (slot-value
					(get-lisp-implementation
					 *lisp-implementation-type*) 'quit-format) 0)))))))

(defun make-makefile (args)
  (multiple-value-bind (options arguments)
      (process-command-line-options +make-makefile-option-spec+ args)
    (reset-variables)
    (when arguments
      (error "Invalid arguments to make-makefile"))
    (destructuring-bind (&key xcvb-path setup verbosity output-path
        build target-lisp-impl target-lisp-bin) options
      (when xcvb-path
        (set-search-path! xcvb-path))
      (when setup
        (push `(:lisp ,setup) *lisp-setup-dependencies*))
      (when verbosity
        (setf *xcvb-verbosity* verbosity))
      (when target-lisp-impl
	(setf *lisp-implementation-type* (find-symbol (string-upcase target-lisp-impl) (find-package :keyword))))
      (when target-lisp-bin
	(setf *lisp-executable-pathname* target-lisp-bin)
	(make-target-features-lisp-expr))
      (search-search-path)
      (write-makefile build :output-path output-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (catch :repl
    (with-exit-on-error ()
      (quit (catch :exit
              (interpret-command-line
               (command-line-arguments:get-command-line-arguments))
              0))))
    (repl))

(defun interpret-command-line (args)
  (let* ((*package* (find-package :xcvb-user))
         (command (pop args))
         (fun (second (lookup-command command))))
    (if fun
        (funcall fun args)
        (errexit 2 "~&Invalid XCVB command ~S -- try 'xcvb help'.~%"
                 command))))

(defun exit (&optional (code 0) &rest r)
  (declare (ignore r))
  (throw :exit code))

(defun errformat (fmt &rest args)
  (apply #'format *error-output* fmt args))

(defun errexit (code fmt &rest args)
  (apply #'errformat fmt args)
  (exit code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
