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

;;; TODO: make every command self-documenting by integrating help and argument parsing.
;;; The command-line-arguments library may have to be extended for that.
;;; Also, some options are global

(defparameter +xcvb-commands+
  '((("help" "-?" "--help" "-h" "-H" nil) program-help "Output this help message")
    (("make-makefile" "mkmk" "mm") make-makefile "Create some Makefile")
    (("version" "-V" "--version") show-version "Show version")
    (("load") load-command "Load a Lisp file")
    (("eval") eval-command "Eval some Lisp form")
    (("repl") repl-command "Start a REPL")))

(defun program-help (args)
  (declare (ignore args))
  (format t "~&Usage: xcvb COMMAND ARGS~%  ~
	  where COMMAND is one of the following:~%   ~{ ~A~}~%"
          (mapcar #'caar +xcvb-commands+)))

(defun show-version (args)
  (declare (ignore args))
  (format t "~&XCVB ~A~%" *xcvb-version*))

(defparameter +make-makefile-option-spec+
 '((("xcvb-path" #\x) :type string :optional t)
   (("setup" #\s) :type string :optional t)
   (("verbosity" #\v) :type integer :optional t)
   (("output-path" #\o) :type string :optional t)
   (("build" #\b) :type string :optional nil)))

(defun make-makefile (args)
  (multiple-value-bind (options arguments) (process-command-line-options +make-makefile-option-spec+ args)
    (reset-variables)
    (when arguments
      (error "Invalid arguments to make-makefile"))
    (destructuring-bind (&key xcvb-path setup verbosity output-path build) options
      (when xcvb-path
        (set-search-path! xcvb-path))
      (when setup
        (push `(:lisp ,setup) *lisp-setup-dependencies*))
      (when verbosity
        (setf *xcvb-verbosity* verbosity))
      (search-search-path)
      (write-makefile build :output-path output-path))))

(defun load-command (args)
  (unless (list-of-length-p 1 args)
    (error "load requires exactly 1 argument, a file to load"))
  (let ((*package* (find-package :xcvb-user)))
    (load (car args))))

(defun eval-command (args)
  (unless (list-of-length-p 1 args)
    (error "eval requires exactly 1 argument, a file to load"))
  (let ((*package* (find-package :xcvb-user)))
    (eval (read-from-string (car args)))))

(defun repl-command (args)
  (unless (null args)
    (error "repl doesn't take any argument"))
  #-(or sbcl) (error "REPL unimplemented")
  (throw :repl nil))

(defun repl ()
  #+sbcl (sb-ext:enable-debugger)
  #+sbcl (sb-impl::toplevel-repl nil)
  #-(or sbcl) (error "REPL unimplemented"))

(defun main ()
  (catch :repl
    (with-exit-on-error ()
      (quit
       (catch :exit
         (interpret-command-line (command-line-arguments:get-command-line-arguments))
         0))))
  (let ((*package* (find-package :xcvb-user)))
    (repl)))

(defun member-equalp (x l)
  (member x l :test #'equalp))

(defun interpret-command-line (args)
  (let* ((command (pop args))
         (fun (second (assoc command +xcvb-commands+ :test #'member-equalp))))
    (if fun
        (funcall fun args)
        (errexit 2 "~&Invalid XCVB command ~S -- try xcvb help~%" command))))

(defun exit (&optional (code 0) &rest r)
  (declare (ignore r))
  (throw :exit code))

(defun errformat (fmt &rest args)
  (apply #'format *error-output* fmt args))

(defun errexit (code fmt &rest args)
  (apply #'errformat fmt args)
  (exit code))
