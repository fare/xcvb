;;; Shell command-line interface for XCVB

#+xcvb
(module (:depends-on ("specials" "macros")))

(in-package :xcvb)

(declaim (optimize (speed 2) (safety 3) (compilation-speed 0) (debug 3)))

(defun reset-variables ()
  ;; TODO: have some macro define notable variables
  ;; so they will be reset here.
  (setf *grains* (make-hash-table :test 'equal)
        *computations* nil
        *target-system-features* nil
        *target-added-features* nil
        *target-suppressed-features* nil
        *source-registry* nil
        *lisp-setup-dependencies* +fast-xcvb-setup-dependencies+
        *pathname-grain-cache* (make-hash-table :test 'equal)
        *worlds* (make-hash-table :test 'equal)
        *print-readably* nil)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Command Spec ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spec for XCVB command-line commands.  Each item of the list takes the form
;; (aliases function short-help long-help) where aliases is a list of strings
;; that can invoke the command, function is the function to call when the
;; command is invoked, short-help is a short help string for the user, and
;; long-help is a longer help string for the user.
(defparameter +xcvb-commands+
  '((("help" "-?" "--help" "-h")
     program-help ()
     "Output some help message"
     "With no additional arguments, the 'help' command provides general help on
using XCVB.  Using 'xcvb help COMMAND' where COMMAND is the name of an XCVB
command gives specific help on that command.")
    (("make-makefile" "mkmk" "mm")
     make-makefile +make-makefile-option-spec+
     "Create some Makefile"
     "Create Makefile rules to build a project.")
    (("simple-build" "simb" "sb")
     simple-build-command +simple-build-option-spec+
     "Build the xcvb dependencies"
     "Determine the dependencies and execute shell commands to build the
application")
    (("asdf-to-xcvb" "a2x")
     asdf-to-xcvb-command +asdf-to-xcvb-option-spec+
     "Convert ASDF system to XCVB"
     "Attempt an automated conversion of an ASDF system to XCVB.
Optionally load a setup Lisp file before anything else, so the user gets
a chance to load and/or configure ASDF itself and any extension thereof.")
    (("remove-xcvb" "rm-x" "rmx" "rx")
     remove-xcvb-command +remove-xcvb-option-spec+
     "Remove XCVB modules from files in build"
     "Given an XCVB build file, removes the XCVB modules from each of the files listed in the build file.")
    (("list-files" "lf")
     list-files-command +list-files-option-spec+
     "List files in a XCVB build"
     "Given an XCVB build file, list all files that are directly part of that build.")
    (("purge-xcvb" "pux" "px")
     purge-xcvb-command ()
     "Remove XCVB module statements from explicitly listed files"
     "Given a list of files, remove the XCVB module statements from each Lisp file and delete specified build files.")
    (("xcvb-to-asdf" "x2a")
     xcvb-to-asdf-command +xcvb-to-asdf-option-spec+
     "Extract an ASDF system from XCVB"
     "Automatically extract an ASDF system from one or many XCVB builds.")
    (("non-enforcing-makefile" "nemk" "nm")
     non-enforcing-makefile +non-enforcing-makefile-option-spec+
     "Create some Makefile for a non-enforcing build"
     "Create some Makefile for a non-enforcing build,
that will use ASDF or POIU to create one or a series of images each containing
the previous image, a build and its dependencies.")
    (("show-source-registry" "source-registry" "ssr")
     show-source-registry-command +show-source-registry-option-spec+
     "Show builds in the configured source registry"
     "Show builds in the implicitly or explicitly configured source registry.
For debugging your XCVB configuration.")
    (("find-module" "fm")
     find-module +find-module-option-spec+
     "Show builds in the specified XCVB path"
     "Show builds in the implicitly or explicitly specified XCVB path.
For debugging your XCVB configuration.")
    (("show-settings" "show" "s")
     (show-settings-command :rest-arity t) +show-settings-option-spec+
     "Show settings"
     "Show settings implicitly or explicitly configured from user-specified options.")
    (("slave-builder")
     slave-builder +slave-builder-option-spec+
     "Build some project as a slave to the XCVB master (for internal use)"
     "Build some project as a slave to the XCVB master (for internal use)")
    (("make-manifest")
     make-manifest +make-manifest-option-spec+
     "Create a manifest of files to load (for internal use)"
     "given fullnames and paths, output fullnames, tthsum and paths")
    #+xcvb-farmer
    (("build")
     standalone-build-command +standalone-build-option-spec+
     "build a project (experimental)"
     "build the project directly")
    (("load")
     load-command ()
     "Load a Lisp file"
     "Load a Lisp file in the context of XCVB itself. For XCVB developers only.")
    (("eval")
     eval-command ()
     "Evaluate some Lisp form"
     "Evaluate some Lisp form in the context of XCVB itself. For XCVB developers only.")
    (("repl")
     repl-command ()
     "Start a REPL"
     "The 'repl' command takes no additional arguments.
Using 'xcvb repl' launches a Lisp REPL.
For XCVB developers only (notably for use with SLIME).")
    (("version" "-V" "--version")
     show-version ()
     "Show version"
     "The 'version' command ignores all arguments, and prints out the version number
for this version of XCVB.")))

(defparameter +source-registry-option-spec+
  '((("source-registry" #\S) :type string :optional t
     :documentation "override your source-registry")))

(defparameter +lisp-implementation-option-spec+
  '((("lisp-implementation" #\l) :type string :initial-value "sbcl" :documentation "specify type of Lisp implementation")
    (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
    (("define-feature" #\D) :type string :list t :optional t :documentation "define a CL into the target")
    (("undefine-feature" #\U) :type string :list t :optional t :documentation "undefine a CL from the target")))

(defparameter +cfasl-option-spec+
  '((("disable-cfasl" #\C) :type boolean :optional t :documentation "disable the CFASL feature")))

(defparameter +verbosity-option-spec+
  '((("verbosity" #\v) :type integer :initial-value 5 :documentation "set verbosity")
    (("debugging" #\Z) :type boolean :optional t :initial-value nil :documentation "debug")))

(defparameter +setup-option-spec+
  '((("setup" #\s) :type string :optional t :documentation "specify a Lisp setup file")))

(defparameter +base-image-option-spec+
  '((("use-base-image" #\B) :type boolean :optional t :initial-value t :documentation "use a base image")))

(defparameter +profiling-option-spec+
  '((("profiling" #\P) :type boolean :optional t :documentation "profiling")))

(defparameter +object-directory-option-spec+
  '((("object-directory" #\O) :type string :optional t :documentation "specify object directory")))

(defparameter +build-option-spec+
  '((("build" #\b) :type string :optional nil :documentation "specify what build to process")))

(defparameter +multi-build-option-spec+
  '((("build" #\b) :type string :list t :optional nil :documentation "specify what builds to process")))


;; Lookup the command spec for the given command name, or return nil if the
;; given command name is invalid.
(defun lookup-command (command-name &key (commands +xcvb-commands+))
  (flet ((member-equalp (x l) (member x l :test #'equalp)))
    (assoc command-name commands :test #'member-equalp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simple Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Command to print out more detailed help.
(defun program-help (args &key (commands +xcvb-commands+) (name "xcvb"))
  (if (null args)
      ;; If user typed "xcvb help", give general help, summarizing commands.
      (format t "~&Usage: ~A COMMAND ARGS~%  ~
       where COMMAND is one of the following:~%~%~
       ~:{    ~1{~22A~} ~*~*~A~%~}~%~
       See 'xcvb help COMMAND' for more information on a specific command.~%"
              name commands)
      ;; Else if user typed "xcvb help COMMAND", give specific help on that
      ;; command.
      (let* ((command (car args))
             (command-spec (lookup-command command :commands commands))
             (command-options (symbol-value (third command-spec))))
        (unless (null (cdr args))
          (errexit 2 "Too many arguments -- try 'xcvb help'."))
        (cond
          (command-spec
            (format t "~1{Command: ~A~%Aliases:~{ ~A~^ ~}~%~%~3*~A~&~}"
                    (cons command command-spec))
            (when command-options
              (command-line-arguments:show-option-help command-options :sort-names t)))
          (t
           (errexit 2 "Invalid XCVB command ~S -- try 'xcvb help'."
                    command))))))

;; Command to print out a version string.
(defun show-version (args)
  (declare (ignore args))
  (let ((features
         (loop :for f :in '(:farmer)
           :when (member (intern (format nil "~A-~A" :xcvb f) :keyword) *features*)
           :collect (string-downcase f))))
    (format t "~&XCVB version ~A
built on ~A ~A
~@[with ~{~A~#[~; and ~:;, ~]~} enabled~%~]~
using ~A~%"
            *xcvb-version* (lisp-implementation-type) (lisp-implementation-version)
            features (or #+xcvb-using-asdf :asdf :xcvb))))

;; Command to load a file.
(defun load-command (args)
  (unless (list-of-length-p 1 args)
    (errexit 2 "load requires exactly 1 argument, a file to load"))
    (load (car args)))

;; Command to eval a file.
(defun eval-command (args)
  (unless (list-of-length-p 1 args)
    (errexit 2 "eval requires exactly 1 argument, a form to evaluate"))
    (eval (read-from-string (car args))))

;; Command to start a REPL.
(defun repl-command (args)
  (unless (null args)
    (errexit 2 "repl doesn't take any argument"))
  (initialize-environment)
  (xcvb-driver:debugging)
  #+clisp (setf *standard-input* *terminal-io*)
  (invoke-restart 'revert-to-repl))

(defun repl ()
  (let (/ // /// * ** *** + ++ +++ -)
    #+sbcl (sb-impl::toplevel-repl nil)
    #-sbcl (loop (rep))))
(defun prompt ()
  (format nil "~A> " (package-name *package*)))
(defun rep ()
  (let ((eof '#:eof))
    (format t "~&")
    (write-string (prompt))
    (xcvb-driver:finish-outputs)
    (setf - (read nil nil eof))
    (when (eq - eof) (exit 0))
    (psetf / (multiple-value-list (eval -)) // / /// //)
    (psetf * (car /) ** * *** **)
    (psetf + - ++ + +++ ++)
    (format t "~{~S~^ ;~%~}" /)
    (xcvb-driver:finish-outputs)))

;;;; Show options
(defparameter +show-settings-option-spec+
  `(,@+build-option-spec+
    ,@+source-registry-option-spec+
    ,@+lisp-implementation-option-spec+
    ,@+cfasl-option-spec+
    ,@+verbosity-option-spec+
    ,@+setup-option-spec+
    ,@+object-directory-option-spec+
    (("all" #\a) :type boolean :optional t :documentation "show all settings")))

(defparameter *showable-settings*
  '(*debugging* *xcvb-verbosity*
    *lisp-implementation-type*
    *lisp-executable-pathname*
    *target-added-features*
    *target-suppressed-features*
    *target-system-features*
    *target-lisp-image-pathname*
    *target-lisp-executable-pathname*
    *target-asdf-version*
    *implementation-identifier*
    *use-cfasls*
    *object-directory*
    *object-directory-pathname*
    *use-base-image*
    *use-master*
    *lisp-setup-dependencies*))

(defun show-settings-command (args &rest keys
			      &key build source-registry setup use-base-image verbosity debugging
			      lisp-implementation lisp-binary-path define-feature undefine-feature
			      disable-cfasl object-directory all)
  (declare (ignore build source-registry setup use-base-image verbosity debugging
		   lisp-implementation lisp-binary-path define-feature undefine-feature
		   disable-cfasl object-directory))
  (apply 'handle-global-options keys)
  #|TODO: (setf *object-directory* ... DO THE SAME AS MAKEFILE-BACKEND ?)|#
  (flet ()
    (if all
	(dolist (var *showable-settings*)
	  (format t "~(~A~) = ~S~%" var (symbol-value var)))
	(loop :for attr :in args
	   :for var = (find-symbol (format nil "*~:@(~A~)*" attr) :xcvb) :do
	   (if (find var *showable-settings*)
	       (format t "~A~%" (symbol-value var))
	       (die "Not a showable setting: ~A" attr))))))

;;;; Common option handling
(defun object-directory-default ()
  (get-target-properties)
  (asdf:merge-pathnames*
   (asdf:coerce-pathname
    (strcat ".cache/xcvb/" *implementation-identifier* "/"))
   (asdf::user-homedir)))

(defun handle-global-options (&rest keys &key
                              verbosity
                              source-registry
                              object-directory
                              lisp-implementation lisp-binary-path
                              disable-cfasl master setup
                              use-base-image
                              debugging
                              define-feature undefine-feature
                              (use-target-lisp t)
                              &allow-other-keys)
    (reset-variables)
    (when debugging
      (xcvb-driver:debugging))
    (when verbosity
      (setf *xcvb-verbosity* verbosity))
    (log-format-pp 9 "xcvb options: ~S" keys)
    (initialize-xcvb-source-registry source-registry)
    (search-source-registry)
    (when lisp-implementation
      (let ((type (find-symbol (string-upcase lisp-implementation) (find-package :keyword))))
        (unless (and type (get-lisp-implementation type))
          (errexit 2 "No known supported Lisp implementation ~S" lisp-implementation))
        (setf *lisp-implementation-type* type)))
    (when lisp-binary-path
      (setf *lisp-executable-pathname* lisp-binary-path))
    (when use-target-lisp
      (flet ((m (x) (mapcar (lambda (s) (intern (string-upcase s) :keyword)) x)))
        (setf *target-added-features* (m define-feature))
        (setf *target-suppressed-features* (m undefine-feature))
        (when-bind (i) (intersection *target-added-features* *target-suppressed-features*)
          (errexit 2 "Can't both define and undefine feature~P ~
                    ~{~A~#[~; and ~:;, ~]~}" (length i) i)))
      (read-target-properties) ;; Gets information from target Lisp.
      (setf *use-cfasls* ;; Must be done after read-target-properties
            (and *use-cfasls* (not disable-cfasl)))
      (setf *object-directory* (or object-directory (object-directory-default))))
    (when *object-directory*
      (setf *object-directory-pathname* (ensure-pathname-is-directory *object-directory*))
      (setf *object-directory* (but-last-char (namestring *object-directory-pathname*)))
      (log-format 8 "object-directory: given ~S using ~S" object-directory *object-directory*))
    (setf *use-base-image* (and *target-can-dump-image-p* use-base-image))
    (setf *use-master* master)
    (when master
      (ensure-tthsum-present))
    (when setup
      (let ((module (resolve-absolute-module-name setup)))
        (unless module
          (error "Cannot find setup module ~A" setup))
        (append1f *lisp-setup-dependencies* (fullname module)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main (&rest arguments)
  (main* :name "xcvb" :commands +xcvb-commands+ :arguments arguments))

(defun main* (&rest keys &key name commands arguments)
  (declare (ignore name commands arguments))
  (with-coded-exit ()
    (flet ((quit (&optional (code 0))
             (log-format 9 "quitting with code ~A" code)
             (quit code)))
      (restart-case
	  (handler-case
	      ;; revert-to-repl is in a nested restart-case so that the other two
	      ;; restarts are available from the repl.
	      (restart-case
		  (progn
		    (apply 'interpret-command-line keys)
		    (quit 0))
		(revert-to-repl ()
		  :report (lambda (stream)
			    (format stream "Abort current computation and bring up a toplevel REPL"))
		  (repl)))
	    (user-error (c)
	      ;; XXX Hrm, why doesn't this seem to work when I set
	      ;; XCVB_DEBUGGING=t in the environment?
	      (if *debugging*
		  (bork c)
		  (die "~A" c))))
	(exit (&optional (exit-code 0))
	  :report (lambda (stream)
		    ;; when invoked interactively exit-code is always 0
		    (format stream "Abort current computation and quit the process with process exit code 0"))
          (quit exit-code))
        (abort ()
          :report (lambda (stream)
                    (format stream "Abort current computation and quit the process with process exit code 111"))
          (quit 111))))))

(defun initialize-environment ()
  ;;; This setting helps extract-target-properties.lisp. See there.
  #+sbcl (sb-posix:unsetenv "SBCL_HOME")

  ;;; This setting makes things sensible for run-program on CCL
  #+clozure (setf *error-output* ccl::*stderr* *trace-output* ccl::*stderr*)

  ;;; Determine the temporary directory
  (labels ((v (x)
             (let ((s (asdf:getenv x)))
               (and (not (equal s "")) s))))
    (let ((tmp (or (v "TMP") (v "TMPDIR"))))
      (when tmp
        (setf *tmp-directory-pathname* (ensure-pathname-is-directory tmp)))))

  ;;; In case debugging is involved, make things easier on the printer
  (setf *print-pretty* nil
        *print-readably* nil))

(defun interpret-command-line (&key name commands arguments)
  (setf *arguments* arguments)
  (initialize-environment)
  (let* ((*package* (find-package :xcvb-user))
         (command (pop arguments))
         (command-spec (lookup-command command :commands commands))
         (fun (second command-spec))
         (option-spec-var (third command-spec)))
    (multiple-value-bind (fun keys)
	(etypecase fun (cons (values (car fun) (cdr fun))) (symbol (values fun nil)))
      (cond
	(option-spec-var
	 (apply 'handle-command-line
		(symbol-value option-spec-var) fun
		:command-line arguments :name command
		keys))
	(fun
	 (funcall fun arguments))
	((not command)
	 (errexit 2 "~:@(~A~) requires a command -- try '~A help'." name name))
	(t
	 (errexit 2 "Invalid ~:@(~A~) command ~S -- try '~A help'." name command name))))))

(defun cmdize/1 (x)
  (typecase x
    (character (format nil "--~A" x))
    (keyword (format nil "--~(~A~)" x))
    (symbol (string-downcase x))
    (string x)
    (list (with-safe-io-syntax () (write-to-string x)))
    (t (princ-to-string x))))

(defun cmdize* (args)
  (mapcar #'cmdize/1 args))

(defun cmdize (&rest args)
  (cmdize* args))

(defun cmd* (&rest args)
  "For debugging purposes, let the user try a command from the REPL"
  (interpret-command-line :arguments (cmdize* args) :name "xcvb" :commands +xcvb-commands+))

(defun cmd (&rest args)
  "For debugging purposes, let the user try a command from the REPL, in a local context"
  (with-local-xcvb-vars ()
    (apply 'cmd* args)))

(defun exit (&optional (code 0) &rest r)
  (declare (ignore r))
  (invoke-restart 'exit code))

(defun errformat (fmt &rest args)
  (fresh-line *error-output*)
  (apply #'format *error-output* fmt args)
  (fresh-line *error-output*))

(defun errexit (code fmt &rest args)
  (apply #'errformat fmt args)
  (exit code))
