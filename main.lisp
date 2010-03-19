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
     show-source-registry-command +source-registry-option-spec+
     "Show builds in the configured source registry"
     "Show builds in the implicitly or explicitly configured source registry.
For debugging your XCVB configuration.")
    (("find-module" "fm")
     find-module +find-module-option-spec+
     "Show builds in the specified XCVB path"
     "Show builds in the implicitly or explicitly specified XCVB path.
For debugging your XCVB configuration.")
    (("slave-builder")
     slave-builder +slave-builder-option-spec+
     "Build some project as a slave to the XCVB master (for internal use)"
     "Build some project as a slave to the XCVB master (for internal use)")
    (("make-manifest")
     make-manifest +make-manifest-option-spec+
     "Create a manifest of files to load (for internal use)"
     "given fullnames and paths, output fullnames, tthsum and paths")
    (("build")
     standalone-build-command +standalone-build-option-spec+
     "build a project (not implemented yet - will fail)"
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


;; Lookup the command spec for the given command name, or return nil if the
;; given command name is invalid.
(defun lookup-command (command-name)
  (flet ((member-equalp (x l) (member x l :test #'equalp)))
    (assoc command-name +xcvb-commands+ :test #'member-equalp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simple Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Command to print out more detailed help.
(defun program-help (args)
  (if (null args)
      ;; If user typed "xcvb help", give general help, summarizing commands.
      (format t "~&Usage: xcvb COMMAND ARGS~%  ~
       where COMMAND is one of the following:~%~%~
       ~:{    ~1{~18A~}~*~*~A~%~}~%~
       See 'xcvb help COMMAND' for more information on a specific command.~%"
              +xcvb-commands+)
      ;; Else if user typed "xcvb help COMMAND", give specific help on that
      ;; command.
      (let* ((command (car args))
             (command-spec (lookup-command command))
             (command-options (symbol-value (third command-spec))))
        (unless (null (cdr args))
          (errexit 2 "Too many arguments -- try 'xcvb help'."))
        (cond
          (command-spec
            (format t "~1{Command: ~A~%Aliases:~{ ~A~^ ~}~%~%~3*~A~}"
                    (cons command command-spec))
            (when command-options
              (command-line-arguments:show-option-help command-options :sort-names t)))
          (t
           (errexit 2 "Invalid XCVB command ~S -- try 'xcvb help'."
                    command))))))

;; Command to print out a version string.
(defun show-version (args)
  (declare (ignore args))
  (format t "~&XCVB version ~A~%(compiled with ~A ~A)~%"
          *xcvb-version* (lisp-implementation-type) (lisp-implementation-version)))

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

;;;; Common option handling

(defun handle-global-options (&rest keys &key
                              verbosity
                              source-registry
                              object-directory
                              lisp-implementation lisp-binary-path
                              disable-cfasl master setup
                              base-image
                              debugging
                              &allow-other-keys)
    (reset-variables)
    (when debugging
      (xcvb-driver:debugging))
    (when verbosity
      (setf *xcvb-verbosity* verbosity))
    (log-format 9 "~&xcvb options: ~S~%" keys)
    (initialize-source-registry source-registry)
    (search-source-registry)
    (when object-directory
      (setf *object-directory* ;; strip last "/"
            (but-last-char
             (namestring
              (ensure-pathname-is-directory
               object-directory)))))
    (log-format 8 "~&object-directory: given ~S using ~S " object-directory *object-directory*)
    (when lisp-implementation
      (let ((type (find-symbol (string-upcase lisp-implementation) (find-package :keyword))))
        (unless (and type (get-lisp-implementation type))
          (errexit 2 "No known supported Lisp implementation ~S" lisp-implementation))
        (setf *lisp-implementation-type* type)))
    (when lisp-binary-path
      (setf *lisp-executable-pathname* lisp-binary-path))
    (read-target-properties) ;; Gets information from target Lisp.
    (when disable-cfasl ;; Must be done after read-target-properties
      (setf *use-cfasls* nil))
    (setf *use-base-image* base-image)
    (setf *use-master* master)
    (when master
      (ensure-tthsum-present)
      (append1f *lisp-setup-dependencies* '(:fasl "/xcvb/master")))
    (when setup
      (let ((module (resolve-absolute-module-name setup)))
        (unless module
          (error "Cannot find setup module ~A" setup))
        (append1f *lisp-setup-dependencies* `(:lisp ,(fullname module))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (with-coded-exit ()
    (flet ((quit (&optional (code 0))
             (log-format 9 "~&quitting with code ~A~%" code)
             (quit code)))
      (restart-case
          ;; revert-to-repl is in a nested restart-case so that the other two
          ;; restarts are available from the repl.
          (restart-case
              (progn
                (interpret-command-line
                 (command-line-arguments:get-command-line-arguments))
                (quit 0))
            (revert-to-repl ()
              :report (lambda (stream)
                        (format stream "Abort current computation and bring up a toplevel REPL"))
              (repl)))
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
  ;;; This setting helps extract-target-properties.lisp
  ;;; NOTE: sb-posix:putenv only works in SBCL 1.0.33.21 and later.
  ;;#+sbcl (sb-posix:putenv (strcat "SBCL_HOME=" *lisp-implementation-directory*))

  ;;; This setting makes things sensible for run-program on CCL
  #+clozure (setf *error-output* ccl::*stderr* *trace-output* ccl::*stderr*)

  ;;; Determine the temporary directory
  (labels ((v (x)
             (let ((s (asdf-utilities:getenv x)))
               (and (not (equal s "")) s))))
    (let ((tmp (or (v "TMP") (v "TMPDIR"))))
      (when tmp
        (setf *tmp-directory-pathname* (ensure-pathname-is-directory tmp)))))

  ;;; In case debugging is involved, make things easier on the printer
  (setf *print-pretty* nil
        *print-readably* nil))

(defun interpret-command-line (args)
  (setf *arguments* args)
  (initialize-environment)
  (let* ((*package* (find-package :xcvb-user))
         (command (pop args))
         (command-spec (lookup-command command))
         (fun (second command-spec))
         (option-spec-var (third command-spec)))
    (cond
      (option-spec-var
       (handle-command-line (symbol-value option-spec-var) fun
                            :command-line args :name command))
      (fun
       (funcall fun args))
      ((not command)
       (errexit 2 "XCVB requires a command -- try 'xcvb help'."))
      (t
       (errexit 2 "Invalid XCVB command ~S -- try 'xcvb help'." command)))))

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

(defun cmd (&rest args)
  "For debugging purposes, let the user try a command from the REPL"
  (interpret-command-line (cmdize* args)))

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
