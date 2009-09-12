;;; Shell command-line interface for XCVB

#+xcvb
(module (:depends-on
         ("makefile-backend" "search-path"
          "asdf-converter" "extract-target-properties"
          (:when (:featurep :sbcl)
            (:require :sb-posix)
            (:require :sb-sprof)))))

(in-package :xcvb)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix)
  (require :sb-sprof))

#+sbcl
(defun call-with-maybe-profiling (maybe thunk)
  (if maybe
    (sb-sprof:with-profiling (:max-samples 10000 :report :graph :loop nil)
      (funcall thunk))
    (funcall thunk)))

#-sbcl
(defun call-with-maybe-profiling (maybe thunk)
  (declare (ignore maybe))
  (funcall thunk))

(defmacro with-maybe-profiling ((maybe) &body body)
  `(call-with-maybe-profiling ,maybe (lambda () ,@body)))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make-Makefile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +make-makefile-option-spec+
 '((("build" #\b) :type string :optional nil :documentation "specify what system to build")
   (("setup" #\s) :type string :optional t :documentation "specify a Lisp setup file")
   (("xcvb-path" #\x) :type string :optional t :documentation "override your XCVB_PATH")
   (("output-path" #\o) :type string :optional t :documentation "specify output path (default: xcvb.mk)")
   (("object-directory" #\O) :type string :optional t :documentation "specify object directory (default: obj)")
   (("lisp-implementation" #\i) :type string :optional t :documentation "specify type of Lisp implementation (default: sbcl)")
   (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
   (("disable-cfasl" #\C) :type boolean :optional t :documentation "disable the CFASL feature")
   (("verbosity" #\v) :type integer :optional t :documentation "set verbosity (default: 5)")
   (("profiling" #\P) :type boolean :optional t :documentation "profiling")))

(defun make-makefile (arguments &key
                                xcvb-path setup verbosity output-path
                                build lisp-implementation lisp-binary-path
                                disable-cfasl object-directory profiling)
  (with-maybe-profiling (profiling)
    (reset-variables)
    (when arguments
      (error "Invalid arguments to make-makefile"))
    (when xcvb-path
      (set-search-path! xcvb-path))
    (when setup
      (setf *lisp-setup-dependencies*
            (append *lisp-setup-dependencies* `((:lisp ,setup)))))
    (when verbosity
      (setf *xcvb-verbosity* verbosity))
    (when output-path
      (setf *default-pathname-defaults*
            (ensure-absolute-pathname (pathname-directory-pathname output-path))))
    (when object-directory
      (setf *object-directory* ;; strip last "/"
            (but-last-char (enough-namestring (ensure-pathname-is-directory object-directory)))))
    (when lisp-implementation
      (setf *lisp-implementation-type*
            (find-symbol (string-upcase lisp-implementation) (find-package :keyword))))
    (when lisp-binary-path
      (setf *lisp-executable-pathname* lisp-binary-path))
    (extract-target-properties)
    (read-target-properties)
    (when disable-cfasl
      (setf *use-cfasls* nil))
    (search-search-path)
    (write-makefile (canonicalize-fullname build) :output-path output-path)))

(defparameter +asdf-to-xcvb-option-spec+
  '((("system" #\b) :type string :optional nil :list t :documentation "Specify a system to convert (can be repeated)")
    (("base" #\B) :type string :optional t :documentation "Base pathname for the new build")
    (("name" #\n) :type string :optional t :documentation "name of the resulting system")
    (("setup"  #\s) :type string :optional t :documentation "Specify the path to a Lisp setup file.")
    (("system-path" #\p) :type string :optional t :list t :documentation "Register an ASDF system path (can be repeated)")
    (("preload" #\l) :type string :optional t :list t :documentation "Specify an ASDF system to preload (can be repeated)")
    (("verbosity" #\v) :type integer :optional t :documentation "set verbosity (default: 5)")))

(defun asdf-to-xcvb-command (arguments &key system setup system-path preload verbosity base name)
  (when arguments
    (error "Invalid arguments to asdf-to-xcvb: ~S~%" arguments))
  (when verbosity
    (setf *xcvb-verbosity* verbosity))
  (setf asdf:*central-registry*
        (append (mapcar #'ensure-pathname-is-directory system-path) asdf:*central-registry*))
  (when setup (load setup))
  (asdf-to-xcvb
   :name name
   :systems (mapcar #'coerce-asdf-system-name system)
   :systems-to-preload (mapcar #'coerce-asdf-system-name preload)
   :base-pathname (when base (ensure-pathname-is-directory base))
   :verbose (and verbosity (> verbosity 5))))

(defparameter +remove-xcvb-option-spec+
  '((("build" #\b) :type string :optional nil
     :documentation "Specify XCVB build to remove modules from")
    (("xcvb-path" #\x) :type string :optional t
     :documentation "override your XCVB_PATH")
    (("verbosity" #\v) :type integer :optional t :default 5 :documentation "set verbosity (default: 5)")))

(defun remove-xcvb-command (arguments &key xcvb-path verbosity build)
  ;;(declare (ignore xcvb-path verbosity))
  (when arguments
    (error "Invalid arguments to remove-xcvb: ~S~%" arguments))
  (when verbosity
    (setf *xcvb-verbosity* verbosity))
  (remove-xcvb-from-build
   :xcvb-path xcvb-path
   :build build))

;; Using the build.xcvb as a starting point, finds files and
;; strips XCVB modules from them.
(defun remove-xcvb-from-build (&key xcvb-path build)
  (reset-variables)
  (when xcvb-path
    (set-search-path! xcvb-path))
  (search-search-path)
  (let ((build (registered-build (canonicalize-fullname build))))
    (with-slots (depends-on) build
      (dolist (name depends-on)
        (let ((path (module-subpathname (grain-pathname build) name)))
          (remove-module-from-file path)))
      (delete-file (grain-pathname build)))))

(defparameter +show-search-path-option-spec+
  '((("xcvb-path" #\x) :type string :optional t :documentation "override your XCVB_PATH")))

(defun show-search-path-command (arguments &key xcvb-path)
  (when arguments
    (error "Invalid arguments to show-search-path: ~S~%" arguments))
  (reset-variables)
  (when xcvb-path
    (set-search-path! xcvb-path))
  (show-search-path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Command Spec ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spec for XCVB command-line commands.  Each item of the list takes the form
;; (aliases function short-help long-help) where aliases is a list of strings
;; that can invoke the command, function is the function to call when the
;; command is invoked, short-help is a short help string for the user, and
;; long-help is a longer help string for the user.
(defparameter +xcvb-commands+
  '((("help" "-?" "--help" "-h") program-help ()
     "Output some help message"
     "With no additional arguments, the 'help' command provides general help on
using XCVB.  Using 'xcvb help COMMAND' where COMMAND is the name of an XCVB
command gives specific help on that command.")
    (("make-makefile" "mkmk" "mm") make-makefile +make-makefile-option-spec+
     "Create some Makefile"
     "Create Makefile rules to build a project.")
    (("asdf-to-xcvb" "a2x") asdf-to-xcvb-command +asdf-to-xcvb-option-spec+
     "Convert ASDF system to XCVB"
     "Attempt an automated conversion of an ASDF system to XCVB.
Optionally load a setup Lisp file before anything else, so the user gets
a chance to load and/or configure ASDF itself and any extension thereof.")
    (("remove-xcvb" "rm-x" "rx") remove-xcvb-command +remove-xcvb-option-spec+
     "Remove XCVB modules from files in build"
     "Given an XCVB build file, removes the XCVB modules from each of the files listed in the build file.")
    (("show-search-path" "search-path" "ssp") show-search-path-command +show-search-path-option-spec+
     "Show builds in the specified XCVB path"
     "Show builds in the implicitly or explicitly specified XCVB path.
For debugging your XCVB configuration.")
    (("load") load-command ()
     "Load a Lisp file"
     "Load a Lisp file in the context of XCVB itself. For XCVB developers only.")
    (("eval") eval-command ()
     "Evaluate some Lisp form"
     "Evaluate some Lisp form in the context of XCVB itself. For XCVB developers only.")
    (("repl") repl-command ()
     "Start a REPL"
     "The 'repl' command takes no additional arguments.
Using 'xcvb repl' launches a Lisp REPL.
For XCVB developers only (notably for use with SLIME).")
    (("version" "-V" "--version") show-version ()
     "Show version"
     "The 'version' command ignores all arguments, and prints out the version number
for this version of XCVB.")))


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
          (errexit 2 "~&Too many arguments -- try 'xcvb help'.~%"))
        (cond
          (command-spec
            (format t "~&~1{Command: ~A~%Aliases:~{ ~A~^ ~}~%~%~3*~A~}~%"
                    (cons command command-spec))
            (when command-options
              (command-line-arguments:show-option-help command-options)))
          (t
           (errexit 2 "~&Invalid XCVB command ~S -- try 'xcvb help'.~%"
                    command))))))

;; Command to print out a version string.
(defun show-version (args)
  (declare (ignore args))
  (format t "~&XCVB version ~A~%(compiled with ~A ~A)~%"
          *xcvb-version* (lisp-implementation-type) (lisp-implementation-version)))

;; Command to load a file.
(defun load-command (args)
  (unless (list-of-length-p 1 args)
    (error "load requires exactly 1 argument, a file to load"))
    (load (car args)))

;; Command to eval a file.
(defun eval-command (args)
  (unless (list-of-length-p 1 args)
    (error "eval requires exactly 1 argument, a form to evaluate"))
    (eval (read-from-string (car args))))

;; Command to start a REPL.
(defun repl-command (args)
  (unless (null args)
    (error "repl doesn't take any argument"))
  #-(or sbcl) (error "REPL unimplemented")
  (throw :repl nil))

(defun repl ()
  #+sbcl (progn (sb-ext:enable-debugger) (sb-impl::toplevel-repl nil))
  #-(or sbcl) (error "REPL unimplemented"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (catch :repl
    (with-coded-exit ()
      (quit (catch :exit
              (interpret-command-line
               (command-line-arguments:get-command-line-arguments))
              0))))
    (repl))

(defun initialize-environment ()
  #+sbcl (sb-posix:putenv (strcat "SBCL_HOME=" *lisp-implementation-directory*))
  (labels ((v (x)
             (let ((s (cl-launch:getenv x)))
               (and (not (equal s "")) s))))
    (let ((tmp (or (v "TMP") (v "TMPDIR"))))
      (when tmp
        (setf *tmp-directory* (ensure-pathname-is-directory tmp)))))
  (setf *print-pretty* nil))

(defun interpret-command-line (args)
  (initialize-environment)
  (let* ((*package* (find-package :xcvb-user))
         (command (pop args))
         (command-spec (lookup-command command))
         (fun (second command-spec))
         (option-spec-var (third command-spec)))
    (cond
      (option-spec-var
       (multiple-value-bind (options arguments)
           (process-command-line-options (symbol-value option-spec-var) args)
         (apply fun arguments options)))
      (fun
       (funcall fun args))
      ((not command)
       (errexit 2 "~&XCVB requires a command -- try 'xcvb help'.~%"))
      (t
       (errexit 2 "~&Invalid XCVB command ~S -- try 'xcvb help'.~%" command)))))

(defun exit (&optional (code 0) &rest r)
  (declare (ignore r))
  (throw :exit code))

(defun errformat (fmt &rest args)
  (apply #'format *error-output* fmt args))

(defun errexit (code fmt &rest args)
  (apply #'errformat fmt args)
  (exit code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
