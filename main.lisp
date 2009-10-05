;;; Shell command-line interface for XCVB

#+xcvb
(module (:depends-on
         ("makefile-backend" "search-path"
          "asdf-converter" "extract-target-properties"
          (:when (:featurep :sbcl)
            (:require :sb-posix)
            (:require :sb-sprof)))))

(in-package :xcvb)

(declaim (optimize (speed 2) (safety 3) (compilation-speed 0) (debug 3)))

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
        *lisp-setup-dependencies* +fast-xcvb-setup-dependencies+
        *pathname-grain-cache* (make-hash-table :test 'equal))
  (initialize-search-path)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make-Makefile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +make-makefile-option-spec+
 '((("build" #\b) :type string :optional nil :documentation "specify what system to build")
   (("setup" #\s) :type string :optional t :documentation "specify a Lisp setup file")
   (("xcvb-path" #\x) :type string :optional t :documentation "override your XCVB_PATH")
   (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
   (("object-directory" #\O) :type string :initial-value "obj" :documentation "specify object directory")
   (("lisp-implementation" #\i) :type string :initial-value "sbcl" :documentation "specify type of Lisp implementation")
   (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
   (("disable-cfasl" #\C) :type boolean :optional t :documentation "disable the CFASL feature")
   (("verbosity" #\v) :type integer :initial-value 5 :documentation "set verbosity")
   (("base-image" #\B) :type boolean :optional t :initial-value t :documentation "use a base image")
   (("master" #\m) :type boolean :optional t :initial-value t :documentation "enable XCVB-master")
   (("profiling" #\P) :type boolean :optional t :documentation "profiling")))

(defun make-makefile (arguments &key
                                xcvb-path setup verbosity output-path
                                build lisp-implementation lisp-binary-path
                                disable-cfasl master object-directory base-image profiling)
  (with-maybe-profiling (profiling)
    (reset-variables)
    (when arguments
      (error "Invalid arguments to make-makefile"))
    (when xcvb-path
      (set-search-path! xcvb-path))
    (setf *use-master* master)
    (when master
      (appendf *lisp-setup-dependencies* `((:fasl "/xcvb/master/master"))))
    (when setup
      (appendf *lisp-setup-dependencies* `((:lisp ,setup))))
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
    (setf *use-base-image* base-image)
    (search-search-path)
    (write-makefile (canonicalize-fullname build) :output-path output-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASDF to XCVB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Remove XCVB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +remove-xcvb-option-spec+
  '((("build" #\b) :type string :optional nil
     :documentation "Specify XCVB build to remove modules from")
    (("xcvb-path" #\x) :type string :optional t
     :documentation "override your XCVB_PATH")
    (("verbosity" #\v) :type integer :optional t :initial-value 5 :documentation "set verbosity (default: 5)")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Show Search Path ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +show-search-path-option-spec+
  '((("xcvb-path" #\x) :type string :optional t :documentation "override your XCVB_PATH")))

(defun show-search-path-command (arguments &key xcvb-path)
  (when arguments
    (error "Invalid arguments to show-search-path: ~S~%" arguments))
  (reset-variables)
  (when xcvb-path
    (set-search-path! xcvb-path))
  (search-search-path)
  (show-search-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; XCVB to ASDF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +xcvb-to-asdf-option-spec+
  '((("build" #\b) :type string :optional nil :list t :documentation "Specify a build to convert (can be repeated)")
    (("name" #\n) :type string :optional t :documentation "name of the new ASDF system")
    (("output-path" #\o) :type string :optional t :documentation "pathname for the new ASDF system")
    (("xcvb-path" #\x) :type string :optional t :documentation "override your XCVB_PATH")
    (("lisp-implementation" #\i) :type string :initial-value "sbcl" :documentation "specify type of Lisp implementation")
    (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
    (("verbosity" #\v) :type integer :initial-value 5 :documentation "set verbosity")))

(defun xcvb-to-asdf-command (arguments &key
                             build name output-path verbosity xcvb-path
                             lisp-implementation lisp-binary-path)
  (when arguments
    (error "Invalid arguments to asdf-to-xcvb: ~S~%" arguments))
  (reset-variables)
  (when verbosity
    (setf *xcvb-verbosity* verbosity))
  (when xcvb-path
    (set-search-path! xcvb-path))
  (when lisp-implementation
    (setf *lisp-implementation-type*
          (find-symbol (string-upcase lisp-implementation) (find-package :keyword))))
  (when lisp-binary-path
    (setf *lisp-executable-pathname* lisp-binary-path))
  (extract-target-properties)
  (read-target-properties)
  (search-search-path)
  (write-asd-file
   :asdf-name name
   :build-names (mapcar #'canonicalize-fullname build)
   :output-path output-path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; non-enforcing makefile ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +non-enforcing-makefile-option-spec+
 '((("build" #\b) :type string :optional nil :list t :documentation "specify a (series of) system(s) to build")
   (("base-image" #\B) :type boolean :optional t :initial-value nil :documentation "use a base image")
   (("name" #\n) :type string :optional t :initial-value "xcvb-tmp" :documentation "ASDF name for the target")
   (("setup" #\s) :type string :optional t :documentation "specify a Lisp setup file")
   (("xcvb-path" #\x) :type string :optional t :documentation "override your XCVB_PATH")
   (("output-path" #\o) :type string :initial-value "xcvb-ne.mk" :documentation "specify output path")
   (("object-directory" #\O) :type string :initial-value "obj-ne" :documentation "specify object directory")
   (("lisp-implementation" #\i) :type string :initial-value "sbcl" :documentation "specify type of Lisp implementation")
   (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
   (("verbosity" #\v) :type integer :initial-value 5 :documentation "set verbosity")
   (("parallel" #\P) :type boolean :optional t :initial-value nil :documentation "compile in parallel with POIU")
;  (("force-cfasl" #\C) :type boolean :optional t :initial-value nil :documentation "force use of CFASL")
;  (("profiling" #\P) :type boolean :optional t :documentation "profiling")
   ))

(defun non-enforcing-makefile (arguments &key
                               build base-image setup xcvb-path name
                               output-path object-directory
                               lisp-implementation lisp-binary-path
                               verbosity parallel #|force-cfasl profiling|#)
  (reset-variables)
  (when arguments
    (error "Invalid arguments to non-enforcing-makefile"))
  (when xcvb-path
    (set-search-path! xcvb-path))
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
  ;;(setf *use-cfasls* force-cfasl)
  (setf *use-base-image* base-image)
  (setf *lisp-setup-dependencies*
        (append +xcvb-setup-dependencies+
                (when setup `((:lisp ,setup)))))
  (search-search-path)
  (write-non-enforcing-makefile
   (mapcar #'canonicalize-fullname build)
   :asdf-name name
   :output-path output-path
   :parallel parallel))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; slave builder ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +slave-builder-option-spec+
 '((("build" #\b) :type string :optional nil :documentation "specify a (series of) system(s) to build")
   (("setup" #\s) :type string :optional t :documentation "specify a Lisp setup file")
   (("xcvb-path" #\x) :type string :optional t :documentation "override your XCVB_PATH")
   (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
   (("object-directory" #\O) :type string :initial-value "obj-ne" :documentation "specify object directory")
   (("lisp-implementation" #\i) :type string :initial-value "sbcl" :documentation "specify type of Lisp implementation")
   (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
   (("disable-cfasl" #\C) :type boolean :optional t :initial-value nil :documentation "disable use of CFASL")
   (("base-image" #\B) :type boolean :optional t :initial-value nil :documentation "use a base image")
   (("verbosity" #\v) :type integer :initial-value 5 :documentation "set verbosity")
   (("profiling" #\P) :type boolean :optional t :documentation "profiling")
   ))

(defun slave-builder (arguments &key
                       build setup xcvb-path
                       output-path object-directory
                       lisp-implementation lisp-binary-path
                       disable-cfasl base-image verbosity profiling)
  (xcvb-driver::debugging)
  (multiple-value-bind (makefile-path makefile-dir)
      (make-makefile
       arguments :master t
       :build build :setup setup
       :xcvb-path xcvb-path :output-path output-path
       :object-directory object-directory
       :lisp-implementation lisp-implementation :lisp-binary-path lisp-binary-path
       :disable-cfasl disable-cfasl :base-image base-image :verbosity verbosity :profiling profiling)
    (let ((*standard-output* *error-output*)
          #|#+sbcl (sb-alien::*default-c-string-external-format* :iso-8859-1)|#)
      (run-program/process-output-stream
       "make" (list "-C" (namestring makefile-dir) "-f" (namestring makefile-path))
       (lambda (stream) (copy-stream-to-stream-line-by-line stream *standard-output*))))
    (let* ((image-grain (graph-for (make-instance 'static-traversal)
                                   `(:image ,(canonicalize-fullname build))))
           (included (image-included image-grain)))
      (with-safe-io-syntax ()
        (write `(:xcvb () ;;; TODO: do something about non-file dependencies
                       ,(manifest-form
                         (loop :for grain :in included
                           :for fullname = (fullname grain)
                           :collect (cons fullname
                                          (merge-pathnames (dependency-namestring fullname)
                                                           makefile-dir)))))
               :readably t :pretty t :case :downcase)
        (terpri)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make a load manifest ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +make-manifest-option-spec+
  '((("output" #\o) :type string :optional t :initial-value "-"
     :documentation "Path to manifest file or - for stdout")
    (("grains" #\g) :type string :optional nil
     :documentation "alist of grains, mapping fullname to pathname")))

(defun make-manifest (arguments &key output grains)
  (when arguments
    (error "Invalid arguments to make-manifest: ~S~%" arguments))
  (create-manifest output (with-safe-io-syntax () (read-from-string grains))))


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
    (("xcvb-to-asdf" "x2a") xcvb-to-asdf-command +xcvb-to-asdf-option-spec+
     "Extract an ASDF system from XCVB"
     "Automatically extract an ASDF system from one or many XCVB builds.")
    (("non-enforcing-makefile" "nemk" "nm") non-enforcing-makefile +non-enforcing-makefile-option-spec+
     "Create some Makefile for a non-enforcing build"
     "Create some Makefile for a non-enforcing build,
that will use ASDF or POIU to create one or a series of images each containing
the previous image, a build and its dependencies.")
    (("show-search-path" "search-path" "ssp") show-search-path-command +show-search-path-option-spec+
     "Show builds in the specified XCVB path"
     "Show builds in the implicitly or explicitly specified XCVB path.
For debugging your XCVB configuration.")
    (("slave-builder") slave-builder +slave-builder-option-spec+
     "Build some project as a slave to the XCVB master (for internal use)"
     "Build some project as a slave to the XCVB master (for internal use)")
    (("make-manifest") make-manifest +make-manifest-option-spec+
     "Create a manifest of files to load (for internal use)"
     "given fullnames and paths, output fullnames, tthsum and paths")
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
        (setf *tmp-directory-pathname* (ensure-pathname-is-directory tmp)))))
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
