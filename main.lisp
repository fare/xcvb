;;; Shell command-line interface for XCVB

#+xcvb
(module (:depends-on ("commands")))

(in-package :xcvb)

(declaim (optimize (speed 1) (safety 3) (compilation-speed 0) (debug 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Backdoor Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These commands allow the user to do stuff with XCVB.

(define-command load-command
    (("load" "--load")
     (args) ()
     "Load a Lisp file"
     "Load a Lisp file in the context of XCVB itself. For XCVB developers only.
If any arguments are left on the command-line, they are interpreted as further commands")
  (unless args
    (errexit 2 "load requires at least one argument, a file to load"))
  (let* ((*package* (find-package :xcvb-user)))
    (load (pop args)))
  (when args
    (interpret-command args)))

(define-command eval-command
    (("eval" "--eval")
     (args) ()
     "Evaluate some Lisp form"
     "Evaluate some Lisp form in the context of XCVB itself. For XCVB developers only.
If any arguments are left on the command-line, they are interpreted as further commands")
  (unless args
    (errexit 2 "eval requires at least one argument, a form to evaluate"))
  (let* ((*package* (find-package :xcvb-user)))
    (eval (read-from-string (pop args))))
  (when args
    (interpret-command args)))

(define-command repl-command
    (("repl" "--repl" "--")
     (args) ()
     "Start a REPL"
     "The 'repl' evaluates each of its arguments.
Using 'xcvb repl' launches a Lisp REPL.
For XCVB developers only (notably for use with SLIME).")
  (initialize-environment)
  (xcvb-driver:debugging)
  #+clisp (setf *standard-input* *terminal-io*)
  (setf *arguments* args)
  (setf *package* (find-package :xcvb-user))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Last Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define these ones last, so they appear at the bottom of the help list,
;; easily seen by the user.

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
    *cache*
    *object-cache*
    *object-cache-namestring*
    *install-prefix*
    *install-program*
    *install-configuration*
    *install-data*
    *install-library*
    *install-image*
    *install-lisp*
    *use-base-image*
    *use-master*
    *lisp-setup-dependencies*))

(define-command (show-settings-command :rest-arity t)
    (("show-settings" "show" "s")
     (args &rest keys &key)
     `(,@+build-option-spec+
       ,@+source-registry-option-spec+
       ,@+lisp-implementation-option-spec+
       ,@+cfasl-option-spec+
       ,@+verbosity-option-spec+
       ,@+setup-option-spec+
       ,@+workspace-option-spec+
       ,@+install-option-spec+
       (("all" #\a) :type boolean :optional t :documentation "show all settings"))
     "Show settings"
     "Show settings implicitly or explicitly configured from user-specified options."
     (all))
  (apply 'handle-global-options keys)
  (if all
      (dolist (var *showable-settings*)
        (format t "~(~A~) = ~S~%" var (symbol-value var)))
      (loop :for attr :in args
        :for var = (find-symbol (format nil "*~:@(~A~)*" attr) :xcvb) :do
        (if (find var *showable-settings*)
            (format t "~A~%" (symbol-value var))
            (die "Not a showable setting: ~A" attr)))))

(define-command show-version
    (("version" "-V" "--version")
     (args) ()
     "Show version"
     "The 'version' command ignores all arguments, and prints out the version number
for this version of XCVB.")
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

(define-command program-help
    (("help" "-?" "--help" "-h")
     (args &key (name *program*)) ()
     "Output some help message"
     "With no additional arguments, the 'help' command provides general help on
using XCVB.  Using 'xcvb help COMMAND' where COMMAND is the name of an XCVB
command gives specific help on that command.")
  (if (null args)
      ;; If user typed "xcvb help", give general help, summarizing commands.
      (format t "~&Usage: ~:@(~A~) COMMAND ARGS~%  ~
       where COMMAND is one of the following:~%~%~
       ~:{    ~1{~22A~} ~A~%~}~%~
       See '~2:*~A help COMMAND' for more information on a specific command.~%"
              name (mapcar (lambda (c) (destructuring-bind (&key names short-description &allow-other-keys)
                                           (registered-command-properties c)
                                         (list names short-description)))
                           (car (commands))))
      ;; Else if user typed "xcvb help COMMAND", give specific help on that
      ;; command.
      (let* ((command (first args))
             (function (gethash command (cdr (commands))))
             (properties (registered-command-properties function))
             (command-options (getf properties :option-spec))
             (names (getf properties :names))
             (description (getf properties :description)))
        (unless (null (cdr args))
          (errexit 2 "Too many arguments -- try 'xcvb help'."))
        (cond
          (properties
            (format t "Command: ~A~%Aliases:~{ ~A~^ ~}~%~%~A~&"
                    (first names) (rest names) description)
            (when command-options
              (command-line-arguments:show-option-help command-options :sort-names t)))
          (t
           (errexit 2 "Invalid XCVB command ~S -- try 'xcvb help'."
                    command))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Common option handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-xcvb-cache (cache)
  (ensure-directory-pathname
   (or cache
       (getenv-absolute-directory "XCVB_CACHE")
       (subpathname* (getenv-absolute-directory "XDG_CACHE_HOME") "xcvb/")
       (subpathname (user-homedir) ".cache/xcvb/"))))

(defun compute-xcvb-cache! (cache)
  (orf *cache* (compute-xcvb-cache cache)))

(defun compute-xcvb-workspace (workspace)
  (ensure-directory-pathname
   (or workspace
       (getenv-absolute-directory "XCVB_WORKSPACE")
       (subpathname
        (ensure-pathname-absolute *default-pathname-defaults*) "workspace/"))))

(defun compute-xcvb-workspace! (workspace)
  (orf *workspace* (compute-xcvb-workspace workspace)))

(defun compute-object-cache (object-cache)
  (ensure-directory-pathname
   (or object-cache
       (getenv-absolute-directory "XCVB_OBJECT_CACHE")
       (progn
         (assert *cache*)
         (get-target-properties)
         (subpathname *cache* *implementation-identifier* :type :directory)))))

(defun compute-object-cache! (object-cache)
  (orf *object-cache* (compute-object-cache object-cache))
  (setf *object-cache-namestring* (namestring *object-cache*)))

(defun compute-install-paths
    (&key install-prefix install-program install-configuration
     install-data install-library install-image install-lisp)
  ;; http://www.pathname.com/fhs/
  ;; http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
  ;; http://technet.microsoft.com/en-us/library/cc749104(WS.10).aspx
  (when (or (eq install-prefix t) (equal install-prefix "~"))
    (setf install-prefix (user-homedir)))
  (macrolet ((x (&rest vars)
               `(progn ,@(loop :for var :in vars :append
                           `((check-type ,var (or null string pathname))
                             (when (stringp ,var)
                               (setf ,var (ensure-directory-pathname ,var))))))))
    (x install-prefix install-program install-configuration
       install-data install-library install-image install-lisp))
  (when (null install-prefix)
    (cond
      ((os-unix-p)
       (setf install-prefix #p"/usr/local/"))
      ((os-windows-p)
       (let ((common-appdata (or #+lispworks (sys:get-folder-path :common-appdata)
                                 (getenv-absolute-directory "ALLUSERSAPPDATA")
                                 (subpathname* (getenv-absolute-directory "ALLUSERSPROFILE") "Application Data/")
				 (error "Can't locate common-appdata"))))
         (setf install-prefix (subpathname common-appdata "common-lisp/"))
         (orf install-data (subpathname install-prefix "share/"))
         (orf install-library (subpathname install-prefix "lib/"))
         (orf install-image (subpathname install-prefix "images/"))
         (orf install-lisp install-prefix)))))
  (when (equal install-prefix (user-homedir)) ;; Magic: home directory!
    (cond
      ((os-unix-p)
       (orf install-program
            (subpathname install-prefix "bin/"))
       (orf install-configuration
            (getenv-absolute-directory "XDG_CONFIG_HOME")
            (subpathname install-prefix ".config/"))
       (orf install-data
            (subpathname install-prefix ".local/share/"))
       (orf install-library
            (subpathname install-prefix ".local/lib/")))
      ((os-windows-p)
       (let* ((appdata (or #+lispworks (sys:get-folder-path :appdata)
                           (getenv-absolute-directory "APPDATA")
                           (subpathname install-prefix "Application Data/")))
              (localappdata (or #+lispworks (sys:get-folder-path :local-appdata)
                                (getenv-absolute-directory "LOCALAPPDATA")
                                (subpathname appdata "Local/"))))
         (orf install-program (subpathname localappdata "bin/"))
         (orf install-configuration (subpathname localappdata "config/"))
         (orf install-data (subpathname localappdata "data/"))
         (orf install-library (subpathname localappdata "lib/"))
         (orf install-image (subpathname install-library "common-lisp/images/"))
         (orf install-lisp localappdata (subpathname install-data "common-lisp/"))))))
  (when (and (os-unix-p) (equal install-prefix #p"/usr/"))
    (orf install-configuration #p"/etc/"))
  (orf install-program (subpathname install-prefix "bin/"))
  (orf install-configuration (subpathname install-prefix (if (os-unix-p) "etc/" "config/")))
  (orf install-data (subpathname install-prefix "share/"))
  (orf install-library (subpathname install-prefix "lib/"))
  (orf install-image (subpathname install-library "common-lisp/images/"))
  (orf install-lisp (subpathname install-data "common-lisp/"))
  (values install-prefix install-program install-configuration
          install-data install-library install-image install-lisp))

(defun compute-install-paths!
    (&key install-prefix install-program install-configuration
     install-data install-library install-image install-lisp)
  (multiple-value-setq (*install-prefix* *install-program*
                        *install-configuration* *install-data* *install-library*
                        *install-image* *install-lisp*)
    (compute-install-paths
     :install-prefix install-prefix
     :install-program install-program
     :install-configuration install-configuration
     :install-data install-data
     :install-library install-library
     :install-image install-image
     :install-lisp install-lisp)))

(defun handle-global-options (&rest keys &key
                              verbosity
                              source-registry
                              lisp-implementation lisp-binary-path
                              disable-cfasl master setup
                              use-base-image
                              debugging profiling
                              define-feature undefine-feature
                              (use-target-lisp t)
                              xcvb-program
                              required-xcvb-version
                              cache object-cache workspace
                              install-prefix
                              install-program
                              install-configuration
                              install-data
                              install-library
                              install-image
                              install-lisp
                              &allow-other-keys)
  (reset-variables)
  (when debugging
    (xcvb-driver:debugging))
  (setf xcvb-driver:*profiling* profiling)
  (when verbosity
    (setf *xcvb-verbosity* verbosity))
  (log-format-pp 9 "xcvb options: ~S" keys)
  (when lisp-implementation
    (let ((type (find-symbol (string-upcase lisp-implementation) (find-package :keyword))))
      (unless (and type (get-lisp-implementation type))
        (errexit 2 "No known supported Lisp implementation ~S" lisp-implementation))
      (setf *lisp-implementation-type* type)))
  (when lisp-binary-path
    (setf *lisp-executable-pathname* lisp-binary-path))
  (when xcvb-program
    (setf *xcvb-program* xcvb-program))
  (compute-xcvb-cache! cache)
  (compute-xcvb-workspace! workspace)
  ;; Initialize source-registry after lisp-implementation,
  ;; so we know what to do with magic require targets on sbcl.
  (initialize-xcvb-source-registry source-registry)
  (when use-target-lisp
    (flet ((m (x) (mapcar #'(lambda (s) (intern (string-upcase s) :keyword)) x)))
      (setf *target-added-features* (m define-feature))
      (setf *target-suppressed-features* (m undefine-feature))
      (when-bind (i) (intersection *target-added-features* *target-suppressed-features*)
        (errexit 2 "Can't both define and undefine feature~P ~
                    ~{~A~#[~; and ~:;, ~]~}" (length i) i)))
    (read-target-properties) ;; Gets information from target Lisp.
    (setf *use-cfasls* ;; Must be done after read-target-properties
          (and *use-cfasls* (not disable-cfasl)))
    (compute-object-cache! object-cache))
  (setf *use-base-image* (and *target-can-dump-image-p* use-base-image))
  (setf *use-master* master)
  (when master
    (ensure-tthsum-present))
  (when setup
    (let ((module (resolve-absolute-module-name setup)))
      (unless module
        (error "Cannot find setup module ~A" setup))
      (append1f *lisp-setup-dependencies* (fullname module))))
  (compute-install-paths!
   :install-prefix install-prefix
   :install-program install-program
   :install-configuration install-configuration
   :install-data install-data
   :install-library install-library
   :install-image install-image
   :install-lisp install-lisp)
  (ensure-required-xcvb-version required-xcvb-version))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun main (&rest arguments)
  (setf *arguments* arguments)
  (initialize-environment)
  (with-safe-io-syntax (:package :xcvb)
    (main* arguments)))

(defun main* (arguments)
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
		    (interpret-command arguments)
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
		    (format stream "Abort current computation and quit the process with process exit code "))
          (quit exit-code))
        (abort ()
          :report (lambda (stream)
                    (format stream "Abort current computation and quit the process with process exit code 111"))
          (quit 111))))))

(defun initialize-environment ()
  (xcvb-driver:setup-environment)
  ;;; This setting helps extract-target-properties.lisp. See there.
  #+sbcl (sb-posix:unsetenv "SBCL_HOME")

  ;;; This setting makes things sensible for run-program on CCL
  #+clozure (setf *error-output* ccl::*stderr* *trace-output* ccl::*stderr*)

  ;;; Determine the temporary directory
  (setf *temporary-directory*
	(or (getenv-absolute-directory "TMPDIR")
	    (when (os-windows-p) (getenv-absolute-directory "TEMP"))
	    (xd::default-temporary-directory)))

  ;;; In case debugging is involved, make things easier on the printer
  (setf *print-pretty* nil
        *print-readably* nil))

(defun cmdize* (args)
  (mapcar #'token-string args))

(defun cmdize (&rest args)
  (cmdize* args))

(defun cmd* (&rest args)
  "For debugging purposes, let the user try a command from the REPL"
  (restart-case
      (interpret-command (cmdize* args))
    (exit (&optional (exit-code 0))
      :report (lambda (stream)
		    ;; when invoked interactively exit-code is always 0
		    (format stream "Abort current computation and return exit code 0"))
      (return-from cmd* exit-code))
    (abort ()
      :report (lambda (stream)
                (format stream "Abort current computation and return exit code 111"))
      (return-from cmd* 111))))

(defun cmd (&rest args)
  "For debugging purposes, let the user try a command from the REPL, in a local context"
  (with-local-xcvb-vars ()
    (initialize-environment)
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

(defun prepare-image (&key version directory)
  (setf *xcvb-version* (or version (get-xcvb-version))
        *xcvb-lisp-directory* (ensure-directory-pathname (or directory (get-xcvb-directory))))
  (asdf:clear-configuration))
