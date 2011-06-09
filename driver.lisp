;;; XCVB driver. Load it in your Lisp image and build with XCVB.

#+xcvb
(module
 (:description "XCVB Driver"
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT" ;; MIT-style license. See LICENSE
  :build-depends-on nil))

;; #.(setf *load-verbose* () *load-print* () *compile-verbose* () *compile-print* ()) ;; Hush!

(cl:in-package :cl-user)

(defpackage :xcvb-driver
  (:nicknames :xcvbd)
  (:use :cl)
  (:export

   ;;; special variables shared with XCVB itself
   #:*lisp-implementation-type*
   #:*lisp-executable-pathname*
   #:*lisp-image-pathname*
   #:*lisp-implementation-directory*
   #:*lisp-flags*
   #:*features-defined* #:*features-undefined*
   #:*xcvb-verbosity*
   #:*lisp-allow-debugger*
   #:*object-directory*
   #:*tmp-directory-pathname*
   #:*use-base-image*

   ;;; special variables for XCVB master itself
   #:*disable-cfasls*
   #:*source-registry*
   #:*manifest*

   ;;; String utilities - copied from fare-utils
   ;;#:string-prefix-p
   ;;#:string-suffix-p
   ;;#:string-enclosed-p

   ;;; I/O utilities
   ;;#:with-output ; copied from fare-utils
   #:slurp-stream-string
   #:slurp-stream-lines
   #:copy-stream-to-stream
   #:copy-stream-to-stream-line-by-line
   #:read-many
   #:with-safe-io-syntax
   #:read-first-file-form
   #:with-temporary-file

   ;;; Escaping the command invocation madness
   #:escape-windows-token
   #:escape-windows-command
   #:escape-sh-token
   #:escape-sh-command
   #:escape-token
   #:escape-command

   ;;; run-program/foo
   #:run-program/process-output-stream
   #:run-program/read-output-lines
   #:run-program/read-output-string
   #:run-program/read-output-form
   #:run-program/read-output-forms
   #:run-program/echo-output

   ;; Magic strings
   #:+xcvb-slave-greeting+
   #:+xcvb-slave-farewell+

   ;;; Using an inferior XCVB
   #:build-and-load #:bnl

   ;;; Build-time variables
   #:*optimization-settings*
   #:*uninteresting-conditions* #:*fatal-conditions* #:*deferred-warnings*
   #:*goal* #:*stderr* #:*debugging*
   #:*post-image-restart* #:*entry-point*

   ;;; Environment support
   #:getenv #:emptyp #:getenvp #:setup-environment
   #:debugging #:with-profiling
   #:format! #:finish-outputs #:quit #:shell-boolean
   #:print-backtrace #:die #:bork #:with-coded-exit
   #:uninteresting-condition-p #:fatal-condition-p
   #:with-controlled-compiler-conditions #:with-controlled-loader-conditions
   #:with-xcvb-compilation-unit
   #:find-symbol* #:call #:eval-string #:load-string #:load-stream
   #:run #:do-run #:run-commands #:run-command
   #:asdf-symbol #:asdf-call
   #:resume #-ecl #:dump-image #+ecl #:create-bundle
   #:register-pathname-mapping #:register-pathname-mappings #:load-pathname-mappings
   #:pathname-mapping))

(in-package :xcvb-driver)

;;; Optimization settings
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *implementation-settings*
    `(;; These should ensure all tail calls are optimized, says jsnell:
      #+sbcl (sb-c::merge-tail-calls 3) #+sbcl (sb-c::insert-debug-catch 0)
      #+(or cmu scl) (ext:inhibit-warnings 3)))
  (defvar *optimization-settings*
    `((speed 2) (safety 3) (compilation-speed 0) (debug 2)
      ,@*implementation-settings*))
  (proclaim `(optimize ,@*optimization-settings*)))

;;; Initial implementation-dependent setup
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; otherwise ACL 5.0 may crap out on ASDF dependencies,
  ;; but even other implementations may have "fun" debugging.
  (setf *print-readably* nil)
  #+gcl ;;; If using GCL, do some safety checks
  (flet ((bork (&rest args)
           (apply #'format *error-output* args)
           (lisp:quit 42)))
    (when (or (< system::*gcl-major-version* 2)
              (and (= system::*gcl-major-version* 2)
                   (< system::*gcl-minor-version* 7)))
      (bork "GCL version ~D.~D < 2.7 not supported"
             system::*gcl-major-version* system::*gcl-minor-version*))
    (unless (member :ansi-cl *features*)
      (bork "XCVB only supports GCL in ANSI mode. Aborting.~%"))
    (setf compiler::*compiler-default-type* (pathname "")
          compiler::*lsp-ext* ""))
  #+cmu (setf ext:*gc-verbose* nil)
  #+clisp (setf custom:*source-file-types* nil custom:*compiled-file-types* nil)
  #+ecl (let ((*load-verbose* nil)) (require :cmp))
  (let* ((unix #+(or unix cygwin) t)
         (windows #+(and (or win32 windows mswindows mingw32) (not unix)) t))
    (cond
      ((and unix windows) (error "Your operating system is simultaneously Unix and Windows?~%~
Congratulations. Now fix XCVB for it assumes that's impossible"))
      (unix (pushnew :os-unix *features*))
      (windows (pushnew :os-windows *features*))
      (t (error "Congratulations for trying XCVB on an operating system~%~
that is neither Unix, nor Windows.~%Now you port it."))))
  (pushnew :xcvb-driver *features*))

;; Variables that define the current system
(defvar *post-image-restart* nil
  "a string containing forms to read and evaluate when the image is restarted,
but before the entry point is called.")
(defvar *entry-point* nil
  "a function with which to restart the dumped image when execution is resumed from it.")
(defvar *debugging* nil
  "boolean: should we enter the debugger on failure?")
(defvar *profiling* nil
  "boolean: should we compute and display the time spend in each command?")
(defvar *goal* nil
  "what is the name of the goal toward which we execute commands?")
(defvar *stderr* *error-output*
  "the original error output stream at startup")
(defvar *uninteresting-conditions*
  (append
   #+sbcl
   '(sb-c::simple-compiler-note
     "&OPTIONAL and &KEY found in the same lambda list: ~S"
     sb-int:package-at-variance
     sb-kernel:uninteresting-redefinition
     sb-kernel:undefined-alien-style-warning
     sb-ext:implicit-generic-function-warning
     sb-kernel:lexical-environment-too-complex
     "Couldn't grovel for ~A (unknown to the C compiler).")
   ;;#+ccl '(ccl:compiler-warning)
   '("No generic function ~S present when encountering macroexpansion of defmethod. Assuming it will be an instance of standard-generic-function.") ;; from closer2mop
   )
  "Conditions that may be skipped. type symbols, predicates or strings")
(defvar *fatal-conditions*
  '(serious-condition)
  "Conditions to be considered fatal during compilation.")
(defvar *deferred-warnings* ()
  "Warnings the handling of which is deferred until the end of the compilation unit")
(defvar *initial-random-state* (make-random-state nil)
  "initial random state to preserve determinism")

;;; Basic helpers
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-symbol* (name package-name &optional (error t))
    (let ((package (find-package (string package-name))))
      (if package
          (let ((symbol (find-symbol (string name) package)))
            (or symbol
                (when error
                  (error "There is no symbol ~A in package ~A" name package-name))))
          (when error
            (error "There is no package ~A" package-name))))))

(defun finish-outputs ()
  (dolist (s (list *stderr* *error-output* *standard-output* *trace-output*))
    (ignore-errors (finish-output s))))

(defun format! (stream format &rest args)
  (finish-outputs)
  (apply 'format stream format args)
  (finish-output stream))


;;; Setting up the environment from shell variables
#+mcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:define-entry-point (_getenv "getenv") ((name :string)) :string))
(defun getenv (x)
  (declare (ignorable x))
  #+(or abcl clisp xcl) (ext:getenv x)
  #+allegro (sys:getenv x)
  #+clozure (ccl:getenv x)
  #+(or cmu scl) (cdr (assoc x ext:*environment-list* :test #'string=))
  #+cormanlisp
  (let* ((buffer (ct:malloc 1))
         (cname (ct:lisp-string-to-c-string x))
         (needed-size (win:getenvironmentvariable cname buffer 0))
         (buffer1 (ct:malloc (1+ needed-size))))
    (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
               nil
               (ct:c-string-to-lisp-string buffer1))
      (ct:free buffer)
      (ct:free buffer1)))
  #+ecl (si:getenv x)
  #+gcl (system:getenv x)
  #+genera nil
  #+lispworks (lispworks:environment-variable x)
  #+mcl (ccl:with-cstrs ((name x))
          (let ((value (_getenv name)))
            (unless (ccl:%null-ptr-p value)
              (ccl:%get-cstring value))))
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl sbcl scl xcl)
  (error "~S is not supported on your implementation" 'getenv))

(defvar *previous-optimization-settings* nil)
(defun get-optimization-settings ()
  #+clozure
  `((speed . ,ccl::*nx-speed*)
    (space . ,ccl::*nx-space*)
    (safety . ,ccl::*nx-safety*)
    (debug . ,ccl::*nx-debug*)
    (compilation-speed . ,ccl::*nx-cspeed*))
  #+sbcl sb-c::*policy*
  #-(or clozure sbcl) nil)
(defun proclaim-optimization-settings ()
  (proclaim `(optimize ,@*optimization-settings*))
  (when *debugging*
    (let ((settings (get-optimization-settings)))
      (unless (equal *previous-optimization-settings* settings)
        (setf *previous-optimization-settings* settings)
        (format! *error-output* "~&Optimization settings: ~S~%" settings)))))


;;; These variables are shared with XCVB itself.
(defvar *lisp-implementation-type*
  ;; TODO: test on all OS and implementation platform combinations!
  (or
   #+abcl :abcl
   #+allegro :allegro
   #+clisp :clisp
   #+clozure :ccl
   #+cmu :cmucl
   #+cormanlisp :corman
   #+ecl :ecl
   #+gcl :gcl
   #+genera :genera
   #+lispworks-personal-edition :lispworks-personal
   #+lispworks :lispworks
   #+mcl :mcl
   #+sbcl :sbcl
   #+scl :scl
   #+xcl :xcl
   (error "Your Lisp implementation is not supported by the XCVB driver (yet). Please help."))
   "Type of Lisp implementation for the target system. A keyword.
  Default: same as XCVB itself.")

(defvar *lisp-executable-pathname* nil
  "Path to the Lisp implementation to use for the target system.
  NIL, or a string.
  Default: what's in your PATH.")

(defvar *lisp-image-pathname* nil
  "What path to a Lisp image do we need invoke the target Lisp with?
  Default: whatever's the default for your implementation.")

(defvar *lisp-implementation-directory*
  (or #+ccl (namestring (ccl::ccl-directory))
      #+gcl (namestring system::*system-directory*)
      #+sbcl (namestring (sb-int:sbcl-homedir-pathname)))
  "Where is the home directory for the Lisp implementation,
  in case we need it to (require ...) special features?
  Default: whatever's the default for your implementation.")

(defvar *lisp-flags* :default
  ;;; TODO: add support for overriding this feature at the command-line?
  "What options do we need invoke the target Lisp with?
A list of strings, or the keyword :DEFAULT.")

(defvar *features-defined* nil
  "What additional features to define in the target image")

(defvar *features-undefined* nil
  "What additional features to undefine in the target image")

(defvar *disable-cfasls* nil
  "Should we disable CFASL support when the target Lisp has it?")

(defvar *xcvb-verbosity* 5
  "Level of verbosity of XCVB:
  0 - silent except for emergency
  5 - usual warnings
  9 - plenty of debug info")

(defvar *lisp-allow-debugger* nil
  "Should we allow interactive debugging of failed build attempts?")

(defvar *object-directory* nil
  "where to store object files.
NIL: default to ~/.cache/xcvb/common-lisp/sbcl-1.0.47-x86/ or some such, see docs")

(defvar *tmp-directory-pathname*
  (pathname
   #+os-unix (format nil "~A/" (or (getenv "TMP") "/tmp"))
   #+os-windows (format nil "~A\\" (or (getenv "TEMP") (error "No temporary directory!"))))
  "pathname of directory where to store temporary files")

(defvar *use-base-image* t
  "Should we be using a base image for all builds?")

;;; These variables are specific to XCVB master.
(defvar *xcvb-binary* "xcvb"
  "Path to the XCVB binary (a string)")

(defvar *source-registry* nil
  "CL source registry specification. A sexp or string.
Will override the shell variable CL_SOURCE_REGISTRY when calling slaves.")

(defvar *xcvb-setup* nil
  "Lisp file to load to setup the target build system, if any")

(defvar *manifest* nil
  ;; Note that older versions are kept in the tail, documenting the command history,
  ;; without affecting the behavior of ASSOC on the alist.
  "an alist of the XCVB load commands executed in this image,
with associated pathnames and tthsums.")


;;; Debugging
(defun debugging (&optional (debug t))
  (setf *debugging* debug
        *load-verbose* debug
        *load-print* debug
        #+clisp custom:*compile-warnings* #+clisp debug
        *compile-verbose* debug
        *compile-print* debug
        *optimization-settings* '((speed 2) (safety 3) (compilation-speed 0) (debug 3)))
  (proclaim-optimization-settings)
  (cond
    (debug
     #+sbcl (sb-ext:enable-debugger)
     #+clisp (ext:set-global-handler nil nil))
    (t
     #+sbcl (sb-ext:disable-debugger)
     #+clisp (ext:set-global-handler 'error #'bork)))
  (values))

;;; Tweak implementation
(defun tweak-implementation ()
  #+sbcl
  (progn
    ;; add ample margin between GC's: 400 MiB
    (setf (sb-ext:bytes-consed-between-gcs) (* 400 1024 1024))
    ;; add ample margin for *next* GC: 200 MiB
    (incf (sb-alien:extern-alien "auto_gc_trigger" sb-alien:long) (* 200 1024 1024))
    #|(sb-ext:gc :full t)|#)
  #+ccl
  (progn
    (ccl::configure-egc 32768 65536 98304)
    (ccl::set-lisp-heap-gc-threshold (* 384 1024 1024))
    (ccl::use-lisp-heap-gc-threshold)
    #|(ccl:gc)|#)
  nil)


;;; environment
(defun emptyp (x)
  (or (null x) (and (vectorp x) (zerop (length x)))))
(defun getenvp (x)
  (not (emptyp (getenv x))))

(defun setup-environment ()
  (debugging (getenvp "XCVB_DEBUGGING"))
  (setf *profiling* (getenvp "XCVB_PROFILING"))
  (tweak-implementation))

;;; Profiling
(defun call-with-maybe-profiling (thunk what goal)
  (when *debugging*
    (format! *trace-output* "~&Now ~S~&" what))
  (if *profiling*
    (let* ((start-time (get-internal-real-time))
           (values (multiple-value-list (funcall thunk)))
           (end-time (get-internal-real-time))
           (duration (coerce (/ (- end-time start-time) internal-time-units-per-second) 'double-float)))
      (format! *trace-output* "~&~S~&" `(:profiling ,what :from ,goal :duration ,duration))
      (apply #'values values))
    (funcall thunk)))
(defmacro with-profiling (what &body body)
  `(call-with-maybe-profiling #'(lambda () ,@body) ,what *goal*))

;;; Exiting properly or im-
(defun quit (&optional (code 0) (finish-output t))
  "Quits from the Lisp world, with the given exit status if provided.
This is designed to abstract away the implementation specific quit forms."
  (when *debugging*
    (format! *stderr* "~&Quitting with code ~A~%" code))
  (when finish-output ;; essential, for ClozureCL, and for standard compliance.
    (finish-outputs))
  #+(or abcl xcl) (ext:quit :status code)
  #+allegro (excl:exit code :quiet t)
  #+clisp (ext:quit code)
  #+clozure (ccl:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+(or cmu scl) (unix:unix-exit code)
  #+ecl (si:quit code)
  #+gcl (lisp:quit code)
  #+lispworks (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
  #+sbcl (sb-unix:unix-exit code)
  #-(or abcl allegro clisp clozure cmu ecl gcl lispworks sbcl scl xcl)
  (error "xcvb driver: Quitting not implemented"))
(defun shell-boolean (x)
  (quit (if x 0 1)))
(defun print-backtrace (out)
  (declare (ignorable out))
  #+clozure (let ((*debug-io* out))
	      (ccl:print-call-history :count 100 :start-frame-number 1)
	      (finish-output out))
  #+sbcl
  (sb-debug:backtrace
   #.(if (find-symbol* "*VERBOSITY*" "SB-DEBUG" nil) :stream 'most-positive-fixnum)
   out))
(defun die (format &rest arguments)
  (format! *stderr* "~&")
  (apply #'format! *stderr* format arguments)
  (format! *stderr* "~&")
  (quit 99))
(defun bork (condition)
  (format! *stderr* "~&BORK:~%~A~%" condition)
  (cond
    (*debugging*
     (invoke-debugger condition))
    (t
     (print-backtrace *stderr*)
     (die "~A" condition))))
(defun call-with-coded-exit (thunk)
  (handler-bind ((error #'bork))
    (funcall thunk)
    (quit 0)))
(defmacro with-coded-exit (() &body body)
  `(call-with-coded-exit #'(lambda () ,@body)))


;;; Filtering conditions during the build.
(defun match-condition-p (x condition)
  (etypecase x
    (symbol (typep condition x))
    (function (funcall x condition))
    (string (and (typep condition 'simple-condition)
                 #+(or ccl cmu scl)
		 (slot-boundp condition
			      #+ccl 'ccl::format-control
			      #+(or cmu scl) 'conditions::format-control
			      #+sbcl 'sb-kernel:format-control)
                 (ignore-errors (equal (simple-condition-format-control condition) x))))))
(defun match-any-condition-p (condition conditions)
  (loop :for x :in conditions :thereis (match-condition-p x condition)))
(defun uninteresting-condition-p (condition)
  (match-any-condition-p condition *uninteresting-conditions*))
(defun fatal-condition-p (condition)
  (match-any-condition-p condition *fatal-conditions*))
(defun call-with-controlled-compiler-conditions (thunk)
  (handler-bind
      ((t
        #'(lambda (condition)
            ;; TODO: do something magic for undefined-function,
            ;; save all of aside, and reconcile in the end of the virtual compilation-unit.
            (cond
              ((uninteresting-condition-p condition)
               (muffle-warning condition))
              ((fatal-condition-p condition)
               (bork condition))))))
    (funcall thunk)))
(defmacro with-controlled-compiler-conditions (() &body body)
  `(call-with-controlled-compiler-conditions #'(lambda () ,@body)))
(defun call-with-controlled-loader-conditions (thunk)
  (let ((*uninteresting-conditions*
         (append
          #+sbcl '(sb-kernel:redefinition-with-defun
                   sb-kernel:redefinition-with-defgeneric
                   sb-kernel:redefinition-with-defmethod)
          #+clisp '(clos::simple-gf-replacing-method-warning)
          *uninteresting-conditions*)))
    (call-with-controlled-compiler-conditions thunk)))
(defmacro with-controlled-loader-conditions (() &body body)
  `(call-with-controlled-loader-conditions #'(lambda () ,@body)))
(defun save-forward-references (forward-references)
  #+sbcl
  (loop :for w :in sb-c::*undefined-warnings*
    :for kind = (sb-c::undefined-warning-kind w) ; :function :variable :type
    :for name = (sb-c::undefined-warning-name w)
    :for symbol = (cond
                    ((consp name)
                     (unless (eq kind :function)
                       (error "unrecognized warning ~S not a function?" w))
                     (ecase (car name)
                       ((setf)
                        (assert (and (consp (cdr name)) (null (cddr name))) ())
				  (setf kind :setf-function)
                        (second name))
                       ((sb-pcl::slot-accessor)
                        (assert (eq :global (second name)))
                        (assert (eq 'boundp (fourth name)))
                        (assert (null (nthcdr 4 name)))
                        (setf kind :sb-pcl-global-boundp-slot-accessor)
                        (third name))))
                    (t
                     (assert (member kind '(:function :variable :type)) ())
                     name))
    :for symbol-name = (symbol-name symbol)
    :for package-name = (package-name (symbol-package symbol))
    :collect `(:undefined ,symbol-name ,package-name ,kind) :into undefined-warnings
    :finally (setf *deferred-warnings* undefined-warnings
                   sb-c::*undefined-warnings* nil))
  (when forward-references
    (with-open-file (s forward-references :direction :output :if-exists :supersede)
      (write *deferred-warnings* :stream s :pretty t :readably t)
      (terpri s))))
(defun call-with-xcvb-compilation-unit (thunk &key forward-references)
  (with-compilation-unit (:override t)
    (let ((*deferred-warnings* ())
          #+sbcl (sb-c::*undefined-warnings* nil))
      (multiple-value-prog1
          (with-controlled-compiler-conditions ()
            (funcall thunk))
        (save-forward-references forward-references)))))
(defmacro with-xcvb-compilation-unit ((&key forward-references) &body body)
  `(call-with-xcvb-compilation-unit #'(lambda () ,@body) :forward-references ,forward-references))


;;; Interpreting commands from the xcvb-driver-command DSL.
(defun function-for-command (designator)
  (fdefinition (find-symbol* designator :xcvb-driver)))
(defun run-command (command)
  (proclaim-optimization-settings)
  (multiple-value-bind (head args)
      (etypecase command
        (symbol (values command nil))
        (cons (values (car command) (cdr command))))
    (apply (function-for-command head) args)))
(defun run-commands (commands)
  (map () #'run-command commands))
(defun do-run (commands)
  (let ((*stderr* *error-output*))
    (setup-environment)
    (run-commands commands)))
(defmacro run (&rest commands)
  `(with-coded-exit ()
    (do-run ',commands)))


;;; Simple commands
(defun do-load (x)
  (with-controlled-loader-conditions ()
    (load x :verbose (>= *xcvb-verbosity* 8) :print (>= *xcvb-verbosity* 9))))
(defun load-file (x)
  (with-profiling `(:load-file ,x)
    (unless (do-load x)
      (error "Failed to load ~A" (list x)))))
(defun call (package symbol &rest args)
  (apply (find-symbol* symbol package) args))
(defun eval-string (string)
  (eval (read-from-string string)))
(defun cl-require (x)
  (with-profiling `(:require ,x)
    (require x)))

(defun load-stream (&optional (s *standard-input*))
  ;; GCL 2.6 can't load from a string-input-stream
  ;; ClozureCL 1.6 can only load from file input
  ;; Allegro 5, I don't remember but it must have been broken when I tested.
  #+(or gcl-pre2.7 clozure allegro)
  (do ((eof '#:eof) (x t (read s nil eof))) ((eq x eof)) (eval x))
  #-(or gcl-pre2.7 clozure allegro)
  (do-load s))
(defun load-string (string)
  (with-input-from-string (s string) (load-stream s)))

;;; ASDF support
(defun asdf-symbol (x)
  (find-symbol* x :asdf))
(defun asdf-call (x &rest args)
  (apply 'call :asdf x args))
(defun load-asdf (x &key parallel (verbose *compile-verbose*)) ;; parallel loading requires POIU
  (with-profiling `(:asdf ,x)
    (asdf-call :operate (asdf-symbol (if parallel :parallel-load-op :load-op)) x
               :verbose verbose)))
(defun register-asdf-directory (x)
  (pushnew x (symbol-value (asdf-symbol :*central-registry*))))
(defun asdf-system-up-to-date-p (operation-class system &rest args)
  "Takes a name of an asdf system (or the system itself) and a asdf operation
  and returns a boolean indicating whether or not anything needs to be done
  in order to perform the given operation on the given system.
  This returns whether or not the operation has already been performed,
  and none of the source files in the system have changed since then"
  (progv
      (list (asdf-symbol :*verbose-out*))
      (list (if (getf args :verbose t) *trace-output* (make-broadcast-stream)))
    (let* ((op (apply #'make-instance operation-class
                      :original-initargs args args))
           (system (if (typep system (asdf-symbol :component))
                       system
                       (asdf-call :find-system system)))
           (steps (asdf-call :traverse op system)))
      (null steps))))
(defun asdf-system-loaded-up-to-date-p (system)
  (asdf-system-up-to-date-p (asdf-symbol :load-op) system))
(defun asdf-systems-up-to-date-p (systems)
  "Takes a list of names of asdf systems, and
  exits lisp with a status code indicating
  whether or not all of those systems were up-to-date or not."
  (every #'asdf-system-loaded-up-to-date-p systems))
(defun asdf-systems-up-to-date (&rest systems)
  "Are all the loaded systems up to date?"
  (with-coded-exit ()
    (shell-boolean (asdf-systems-up-to-date-p systems))))

;;; Actually compiling
(defmacro with-determinism (goal &body body)
  `(call-with-determinism ,goal #'(lambda () ,@body)))

(defun seed-random-state (seed) ; seed is a integer
  (declare (ignorable seed))
  #+sbcl (sb-ext:seed-random-state seed)
  #+ccl (flet ((get-bits (&aux bits)
                 (multiple-value-setq (seed bits) (floor seed ccl::mrg31k3p-limit))
                 bits))
          (multiple-value-bind (x0 x1 x2 x3 x4 x5)
              (apply 'values (loop :repeat 6 :collect (get-bits)))
            (when (zerop (logior x0 x1 x2))
              (setf x0 (logior (get-bits) 1)))
            (when (zerop (logior x3 x4 x5))
              (setf x3 (logior (get-bits) 1)))
            (ccl::initialize-mrg31k3p-state x0 x1 x2 x3 x4 x5)))
  #-(or sbcl ccl) (make-random-state *initial-random-state*))

(defun call-with-determinism (seed thunk)
  ;;; The seed is an arbitrary object from (a hash of) which we initialize
  ;;; all sources of randomness and non-determinism in the file being compiled.
  ;;;
  ;;; We typically use as a seed the goal as opposed to the tthsum of some contents
  ;;; to give a greater chance to trivial modifications of the source text (e.g.
  ;;; comments and whitespace changes) to be without effect on the compilation output.
  ;;; We possibly should be using the tthsum instead of a sxhash,
  ;;; as provided by the master process.
  ;;;
  ;;; In SBCL, we'll also need to somehow disable the start-time slot of the
  ;;; (def!struct (source-info ...)) from main/compiler.lisp (package SB-C).
  (let* ((hash (sxhash seed))
         (*gensym-counter* (* hash 10000))
         #+sbcl (sb-impl::*gentemp-counter* (* hash 10000))
         ;;; SBCL will hopefully export a better mechanism soon. See:
         ;;; https://bugs.launchpad.net/sbcl/+bug/310116
         (*random-state* (seed-random-state hash)))
    (funcall thunk)))

(defun do-compile-lisp (dependencies source fasl
                        &key #+sbcl cfasl #+ecl lisp-object)
  (let ((*goal* `(:compile-lisp ,source))
        (*default-pathname-defaults* (truename *default-pathname-defaults*)))
    #+ecl (require :cmp)
    (multiple-value-bind (output-truename warnings-p failure-p)
        (with-profiling `(:preparing-and-compiling ,source)
          (with-xcvb-compilation-unit ()
            (run-commands dependencies)
            (with-profiling `(:compiling ,source)
              (with-determinism `(:compiling ,source)
                (multiple-value-prog1
                    (apply #'compile-file source
                           :output-file (merge-pathnames (or #+ecl lisp-object fasl))
                           (append
                            #+sbcl (when cfasl `(:emit-cfasl ,(merge-pathnames cfasl)))
                            #+ecl (when lisp-object '(:system-p t))))
                  #+ecl
                  (when lisp-object
                    (or (call :c :build-fasl
                              (merge-pathnames fasl)
                              :lisp-files (list (merge-pathnames lisp-object)))
                        (die "Failed to build ~S from ~S" fasl lisp-object))))))))
      (declare (ignorable warnings-p failure-p))
      (unless output-truename
        (die "Compilation Failed for ~A, no fasl created" source))
      #-clisp
      (when failure-p
        (die "Compilation Failed for ~A" source))
      #-(or clisp ecl)
      (when warnings-p
        (die "Compilation Warned for ~A" source))))
  (values))

(defun compile-lisp (spec &rest dependencies)
  (apply 'do-compile-lisp dependencies spec))

(defparameter *arguments* nil
  "Command-line arguments")

(defparameter *dumped* nil
  "Is this a dumped image? As a standalone executable?")

(defun raw-command-line-arguments ()
  #+abcl (cdr ext:*command-line-argument-list*) ; abcl adds a "--" to our "--"
  #+allegro (sys:command-line-arguments) ; default: :application t
  #+clisp (coerce (ext:argv) 'list)
  #+clozure (ccl::command-line-arguments)
  #+(or cmu scl) extensions:*command-line-strings*
  #+ecl (loop :for i :from 0 :below (si:argc) :collect (si:argv i))
  #+gcl si:*command-args*
  #+lispworks sys:*line-arguments-list*
  #+sbcl sb-ext:*posix-argv*
  #+xcl system:*argv*
  #-(or abcl allegro clisp clozure cmu ecl gcl lispworks sbcl scl xcl)
  (error "raw-command-line-arguments not implemented yet"))

(defun command-line-arguments ()
  (let* ((raw (raw-command-line-arguments))
	 (cooked
	  #+(or sbcl allegro) raw
	  #-(or sbcl allegro)
	  (if (eq *dumped* :executable)
	      raw
	      (member "--" raw :test 'string-equal))))
    (cdr cooked)))

;;; Dumping and resuming an image
(defun do-resume (&key (post-image-restart *post-image-restart*) (entry-point *entry-point*))
  (with-standard-io-syntax
    (when post-image-restart (load-string post-image-restart)))
  (with-coded-exit ()
    (when entry-point
      (let ((ret (apply entry-point *arguments*)))
	(if (typep ret 'integer)
	    (quit ret)
	    (quit 99))))))

(defun resume ()
  (setf *arguments* (command-line-arguments))
  (do-resume))

(defun read-function (string)
  (eval `(function ,(read-from-string string))))

#-ecl
(defun dump-image (filename &key output-name executable pre-image-dump post-image-restart entry-point package)
  (declare (ignorable filename output-name executable pre-image-dump post-image-restart entry-point))
  (setf *dumped* (if executable :executable t))
  (setf *package* (find-package (or package :cl-user)))
  (with-standard-io-syntax
    (when pre-image-dump (load-string pre-image-dump))
    (setf *entry-point* (when entry-point (read-function entry-point)))
    (when post-image-restart (setf *post-image-restart* post-image-restart)))
  #-(or clisp clozure cmu lispworks sbcl)
  (when executable
    (error "Dumping an executable is not supported on this implementation! Aborting."))
  #+allegro
  (progn
    (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) ; :new 5000000
    (excl:dumplisp :name filename :suppress-allegro-cl-banner t))
  #+clisp
  (apply #'ext:saveinitmem filename
   :quiet t
   :start-package *package*
   :keep-global-handlers nil
   :executable (if executable 0 t) ;--- requires clisp 2.48 or later, still catches --clisp-x
   (when executable
     (list
      :norc t
      :script nil
      :init-function #'resume
      ;; :parse-options nil ;--- requires a non-standard patch to clisp.
      )))
  #+clozure
  (ccl:save-application filename :prepend-kernel t
                        :toplevel-function (when executable #'resume))
  #+(or cmu scl)
  (progn
   (ext:gc :full t)
   (setf ext:*batch-mode* nil)
   (setf ext::*gc-run-time* 0)
   (apply 'ext:save-lisp filename #+cmu :executable #+cmu t
          (when executable '(:init-function resume :process-command-line nil))))
  #+gcl
  (progn
   (si::set-hole-size 500) (si::gbc nil) (si::sgc-on t)
   (si::save-system filename))
  #+lispworks
  (if executable
      (lispworks:deliver 'resume filename 0 :interface nil)
      (hcl:save-image filename :environment nil))
  #+sbcl
  (progn
    ;;(sb-pcl::precompile-random-code-segments) ;--- it is ugly slow at compile-time (!) when the initial core is a big CLOS program. If you want it, do it yourself
   (setf sb-ext::*gc-run-time* 0)
   (apply 'sb-ext:save-lisp-and-die filename
    :executable t ;--- always include the runtime that goes with the core
    (when executable (list :toplevel #'resume :save-runtime-options t)))) ;--- only save runtime-options for standalone executables
  #-(or allegro clisp clozure cmu gcl lispworks sbcl scl)
  (die "Can't dump ~S: xcvb-driver doesn't support image dumping with this Lisp implementation.~%" filename))

;;; Actually creating images
#-ecl
(defun do-create-image (image dependencies &rest flags)
  (let ((*goal* `(create-image ,image))
        #+sbcl (*uninteresting-conditions*
                (cons "undefined ~(~A~): ~S" *uninteresting-conditions*)))
    (with-controlled-compiler-conditions ()
      (run-commands dependencies))
    (apply #'dump-image image flags)))

(defun read-first-form (pn)
  (with-open-file (s pn :direction :input :if-does-not-exist :error)
    (read s)))

#+ecl ;; wholly untested and probably buggy.
(defun do-create-image (image dependencies &key
			executable output-name pre-image-dump post-image-restart entry-point)
  (do-create-bundle image dependencies
		    :kind (if executable :program :shared-library)
                    :output-name output-name
                    :pre-image-dump pre-image-dump
		    :post-image-restart post-image-restart
		    :entry-point entry-point))

#+ecl
(defun do-create-bundle (bundle dependencies
			 &rest keys
			 &key kind output-name pre-image-dump post-image-restart entry-point)
  (let ((*goal* `(create-bundle ,bundle ,dependencies ,@keys))
	(first-dep (car dependencies)))
    (require :cmp)
    (multiple-value-bind (object-files manifest)
        (case (first first-dep)
          ((:load-manifest)
           (assert (null (rest dependencies)))
           (let ((manifest (read-first-form (second first-dep))))
             (values
              (loop :for l :in manifest :collect
                (destructuring-bind (&key command parent pathname
                                     tthsum source-pathname source-tthsum) l
                  (declare (ignore tthsum source-pathname source-tthsum))
                  (assert (eq (car command) :load-file))
                  pathname))
              manifest)))
          (:load-file
           (loop :for l :in dependencies :collect
           (destructuring-bind (link-file pathname) l
             (assert (eq link-file :load-file))
             pathname)))
          (t
           (assert (null dependencies))))
      (c::builder
       kind (parse-namestring bundle)
       :lisp-files object-files
       :init-name (c::compute-init-name (or output-name bundle) :kind kind)
       :epilogue-code
       (when (eq kind :program)
         `(progn
            (setf xcvb-driver:*manifest*
                  ',(reverse manifest))
            ,(when pre-image-dump
                   `(load-string ,pre-image-dump))
            (setf *entry-point* ,(when entry-point `(read-function ,entry-point)))
            (setf *post-image-restart* ,post-image-restart)
            (resume))))))) ;; default behavior would be (si::top-level)

#+ecl
(defun create-bundle (spec &rest dependencies)
  (destructuring-bind (bundle &rest keys) spec
    (apply 'do-create-bundle bundle dependencies keys)))

(defun create-image (spec &rest dependencies)
  (destructuring-bind (image &rest keys) spec
    (apply 'do-create-image image dependencies keys)))

(defvar *pathname-mappings* (make-hash-table :test 'equal)
  "Mappings from xcvb fullname to pathname")

(defun pathname-directory-pathname (pn)
  (make-pathname :name nil :type nil :version nil :defaults pn))
(defun register-pathname-mapping (&key name path #|logical|#)
  ;; should we add a logical pathname translation?
  (setf (gethash name *pathname-mappings*) (truename path))
  (values))
(defun register-pathname-mappings (mappings &key (defaults *load-truename*))
  (let ((*default-pathname-defaults*
         (or (and defaults (truename (pathname-directory-pathname defaults)))
             *default-pathname-defaults*)))
    (dolist (m mappings)
      (apply 'register-pathname-mapping m))))
(defun pathname-mapping (name)
  (gethash name *pathname-mappings*))
(defun load-pathname-mappings (file)
  (let ((tn (truename file)))
    (register-pathname-mappings (read-first-form tn) :defaults tn)))

(defun process-cffi-grovel-file (input c exe output &key cc-flags)
  (flet ((f (x) (namestring (merge-pathnames x))))
    (let* ((input (f input))
           (c (f c))
           (exe (f exe))
           (output (f output))
           (*default-pathname-defaults* (pathname-directory-pathname exe)))
      (progv (list (find-symbol* :*cc-flags* :cffi-grovel)) (list cc-flags)
        (call :cffi-grovel :generate-c-file input c)
        (call :cffi-grovel :cc-compile-and-link c exe)
        (call :cffi-grovel :invoke exe output)))))

(defun process-cffi-wrapper-file (input c so output &key cc-flags)
  (declare (ignore output)); see below
  (flet ((f (x) (namestring (merge-pathnames x))))
    (let* ((input (f input))
           (c (f c))
           (so (f so))
           ;;(output (f output))
           (*default-pathname-defaults* (pathname-directory-pathname so)))
      (progv (list (find-symbol* :*cc-flags* :cffi-grovel)) (list cc-flags)
        (with-standard-io-syntax
          (multiple-value-bind (c-file lisp-forms)
              (call :cffi-grovel :generate-c-lib-file input c)
            (declare (ignore c-file))
            (call :cffi-grovel :cc-compile-and-link c so :library t)
            (values (call :cffi-grovel :generate-bindings-file
                          c so lisp-forms c)
                    ;; currently use C instead of OUTPUT, due to output locations.
                    ;; ugly, but generate-bindings-file already adds .grovel-tmp.lisp
                    ;; to the output name, so we reuse the c name here. Sigh.
                    so)))))))

;;; Magic strings. Do not change. Constants, except we can't portably use defconstant here.
(defvar +xcvb-slave-greeting+ #.(format nil "XCVB-SLAVE~%"))
(defvar +xcvb-slave-farewell+ #.(format nil "~%Your desires are my orders~%"))

;;; String utilities
(defun string-prefix-p (prefix string)
  (let* ((x (string prefix))
         (y (string string))
         (lx (length x))
         (ly (length y)))
    (and (<= lx ly) (string= x y :end2 lx))))

(defun string-suffix-p (string suffix)
  (let* ((x (string string))
         (y (string suffix))
         (lx (length x))
         (ly (length y)))
    (and (<= ly lx) (string= x y :start1 (- lx ly)))))

(defun string-enclosed-p (prefix string suffix)
  (and (string-prefix-p prefix string)
       (string-suffix-p string suffix)))

;;; I/O utilities
(defgeneric call-with-output (x thunk)
  (:documentation ;; from fare-utils
   "Calls FUN with an actual stream argument, behaving like FORMAT with respect to stream'ing:
If OBJ is a stream, use it as the stream.
If OBJ is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OBJ is T, use *STANDARD-OUTPUT* as the stream.
If OBJ is a string with a fill-pointer, use it as a string-output-stream.
Otherwise, signal an error.")
  (:method ((x null) thunk)
    (declare (ignorable x))
    (with-output-to-string (s) (funcall thunk s)))
  (:method ((x (eql t)) thunk)
    (declare (ignorable x))
    (funcall thunk *standard-output*) nil)
  #-genera
  (:method ((x stream) thunk)
    (funcall thunk x) nil)
  (:method ((x string) thunk)
    (assert (fill-pointer x))
    (with-output-to-string (s x) (funcall thunk s)))
  (:method (x thunk)
    (declare (ignorable thunk))
    (cond
      #+genera
      ((typep x 'stream) (funcall thunk x) nil)
      (t (error "not a valid stream designator ~S" x)))))

(defmacro with-output ((x &optional (value x)) &body body)
  `(call-with-output ,value #'(lambda (,x) ,@body)))

(defun copy-stream-to-stream (input output &key (element-type 'character))
  (loop :with length = 8192
    :for buffer = (make-array length :element-type element-type)
    :for end = (read-sequence buffer input)
    :until (zerop end)
    :do (write-sequence buffer output :end end)
    :do (when (< end length) (return))))

(defun copy-stream-to-stream-line-by-line (input output)
  (loop :for (line eof) = (multiple-value-list (read-line input nil nil))
    :while line
    :do (progn
          (princ line output)
          (unless eof (terpri output))
          (finish-output output)
          (when eof (return)))))

(defun slurp-stream-string (input)
  (with-output-to-string (output)
    (copy-stream-to-stream input output :element-type 'character)))

(defun slurp-stream-lines (input)
  (loop :for l = (read-line input nil nil)
    :while l :collect l))

(defun read-many (s)
  (loop :with eof = '#:eof
    :for form = (read s nil eof)
    :until (eq form eof)
    :collect form))

(defmacro with-safe-io-syntax ((&key (package :cl)) &body body)
  `(call-with-safe-io-syntax (lambda () ,@body) :package ,package))
(defun call-with-safe-io-syntax (thunk &key (package :cl))
  (with-standard-io-syntax ()
    (let ((*package* (find-package package))
          (*print-readably* t)
          (*print-escape* t)
	  (*read-eval* nil))
      (funcall thunk))))

(defun read-first-file-form (filepath &key (package :cl))
  "Reads the first form from the top of a file"
  (with-safe-io-syntax (:package package)
    (with-open-file (in filepath)
      (read in nil nil))))

(defun requires-escaping-p (token &key good-chars bad-chars)
  (some
   (cond
     ((functionp good-chars)
      (complement good-chars))
     ((functionp bad-chars)
      bad-chars)
     ((and good-chars (typep good-chars 'sequence))
      (lambda (c) (not (find c good-chars))))
     ((and bad-chars (typep bad-chars 'sequence))
      (lambda (c) (find c bad-chars)))
     (t (error "requires-escaping-p: no good-char criterion")))
   token))

(defun escape-token (token &key stream quote good-chars bad-chars escaper)
  (cond
    ((requires-escaping-p token :good-chars good-chars :bad-chars bad-chars)
     (with-output (stream)
       (apply escaper token stream (when quote `(:quote ,quote)))))
    ((null stream)
     token)
    (t
     (with-output (stream)
       (princ token stream)))))

(defun escape-command (command s &optional
                       (escaper #+os-unix 'escape-sh-token
                                #+os-windows 'escape-windows-token))
  (with-output (s)
    (loop :for first = t :then nil :for token :in command :do
      (unless first (princ #\space s))
      (funcall escaper token s))))

(defun escape-windows-token-within-double-quotes (x &optional s)
  (labels ((issue (c) (princ c s))
           (issue-backslash (n) (loop :repeat n :do (issue #\\))))
    (loop
      :initially (issue #\") :finally (issue #\")
      :with l = (length x) :with i = 0
      :for i+1 = (1+ i) :while (< i l) :do
      (case (char x i)
        ((#\") (issue-backslash 1) (issue #\") (incf i))
        ((#\\)
         (let* ((j (and (< i+1 l) (position-if-not (lambda (c) (eql c #\\)) x :start i+1)))
                (n (- (or j l) i)))
           (cond
             ((null j)
              (issue-backslash (* 2 n)) (setf i l))
             ((and (< j l) (eql (char x j) #\"))
              (issue-backslash (1+ (* 2 n))) (issue #\") (setf i (1+ j)))
             (t
              (issue-backslash n) (setf i j)))))
        (otherwise
         (issue (char x i)) (incf i))))))

(defun escape-windows-token (token &optional s)
  (escape-token token :stream s :bad-chars #(#\space #\tab #\") :quote nil
                :escaper 'escape-windows-token-within-double-quotes))

(defun escape-windows-command (command &optional s)
    ;; encode a list of arguments into a string suitable for parsing by CommandLineToArgv
    ;; http://msdn.microsoft.com/en-us/library/bb776391(v=vs.85).aspx
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft(v=vs.85).aspx
  (etypecase command
    (string command)
    (list (escape-command command s 'escape-windows-token))))

(defun escape-sh-token-within-double-quotes (x s &key (quote t))
  (when quote (princ #\" s))
  (loop :for c :across x :do
    (when (find c "$`\\\"") (princ #\\ s))
    (princ c s))
  (when quote (princ #\" s)))

(defun escape-sh-token (token &optional s)
  (escape-token token :stream s :quote #\" :good-chars
                #'(lambda (x) (or (alphanumericp x) (find x "+-_.,%@:/")))
                :escaper 'escape-sh-token-within-double-quotes))

(defun escape-sh-command (command &optional s)
  (escape-command command s 'escape-sh-token))

(defun call-with-temporary-file (thunk &key
                                 prefix
                                 keep
                                 (direction :io)
                                 (element-type (or #+(or abcl xcl) 'character :default))
                                 (external-format :default))
  (check-type direction (member :output :io))
  (loop
    :with prefix = (or prefix (format nil "~Axm" *tmp-directory-pathname*))
    :for counter :from (random (ash 1 32))
    :for pathname = (pathname (format nil "~A~36R" prefix counter)) :do
     ;; TODO: on Unix, do something about umask
     ;; TODO: on Unix, audit the code so we make sure it uses O_CREAT|O_EXCL
     ;; TODO: on Unix, use CFFI and mkstemp -- but the master is precisely meant to not depend on CFFI or on anything! Grrrr.
    (with-open-file (stream pathname
                            :direction direction
                            :element-type element-type :external-format external-format
                            :if-exists nil :if-does-not-exist :create)
      (when stream
        (return
          (if keep
              (funcall thunk stream pathname)
              (unwind-protect
                   (funcall thunk stream pathname)
                (ignore-errors (delete-file pathname)))))))))

(defmacro with-temporary-file ((&key (stream (gensym "STREAM") streamp)
                                (pathname (gensym "PATHNAME") pathnamep)
                                prefix keep element-type external-format)
                               &body body)
  (check-type stream symbol)
  (check-type pathname symbol)
  `(flet ((think (,stream ,pathname)
            ,@(unless pathnamep `((declare (ignore ,pathname))))
            ,@(unless streamp `((when ,stream (close ,stream))))
            ,@body))
     #-gcl (declare (dynamic-extent #'think))
     (call-with-temporary-file
      #'think
      ,@(when prefix `(:prefix ,prefix))
      ,@(when keep `(:keep ,keep))
      ,@(when element-type `(:element-type ,element-type))
      ,@(when external-format `(:external-format external-format)))))

(defun escape-shell-command (command &optional stream)
  (#+os-unix escape-sh-command
   #+os-windows escape-windows-command
   command stream))

;;; Simple variant of run-program with no input, and capturing output
;;; On some implementations, may output to a temporary file...
(defun run-program/process-output-stream (command output-processor
                                          &key ignore-error-status force-shell
                                          (element-type (or #+(or abcl xcl) 'character :default))
                                          (external-format :default))
  (declare (ignorable ignore-error-status element-type external-format))
  (let* ((p (find-package :quux-iolib))
         (r 'run-program/process-output-stream)
         (s (and p (find-symbol (string r) p))))
    (when s (return-from run-program/process-output-stream
              (funcall s command output-processor :ignore-error-status ignore-error-status))))
  #-(or abcl allegro clisp clozure cmu ecl gcl lispworks sbcl scl xcl)
  (error "RUN-PROGRAM/PROCESS-OUTPUT-STREAM not implemented for this Lisp")
  (labels (#+(or allegro clisp clozure cmu ecl
                 (and lispworks os-unix) sbcl scl)
           (run-program (command &key pipe)
             "runs the specified command (a list of program and arguments).
              If using a pipe, returns two values: process and stream
              If not using a pipe, returns one values: the process result"
             (let* ((command
                     (etypecase command
                       #+os-unix (string `("/bin/sh" "-c" ,command))
                       #+os-unix (list command)
                       #+os-windows
                       (string
                        #+(or allegro clozure) command ;; (format nil "cmd /c ~A" command)
                        #-allegro `("cmd" "/c" ,command))
                       #+os-windows
                       (list
                        #+(or allegro clozure) (escape-windows-command command)
                        #-allegro command)))
                    ;; CCL on windows requires some magic until they fix
                    ;; http://trac.clozure.com/ccl/ticket/858
                    #+(and clozure os-windows) (command (list command))
                    (process*
                     (multiple-value-list
                      #+allegro
                      (excl:run-shell-command
                       #+os-unix (coerce (cons (first command) command) 'vector)
                       #+os-windows command
                       :input nil :output (and pipe :stream) :wait (not pipe))
                      #+lispworks
                      (system:run-shell-command
                       (cons "/usr/bin/env" command) ; lispworks wants a full path.
                       :input nil :output (and pipe :stream)
                       :wait (not pipe) :save-exit-status (and pipe t))
                      #-(or allegro lispworks)
                      (#+(or clisp cmu ecl scl) ext:run-program
                       #+clozure ccl:run-program
                       #+sbcl sb-ext:run-program
                       (car command) #+clisp :arguments (cdr command)
                       :input nil :output (and pipe :stream) :wait (not pipe)
                       . #.(append
                            #+(or clozure cmu ecl sbcl scl) '(:error t)
                            #+sbcl '(:search t
                                     :external-format external-format)))))
                    (process
                     #+(or allegro lispworks ecl) (third process*)
                     #-(or allegro lispworks ecl) (first process*))
                    (stream
                     #+(or allegro lispworks ecl) (first process*)
                     #+clisp process
                     #+clozure (ccl::external-process-output process)
                     #+(or cmu scl) (ext:process-output process)
                     #+sbcl (sb-ext:process-output process)))
               (if pipe
                   (values process stream)
                   #-allegro (process-result process)
                   #+allegro (check-result (first process*)))))
           #-(or abcl cormanlisp gcl (and lispworks os-windows) xcl)
           (process-result (process)
             (declare (ignorable process))
             ;; 1- wait
             #+(and clozure os-unix) (ccl::external-process-wait process)
             #+(or cmu scl) (ext:process-wait process)
             #+(and ecl os-unix) (ext:external-process-wait process)
             #+sbcl (sb-ext:process-wait process)
             ;; 2- extract result
             #+allegro (sys:reap-os-subprocess :pid process :wait t)
             #+clozure (nth-value 1 (ccl:external-process-status process))
             #+(or cmu scl) (ext:process-exit-code process)
             #+ecl (nth-value 1 (ext:external-process-status process))
             #+lispworks (system:pid-exit-status process :wait t)
             #+sbcl (sb-ext:process-exit-code process))
           (check-result (return-code)
             (unless (or ignore-error-status
                         #+clisp t
                         (zerop return-code))
               (cerror "ignore error code~*~*"
                       "Process ~S exited with error code ~D"
                       command return-code)))
           (system (command)
             #+(or abcl xcl) (ext:run-shell-command command)
             #+allegro
             (excl:run-shell-command command :input nil :output nil :wait t)
             #+(or clisp clozure cmu (and lispworks os-unix) sbcl scl)
             (run-program command :pipe nil)
             #+ecl (ext:system command)
             #+cormanlisp (win32:system command)
             #+gcl (lisp:system command)
             #+(and lispworks os-windows)
             (system:call-system-showing-output
              command :show-cmd nil :prefix "" :output-stream nil))
           (use-run-program ()
             #-(or abcl cormanlisp gcl (and lispworks os-windows) xcl)
             (multiple-value-bind (process stream)
                 (run-program command :pipe t)
               (unwind-protect
                    (funcall output-processor stream)
                 (when stream (close stream))
                 (check-result (process-result process)))))
           (use-system ()
             (with-temporary-file (:pathname tmp)
               (let* ((command-string
                       (format nil "~A > ~A"
                               (etypecase command
                                 (string command)
                                 (list (escape-shell-command command)))
                               tmp)))
                 (check-result (system command-string))
                 (with-open-file (stream tmp
                                         :direction :input :if-does-not-exist :error
                                         :element-type element-type
                                         :external-format external-format)
                   (funcall output-processor stream))))))
    (if (and (not force-shell)
             #+(or abcl cormanlisp gcl (and lispworks os-windows) xcl) nil)
        (use-run-program)
        (use-system))))

(defun run-program/read-output-lines (command &rest keys)
  (apply 'run-program/process-output-stream command
         'slurp-stream-lines keys))

(defun run-program/read-output-string (command &rest keys)
  (apply 'run-program/process-output-stream command
         'slurp-stream-string keys))

(defun run-program/read-output-form (command &rest keys)
  (apply 'run-program/process-output-stream command
         'read keys))

(defun run-program/read-output-forms (command &rest keys)
  (apply 'run-program/process-output-stream command
         'read-many keys))

(defun run-program/echo-output (command &key prefix (stream t) ignore-error-status)
  (run-program/process-output-stream
   command #'(lambda (s) (loop :for line = (read-line s nil nil) :while line :do
                           (format stream "~@[~A~]~A~&" prefix line) (force-output stream)))
   :ignore-error-status ignore-error-status))

;;; Maintaining memory of which grains have been loaded in the current image.
;; TODO: fix brokenness. We need to distinguish
;; 1- either a grain or a virtual command that we issue, e.g. (:load-file (:fasl "/foo/bar"))
;; 2- the actual thing that the driver runs, e.g. (:load-file "/path/to/foo/bar.fasl")
;; The mapping can be done at one place or the other, but currently there's a big confusion!
(defun process-manifest-entry (&rest entry &key command pathname tthsum &allow-other-keys)
  ;; also source source-tthsum source-pathname
  (unless (and tthsum
               (equal tthsum
                      (getf (find command *manifest* :test #'equal
                                  :key (lambda (x) (getf x :command)))
                            :tthsum))
               (progn
                 (when (>= *xcvb-verbosity* 8)
                   (format! *error-output* "~&Skipping XCVB command ~S ~@[from already loaded file ~S (tthsum: ~A)~]~%"
                            command pathname tthsum))
                 t))
    (when (>= *xcvb-verbosity* 7)
      (format! *error-output* "~&Loading XCVB grain ~S~@[ pathname: ~S~]~@[ (tthsum: ~A)~]~%"
               command pathname tthsum))
    (cond
      (pathname
       (assert (and (consp command) (eq :load-file (car command))
                    (consp (cdr command)) (null (cddr command))))
       (load pathname :verbose (>= *xcvb-verbosity* 8) :print (>= *xcvb-verbosity* 9)))
      (t
       (run-command command)))
    (push entry *manifest*)))

(defun process-manifest (manifest)
  (dolist (entry manifest)
    (apply 'process-manifest-entry entry)))

(defun initialize-manifest (pathname)
  (assert (not *manifest*))
  (setf *manifest* (reverse (read-first-file-form pathname))))
(defun load-manifest (pathname)
  (process-manifest (read-first-file-form pathname)))

;;; Run a slave, obey its orders.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *bnl-keys-with-defaults*
    '((xcvb-binary *xcvb-binary*)
      (setup *xcvb-setup*)
      (source-registry *source-registry*)
      (output-path nil)
      (object-directory *object-directory*)
      (lisp-implementation *lisp-implementation-type*)
      (lisp-binary-path *lisp-executable-pathname*)
      (lisp-image-path *lisp-image-pathname*)
      (features-defined *features-defined*)
      (features-undefined *features-undefined*)
      (disable-cfasl *disable-cfasls*)
      (use-base-image *use-base-image*)
      (verbosity *xcvb-verbosity*)
      (debugging *lisp-allow-debugger*)
      (profiling nil)))
  (defparameter *bnl-keys* (mapcar #'car *bnl-keys-with-defaults*)))

(defun build-slave-command-line (build &key . #.*bnl-keys-with-defaults*)
  (flet ((list-option-arguments (string values)
           (loop
             :for value :in values
             :nconc (list string value))))
    (macrolet
        ((to-option-name (name)
                 (format nil "--~(~a~)" name))
         (pathname-option (var)
           `(when ,var
              (list (to-option-name ,var) (namestring ,var))))
         (string-option (var)
           `(when ,var
              (list (to-option-name ,var) (let ((*print-case* :downcase))
                                            (princ-to-string ,var)))))
         (boolean-option (var)
           `(when ,var
              (list (to-option-name ,var))))
         (pluralize (wrapper &rest vars)
           `(append ,@(loop :for var :in vars :collect `(,wrapper ,var))))
         (string-options (&rest vars)
           `(pluralize string-option ,@vars))
         (pathname-options (&rest vars)
           `(pluralize pathname-option ,@vars))
         (boolean-options (&rest vars)
           `(pluralize boolean-option ,@vars)))
      (append
       (list xcvb-binary "slave-builder")
       (string-options build setup lisp-implementation verbosity source-registry)
       (pathname-options output-path object-directory lisp-binary-path lisp-image-path)
       (list-option-arguments "define-feature" features-defined)
       (list-option-arguments "undefine-feature" features-undefined)
       (boolean-options disable-cfasl use-base-image debugging profiling)))))

(defun build-in-slave (build &rest args &key . #.*bnl-keys*)
  (declare (ignore . #.(remove 'verbosity *bnl-keys*)))
  (let* ((slave-command (apply 'build-slave-command-line build args))
         (slave-output
          (with-safe-io-syntax ()
            (run-program/read-output-string slave-command :ignore-error-status t)))
         (manifest
          (progn
            (unless (and slave-output
                         (string-enclosed-p
                          +xcvb-slave-greeting+ slave-output +xcvb-slave-farewell+))
              (format! *error-output*
                       "Failed to execute a build slave.~%~
			Slave command:~%  ~S~%~
			Slave output:~%~A~%~
			(If using SLIME, you might have useful error output in your *inferior-lisp* buffer.)"
                       slave-command slave-output)
              (error "XCVB slave failed"))
            (read-from-string
             slave-output t nil
             :start (length +xcvb-slave-greeting+)
             :end (- (length slave-output) (length +xcvb-slave-farewell+)))))
         (*xcvb-verbosity* (+ (or verbosity *xcvb-verbosity*) 2)))
    (when (>= *xcvb-verbosity* 9)
      (format! *error-output* "~&Slave XCVB returned following manifest:~%~S~%" manifest))
    manifest))

(defun build-and-load (build &rest args &key . #.*bnl-keys*)
  (declare (ignore . #.*bnl-keys*))
  (process-manifest (apply 'build-in-slave build args)))

(defun bnl (build &rest keys &key . #.*bnl-keys*)
  "Short hand for build-and-load"
  (declare (ignore . #.*bnl-keys*))
  (apply 'build-and-load build keys))

