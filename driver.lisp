;;; XCVB driver to be compiled and loaded in target images
;;; largely inspired from cl-launch, a bit by qres-build + hacks by sbrody and fare

#+xcvb
(module
 (:author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT" ;; MIT-style license. See LICENSE
  :description "XCVB Driver"
  :long-description "Driver code to be loaded in all buildee images for XCVB."
  :build-depends-on nil))

;;; Hush!
#+debug
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:setf cl:*load-verbose* () cl:*load-print* ()
           cl:*compile-verbose* () cl:*compile-print* ()))

(cl:defpackage :xcvb-driver
  (:nicknames :xcvbd)
  (:use :common-lisp)
  (:export
   #:*optimization-settings*
   #:*restart* #:debugging #:profiling #:*goal* #:*stderr*
   #:*uninteresting-conditions* #:*fatal-condition* #:*deferred-warnings*
   #:getenv #:emptyp #:setenvp #:setup-environment
   #:debugging #:with-profiling
   #:finish-outputs #:quit #:shell-boolean
   #:print-backtrace #:die #:bork #:with-coded-exit
   #:uninteresting-condition-p #:fatal-condition-p
   #:with-controlled-compiler-conditions #:with-controlled-loader-conditions
   #:with-xcvb-compilation-unit
   #:do-find-symbol #:call #:eval-string
   #:run #:do-run #:run-commands #:run-command
   #:asdf-symbol #:asdf-call
   #:resume #-ecl #:dump-image))

(in-package :xcvb-driver)

;;; Optimization settings
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *optimization-settings*
    '(optimize (speed 1) (safety 3) (compilation-speed 0) (debug 3)
      #+sbcl (sb-ext:inhibit-warnings 3)
      ;; These should ensure all tail calls are optimized, says jsnell:
      #+sbcl (sb-c::merge-tail-calls 3) #+sbcl (sb-c::insert-debug-catch 0)
      #+cmu (ext:inhibit-warnings 3)))
  (proclaim *optimization-settings*))

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
  #+ecl (require :cmp))


;; Variables that define the current system
(defvar *restart* nil
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
   '("No generic function ~S present when encountering macroexpansion of defmethod. Assuming it will be an instance of standard-generic-function.") ;; from closer2mop
   )
  "Conditions that may be skipped. type symbols, predicates or strings")
(defvar *fatal-condition*
  '(or serious-condition)
  "Conditions to be considered fatal during compilation.")
(defvar *deferred-warnings* ()
  "Warnings the handling of which is deferred until the end of the compilation unit")
(defvar *initial-random-state* (make-random-state nil)
  "initial random state to preserve determinism")

;;; Setting up the environment from shell variables
(defun getenv (x)
  #-(or cmu clozure allegro gcl clisp ecl sbcl lispworks)
  (error "GETENV not supported for your Lisp implementation")
  #-cmu
  (#+clozure ccl::getenv
   #+allegro sys:getenv
   #+gcl system:getenv
   #+clisp ext:getenv
   #+ecl si:getenv
   #+sbcl sb-ext:posix-getenv
   #+lispworks lispworks:environment-variable
   x)
  #+cmu (cdr (assoc (intern x :keyword) ext:*environment-list*)))
(defun emptyp (x)
  (or (null x) (and (vectorp x) (zerop (length x)))))
(defun setenvp (x)
  (not (emptyp (getenv x))))
(defun setup-environment ()
  (debugging (setenvp "XCVB_DEBUGGING"))
  (setf *profiling* (setenvp "XCVB_PROFILING")))
(defun proclaim-optimization-settings ()
  (proclaim *optimization-settings*)
  (when *debugging*
    #+sbcl (format *error-output* "~&SB-C::*POLICY* = ~S~%" sb-c::*policy*)))

;;; Debugging
(defun debugging (&optional (debug t))
  (setf *debugging* debug
        *load-verbose* debug
        *load-print* debug
        *compile-verbose* debug
        *compile-print* debug)
  (cond
    (debug
     #+sbcl (sb-ext:enable-debugger)
     #+clisp (ext:set-global-handler nil nil))
    (t
     #+sbcl (sb-ext:disable-debugger)
     #+clisp (ext:set-global-handler 'error #'bork)))
  (values))

;;; Profiling
(defun call-with-maybe-profiling (thunk what goal)
  (when *debugging*
    (format *trace-output* "~&Now ~S~&" what))
  (if *profiling*
    (let* ((start-time (get-internal-real-time))
           (values (multiple-value-list (funcall thunk)))
           (end-time (get-internal-real-time))
           (duration (coerce (/ (- end-time start-time) internal-time-units-per-second) 'double-float)))
      (format *trace-output* "~&~S~&" `(:profiling ,what :from ,goal :duration ,duration))
      (apply #'values values))
    (funcall thunk)))
(defmacro with-profiling (what &body body)
  `(call-with-maybe-profiling (lambda () ,@body) ,what *goal*))

;;; Exiting properly or im-
(defun finish-outputs ()
  (finish-output *error-output*)
  (finish-output *standard-output*))
(defun quit (&optional (code 0) (finish-output t))
  "Quits from the Lisp world, with the given exit status if provided.
This is designed to abstract away the implementation specific quit forms."
  (when finish-output (finish-outputs))
  #+cmu (unix:unix-exit code)
  #+clisp (ext:quit code)
  #+sbcl (sb-unix:unix-exit code)
  #+clozure (ccl:quit code)
  #+gcl (lisp:quit code)
  #+allegro (excl:exit code :quiet t)
  #+ecl (si:quit code)
  #+lispworks (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
  #-(or cmu clisp sbcl clozure gcl allegro ecl lispworks)
  (error "xcvb driver: Quitting not implemented"))
(defun shell-boolean (x)
  (quit
   (if x 0 1)))
(defun print-backtrace (out)
  (declare (ignorable out))
  nil
  #+clozure (let ((*debug-io* out))
	      (ccl:print-call-history :count 100 :start-frame-number 1)
	      (finish-output out))
  #+sbcl (sb-debug:backtrace most-positive-fixnum out))
(defun die (format &rest arguments)
  (format *stderr* "~&")
  (apply #'format *stderr* format arguments)
  (format *stderr* "~&")
  (quit 99))
(defun bork (condition)
  (format *stderr* "~&BORK:~%~A~%" condition)
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
  `(call-with-coded-exit (lambda () ,@body)))


;;; Filtering conditions during the build.
(defun uninteresting-condition-p (condition)
  (loop :for x :in *uninteresting-conditions* :thereis (match-condition-p x condition)))
(defun match-condition-p (x condition)
  (etypecase x
    (symbol (typep condition x))
    (function (funcall x condition))
    (string (and (typep condition 'simple-condition)
                 (equal (simple-condition-format-control condition) x)))))
(defun fatal-condition-p (condition)
  (typep condition *fatal-condition*))
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
  `(call-with-controlled-compiler-conditions (lambda () ,@body)))
(defun call-with-controlled-loader-conditions (thunk)
  (let ((*uninteresting-conditions*
         (append
          #+sbcl '(sb-kernel:redefinition-with-defun
                   sb-kernel:redefinition-with-defgeneric
                   sb-kernel:redefinition-with-defmethod)
          *uninteresting-conditions*)))
    (call-with-controlled-compiler-conditions thunk)))
(defmacro with-controlled-loader-conditions (() &body body)
  `(call-with-controlled-loader-conditions (lambda () ,@body)))
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
  `(call-with-xcvb-compilation-unit (lambda () ,@body) :forward-references ,forward-references))


;;; Interpreting commands from the xcvb-driver-command DSL.
(defun do-find-symbol (name package-name)
  (let ((package (find-package (string package-name))))
    (unless package
      (error "Trying to use package ~A, but it is not loaded yet!" package-name))
    (let ((symbol (find-symbol (string name) package)))
      (unless symbol
        (error "Trying to use symbol ~A in package ~A, but it does not exist!" name package-name))
      symbol)))

(defun function-for-command (designator)
  (fdefinition (do-find-symbol designator :xcvb-driver)))
(defun run-command (command)
  (proclaim-optimization-settings)
  (multiple-value-bind (head args)
      (etypecase command
        (symbol (values command nil))
        (cons (values (car command) (cdr command))))
    (apply (function-for-command head) args)))
(defun do-run (commands)
  (let ((*stderr* *error-output*))
    (setup-environment)
    (run-commands commands)))
(defun run-commands (commands)
  (map () #'run-command commands))
(defmacro run (&rest commands)
  `(with-coded-exit ()
    (do-run ',commands)))


;;; Simple commands
(defun load-file (x)
  (with-profiling `(:load-file ,x)
    (with-controlled-loader-conditions ()
      (unless (load x)
        (die "Failed to load ~A" (list x))))))
(defun call (package symbol &rest args)
  (apply (do-find-symbol symbol package) args))
(defun eval-string (string)
  (eval (read-from-string string)))
(defun cl-require (x)
  (with-profiling `(:require ,x)
    (require x)))

;;; ASDF support
(defun asdf-symbol (x)
  (do-find-symbol x :asdf))
(defun asdf-call (x &rest args)
  (apply 'call :asdf x args))
(defun load-asdf (x &key parallel (verbose *compile-verbose*)) ;; parallel loading requires POIU
  (with-profiling `(:asdf ,x)
    (asdf-call
     :operate (asdf-symbol (if parallel :parallel-load-op :load-op)) x
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
      (list (if (getf args :verbose t) *trace-output*
            (make-broadcast-stream)))
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

;;; XCVB Master support
(defun initialize-manifest (path)
  (call :xcvb-master :initialize-manifest path))
(defun xcvb-driver::load-manifest (path)
  (call :xcvb-master :load-manifest path))

;;; Actually compiling
(defmacro with-determinism (goal &body body)
  `(call-with-determinism ,goal (lambda () ,@body)))

(defun call-with-determinism (goal thunk)
  ;;; We use the goal (fullname) as the seed as opposed to the tthsum of some contents
  ;;; to give a greater chance to trivial modifications of the source text (e.g.
  ;;; comments and whitespace changes) to be without effect on the compilation output.
  ;;;
  ;;; In SBCL, we'll also need to somehow disable the start-time slot of the
  ;;; (def!struct (source-info ...)) from main/compiler.lisp (package SB-C).
  (let* ((hash (sxhash goal))
         (*gensym-counter* (* hash 10000))
         #+sbcl (sb-impl::*gentemp-counter* (* hash 10000))
         ;;; SBCL will hopefully export a better mechanism soon. See:
         ;;; https://bugs.launchpad.net/sbcl/+bug/310116
         (*random-state*
          #+sbcl (sb-kernel::%make-random-state
                  :state (sb-kernel::init-random-state (ldb (byte 32 0) hash)))
          #-sbcl (make-random-state *initial-random-state*)))
    (funcall thunk)))

(defun do-compile-lisp (dependencies source fasl &key cfasl)
  (let ((*goal* `(:compile-lisp ,source))
        (*default-pathname-defaults* (truename *default-pathname-defaults*)))
    (multiple-value-bind (output-truename warnings-p failure-p)
        (with-profiling `(:preparing-and-compiling ,source)
          (with-xcvb-compilation-unit ()
            (run-commands dependencies)
            (with-profiling `(:compiling ,source)
              (with-determinism `(:compiling ,source)
                (apply #'compile-file source
                       :output-file (merge-pathnames fasl)
                       ;; #+(or ecl gcl) :system-p #+(or ecl gcl) t
                       (when cfasl
                         `(:emit-cfasl ,(merge-pathnames cfasl))))))))
      (declare (ignore output-truename))
      (when failure-p
        (die "Compilation Failed for ~A" source))
      (when warnings-p
        (die "Compilation Warned for ~A" source))))
  (values))
(defun compile-lisp (spec &rest dependencies)
  (destructuring-bind (source fasl &key cfasl) spec
    (do-compile-lisp dependencies source fasl :cfasl cfasl)))


;;; Dumping and resuming an image
(defun do-resume (&key (restart *restart*))
  (when restart (funcall restart))
  (quit 0))
(defun resume (&key restart)
  (do-resume :restart restart))

#-ecl
(defun dump-image (filename &key standalone package)
  (declare (ignorable filename standalone))
  (setf *package* (find-package (or package :cl-user)))
  (defparameter *previous-features* *features*)
  #+clisp
  (apply #'ext:saveinitmem filename
   :quiet t
   :start-package *package*
   :keep-global-handlers nil
   :executable (if standalone 0 t) ;--- requires clisp 2.48 or later, still catches --clisp-x
   (when standalone
     (list
      :norc t
      :script nil
      :init-function #'resume
      )))
  #+sbcl
  (progn
    ;;(sb-pcl::precompile-random-code-segments) ;--- it is ugly slow at compile-time (!) when the initial core is a big CLOS program. If you want it, do it yourself.
   (setf sb-ext::*gc-run-time* 0)
   (apply 'sb-ext:save-lisp-and-die filename
    :executable t ;--- always include the runtime that goes with the core
    (when standalone (list :toplevel #'resume :save-runtime-options t)))) ;--- only save runtime-options for standalone executables
  #+cmu
  (progn
   (ext:gc :full t)
   (setf ext:*batch-mode* nil)
   (setf ext::*gc-run-time* 0)
   (extensions:save-lisp filename))
  #+clozure
  (ccl:save-application filename :prepend-kernel t)
  #+allegro
  (progn
   (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) ; :new 5000000
   (excl:dumplisp :name filename :suppress-allegro-cl-banner t))
  #+lispworks
  (if standalone
    (lispworks:deliver 'resume filename 0 :interface nil)
    (hcl:save-image filename :environment nil))
  #+gcl
  (progn
   (si::set-hole-size 500) (si::gbc nil) (si::sgc-on t)
   (si::save-system filename))
  #-(or clisp sbcl cmu clozure allegro gcl lispworks)
  (%abort 11 "XCVB-Driver doesn't supports image dumping with this Lisp implementation.~%"))

;;; Actually creating images
#-ecl
(defun do-create-image (image dependencies &rest flags)
  (let ((*goal* `(create-image ,image))
        #+sbcl (*uninteresting-conditions*
                (cons "undefined ~(~A~): ~S" *uninteresting-conditions*)))
    (with-controlled-compiler-conditions ()
      (run-commands dependencies))
    (apply #'dump-image image flags)))

#+ecl ;; wholly untested and probably buggy.
(defun do-create-image (image dependencies &key standalone)
  (let ((*goal* `(create-image ,image))
        (epilogue-code
          `(progn
            ,(if standalone '(resume) '(si::top-level)))))
    (c::builder :program (parse-namestring image)
		:lisp-files (mapcar #'second dependencies)
		:epilogue-code epilogue-code)))
(defun create-image (spec &rest dependencies)
  (destructuring-bind (image &key standalone package) spec
    (do-create-image image dependencies
                     :standalone standalone :package package)))

;;;(format t "~&XCVB driver loaded~%")
