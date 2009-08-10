;;; XCVB driver to be compiled in buildee images
;;; (largely inspired from cl-launch, a bit by qres-build + hacks by fare & sbrody)

#+xcvb
(module
 (:author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT" ;; MIT-style license. See LICENSE
  :description "XCVB Driver"
  :long-description "Driver code to be loaded in all buildee images for XCVB."))

(in-package :cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 2) (safety 3) (compilation-speed 0) (debug 3);;2)
           #+sbcl (sb-ext:inhibit-warnings 3)
           #+sbcl (sb-c::merge-tail-calls 3) ;-- this plus debug 1 (or sb-c::insert-debug-catch 0 ???) should ensure all tail calls are optimized, says jsnell
	   #+cmu (ext:inhibit-warnings 3)))
  #+sbcl (proclaim '(sb-ext:muffle-conditions sb-ext:compiler-note))
  #+gcl ;;; If using GCL, do some safety checks
  (when (or #-ansi-cl t)
    (format *error-output*
     "XCVB only supports GCL in ANSI mode. Aborting.~%")
    (lisp:quit))
  #+gcl
  (when (or (< system::*gcl-major-version* 2)
            (and (= system::*gcl-major-version* 2)
                 (< system::*gcl-minor-version* 7)))
    (pushnew :gcl-pre2.7 *features*))
  (setf *print-readably* nil ; allegro 5.0 notably will bork without this
        *load-verbose* nil *compile-verbose* nil *compile-print* nil)
  #+cmu (setf ext:*gc-verbose* nil)
  #+clisp (setf custom:*source-file-types* nil custom:*compiled-file-types* nil)
  #+gcl (setf compiler::*compiler-default-type* (pathname "")
              compiler::*lsp-ext* "")
  #+ecl (require 'cmp)
  ;;;; Ensure package hygiene
  (unless (find-package :xcvb-driver)
    (make-package
     :xcvb-driver :nicknames '(:xcvbd)
     :use (list (or (find-package :common-lisp) :lisp)))))

(in-package :xcvb-driver)

(map () #'export
     '(finish-outputs quit resume restart debugging
       run do-run run-commands run-command
       with-coded-exit with-controlled-compiler-conditions with-xcvb-compilation
       *uninteresting-conditions* *debugging*))

(defvar *debugging* nil)

;; Variables that define the current system
(defvar *restart* nil)

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

(defun print-backtrace (out)
  (declare (ignorable out))
  nil
  #+clozure (let ((*debug-io* out))
	      (ccl:print-call-history :count 100 :start-frame-number 1)
	      (finish-output out))
  #+sbcl (sb-debug:backtrace most-positive-fixnum out))

(defvar *stderr* *error-output*)

(defun bork (condition)
  (unless *debugging*
    (format *stderr* "~&~A~%" condition)
    (print-backtrace *stderr*)
    (format *stderr* "~&~A~%" condition)
    (quit 99)))

(defun call-with-coded-exit (thunk)
  (handler-bind ((error #'bork))
    (funcall thunk)
    (quit 0)))

(defmacro with-coded-exit (() &body body)
  `(call-with-coded-exit (lambda () ,@body)))

(defvar *uninteresting-conditions*
  (append
   #+sbcl
   '(sb-c::simple-compiler-note
     "&OPTIONAL and &KEY found in the same lambda list: ~S"
     sb-int:package-at-variance
     sb-kernel:uninteresting-redefinition
     sb-kernel:undefined-alien-style-warning
     sb-ext:implicit-generic-function-warning
     sb-kernel:lexical-environment-too-complex)
   )
  "Conditions that may be skipped. type symbols, predicates or strings")

(defun uninteresting-condition-p (condition)
  ;; TODO: do something interesting, extensible and semi-portable here.
  (loop :for x :in *uninteresting-conditions* :thereis
        (etypecase x
          (symbol (typep condition x))
          (function (funcall x condition))
          (string (and (typep condition 'simple-condition)
                       (equal (simple-condition-format-control condition) x))))))

(defun call-with-controlled-compiler-conditions (thunk)
  (handler-bind
      ((t
        #'(lambda (condition)
            ;; TODO: do something magic for undefined-function,
            ;; save all of aside, and reconcile in the end of the virtual compilation-unit.
            (when (uninteresting-condition-p condition)
              (muffle-warning condition)))))
    (funcall thunk)))

(defmacro with-controlled-compiler-conditions (() &body body)
  `(call-with-controlled-compiler-conditions (lambda () ,@body)))

(defvar *deferred-warnings* ()
  "Warnings the handling of which is deferred until the end of the compilation unit")

(defun call-with-xcvb-compilation-unit (thunk &key forward-references)
  (with-compilation-unit (:override t)
    (let ((*deferred-warnings* ())
          #+sbcl (sb-c::*undefined-warnings* nil))
      (multiple-value-prog1
          (handler-bind (#+clozure
                         (ccl::compiler-warning
                          #'(lambda (condition)
                              ;;(push condition *deferred-warnings*) ;TODO: decode it as for SBCL!
                              (muffle-warning condition))))
            (funcall thunk))
        #+sbcl
        (loop :for w :in sb-c::*undefined-warnings*
              :for kind = (sb-c::undefined-warning-kind w) ; :function :variable :type
              :for name = (sb-c::undefined-warning-name w)
              :for symbol = (cond
                              ((and (consp name) (eq (car name) 'setf))
                               (assert (eq kind :function))
                               (assert (and (consp (cdr name)) (null (cddr name))) ())
                               (setf kind :setf-function)
                               (second name))
                              (t
                               (assert (member kind '(:function :variable :type)) ())
                               name))
              :for symbol-name = (symbol-name symbol)
              :for package-name = (package-name (symbol-package symbol))
              :collect `(:undefined ,symbol-name ,package-name ,kind) :into undefined-warnings
              :finally (setf *deferred-warnings* undefined-warnings
                             sb-c::*undefined-warnings* nil)))
      (when forward-references
        (with-open-file (s forward-references :direction :output :if-exists :supersede)
          (write *deferred-warnings* :stream s :pretty t :readably t)
          (terpri s))))))

(defmacro with-xcvb-compilation-unit ((&key forward-references) &body body)
  `(call-with-xcvb-compilation-unit (lambda () ,@body) :forward-references ,forward-references))

(defun do-resume ()
  (when *restart* (funcall *restart*))
  (quit 0))

(defun resume ()
  (do-resume))

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
  (multiple-value-bind (head args)
      (etypecase command
        (symbol (values command nil))
        (cons (values (car command) (cdr command))))
    (apply (function-for-command head) args)))

(defun do-run (commands)
  (let ((*stderr* *error-output*))
    (run-commands commands)))

(defun run-commands (commands)
  (map () #'run-command commands))

(defmacro run (&rest commands)
  `(with-coded-exit ()
    (do-run ',commands)))

(defun load-file (x)
  (load x))

(defun call (package symbol &rest args)
  (apply (do-find-symbol symbol package) args))

(defun eval-string (string)
  (eval (read-from-string string)))

(defun asdf-symbol (x)
  (do-find-symbol x :asdf))
(defun asdf-call (x &rest args)
  (apply 'call :asdf x args))

(defun load-asdf (x)
  (asdf-call :oos (asdf-symbol :load-op) x))

(defun do-compile-lisp (dependencies source fasl &key cfasl)
  (let ((*default-pathname-defaults* (truename *default-pathname-defaults*)))
    (multiple-value-bind (output-truename warnings-p failure-p)
        (with-controlled-compiler-conditions ()
          (with-xcvb-compilation-unit ()
            (run-commands dependencies)
            (apply #'compile-file source
                   :output-file (merge-pathnames fasl)
                   ;; #+(or ecl gcl) :system-p #+(or ecl gcl) t
                   (when cfasl
                     `(:emit-cfasl ,(merge-pathnames cfasl))))))
        (declare (ignore output-truename))
        (when warnings-p
          (error "Compilation Warned for ~A" source))
        (when failure-p
          (error "Compilation Failed for ~A" source))))
  (values))

(defun compile-lisp (spec &rest dependencies)
  (destructuring-bind (source fasl &key cfasl) spec
    (do-compile-lisp dependencies source fasl :cfasl cfasl)))

#-ecl
(defun do-create-image (image dependencies &rest flags)
  (with-controlled-compiler-conditions ()
    (run-commands dependencies))
  (apply #'dump-image image flags))

#+ecl ;; wholly untested and probably buggy.
(defun do-create-image (image dependencies &key standalone)
  (let ((epilogue-code
          `(progn
            ,(if standalone '(resume) '(si::top-level)))))
    (c::builder :program (parse-namestring image)
		:lisp-files (mapcar #'second dependencies)
		:epilogue-code epilogue-code)))

(defun create-image (spec &rest dependencies)
  (destructuring-bind (image &key standalone package) spec
    (do-create-image image dependencies
                     :standalone standalone :package package)))

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

(defun shell-boolean (x)
  (quit
   (if x 0 1)))

(defun asdf-systems-up-to-date (&rest systems)
  "Are all the loaded systems up to date?"
  (with-coded-exit ()
    (shell-boolean (asdf-systems-up-to-date-p systems))))

(defun debugging ()
  (setf *debugging* t)
  #+sbcl (sb-ext:enable-debugger)
  #+clisp (ext:set-global-handler #'invoke-debugger)
  (setf *load-verbose* t *compile-verbose* t *compile-print* t)
  (values))

;;;(format t "~&XCVB driver loaded~%")
