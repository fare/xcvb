;;;;; XCVB driver. Load it in your Lisp image and build with XCVB.

;;;; ----- Prelude -----
#+xcvb
(module
 (:description "XCVB Driver"
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT" ;; MIT-style license. See LICENSE
  :build-depends-on nil))

;; #.(setf *load-verbose* () *load-print* () *compile-verbose* () *compile-print* ()) ;; Hush!

(cl:in-package :cl-user)

(declaim (optimize (speed 2) (space 2) (safety 3) (debug 3) (compilation-speed 0))
         #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

(defpackage :xcvb-driver
  (:nicknames :xcvbd :xd)
  (:use :cl)
  (:export

   ;;; special variables shared with XCVB itself
   #:*lisp-implementation-type*
   #:*lisp-executable-pathname* #:*lisp-image-pathname*
   #:*lisp-implementation-directory*
   #:*lisp-flags*  #:*lisp-allow-debugger*
   #:*use-base-image* #:*disable-cfasls*
   #:*features-defined* #:*features-undefined*
   #:*xcvb-verbosity*
   #:*cache* #:*object-cache* #:*workspace*
   #:*install-prefix* #:*install-program* #:*install-configuration*
   #:*install-data* #:*install-library* #:*install-image* #:*install-lisp*
   #:*temporary-directory*
   #:*source-registry*

   ;;; special variables for XCVB master itself
   #:*xcvb-program* #:*manifest*
   #:*required-xcvb-version*

   ;;; special variables for portability issues
   #:*default-element-type*

   ;;; String utilities - copied from fare-utils
   ;;#:string-prefix-p #:string-suffix-p #:string-enclosed-p

   ;; command-line arguments
   #:raw-command-line-arguments #:command-line-arguments #:*arguments* #:*dumped*
   
   ;;; I/O utilities
   #:with-output #:with-input-file #:with-safe-io-syntax #:with-temporary-file
   #:slurp-stream-string #:slurp-stream-lines #:slurp-stream-forms
   #:slurp-file-string #:slurp-file-lines #:slurp-file-forms
   #:copy-stream-to-stream #:copy-stream-to-stream-line-by-line
   #:read-first-file-form #:read-function
   #:slurp-input-stream

   ;;; Escaping the command invocation madness
   #:easy-sh-character-p #:escape-sh-token #:escape-sh-command
   #:escape-windows-token #:escape-windows-command
   #:escape-token #:escape-command

   ;;; run-program/foo
   #:run-program/
   #:subprocess-error
   #:subprocess-error-code #:subprocess-error-command #:subprocess-error-process
   ;; Obsolete:
   #:run-program/process-output-stream
   #:run-program/read-output-lines #:run-program/read-output-string
   #:run-program/read-output-form #:run-program/read-output-forms
   #:run-program/for-side-effects #:run-program/echo-output

   ;; pathname utilities
   #:native-namestring #:parse-native-namestring

   ;; current directory
   #:getcwd #:chdir #:with-current-directory

   ;; Magic strings
   #:+xcvb-slave-greeting+ #:+xcvb-slave-farewell+

   ;;; Using an inferior XCVB
   #:build-and-load #:bnl #:build-in-slave

   ;;; Build-time variables
   #:*optimization-settings*
   #:*uninteresting-conditions* #:*uninteresting-load-conditions*
   #:*fatal-conditions* #:*deferred-warnings*
   #:*goal* #:*stderr* #:*debugging* #:*profiling*
   #:*post-image-restart* #:*entry-point*

   ;;; Environment support
   #:getenv #:emptyp #:getenvp #:setup-environment
   #:debugging #:with-profiling
   #:format! #:finish-outputs #:quit #:shell-boolean
   #:print-backtrace #:die #:bork #:with-coded-exit
   #:uninteresting-condition-p #:fatal-condition-p
   #:proclaim-optimization-settings
   #:with-controlled-compiler-conditions #:with-controlled-loader-conditions
   #:with-xcvb-compilation-unit
   #:find-symbol* #:call #:eval-string #:load-string #:load-stream
   ;; #:run #:do-run #:run-commands #:run-command ; used by XCVB, not end-users.
   #:resume #-ecl #:dump-image #+ecl #:create-bundle
   #:register-fullname #:register-fullnames #:load-fullname-mappings
   #:registered-fullname-pathname))

(in-package :xcvb-driver)

;;; Initial implementation-dependent setup
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *implementation-settings*
    `(;; These should ensure all tail calls are optimized, says jsnell:
      #+sbcl (sb-c::insert-debug-catch 0) ;; (sb-c::merge-tail-calls 3) is redundant and deprecated
      #+(or cmu scl) (ext:inhibit-warnings 3)))
  (defvar *optimization-settings*
    `((speed 2) (space 2) (safety 3) (debug 2) (compilation-speed 0)
      ,@*implementation-settings*))
  (proclaim `(optimize ,@*optimization-settings*))
  ;; otherwise ACL 5.0 may crap out on ASDF dependencies,
  ;; but even other implementations may have "fun" debugging.
  (setf *print-readably* nil)
  (defun featurep (x &optional (*features* *features*))
    (cond
      ((atom x) (and (member x *features*) t))
      ((eq :not (car x)) (assert (null (cddr x))) (not (featurep (cadr x))))
      ((eq :or (car x)) (some #'featurep (cdr x)))
      ((eq :and (car x)) (every #'featurep (cdr x)))
      (t (error "Malformed feature specification ~S" x))))
  (defun os-unix-p ()
    (featurep '(:or :unix :cygwin :darwin)))
  (defun os-windows-p ()
    (and (not (os-unix-p)) (featurep '(:or :win32 :windows :mswindows :mingw32))))
  (defun detect-os ()
    (flet ((yes (yes) (pushnew yes *features*))
           (no (no) (setf *features* (remove no *features*))))
      (cond
        ((os-unix-p) (yes :os-unix) (no :os-windows))
        ((os-windows-p) (yes :os-windows) (no :os-unix))
        (t (error "Congratulations for trying XCVB on an operating system~%~
that is neither Unix, nor Windows.~%Now you port it.")))))
  (detect-os)
  #+(or abcl (and allegro ics) (and (or clisp cmu ecl mkcl) unicode) clozure
        lispworks (and sbcl sb-unicode) scl)
  (pushnew :xcvb-unicode *features*)
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
  #+(and ecl (not ecl-bytecmp))
  (progn
    (let ((*load-verbose* nil)) (require :cmp))
    (setf c::*compile-in-constants* t))
  #.(or #+mcl ;; the #$ doesn't work on other lisps, even protected by #+mcl
     (read-from-string
      "(eval-when (:compile-toplevel :load-toplevel :execute)
         (ccl:define-entry-point (_getenv \"getenv\") ((name :string)) :string)
         (ccl:define-entry-point (_system \"system\") ((name :string)) :int)
         ;; See http://code.google.com/p/mcl/wiki/Portability
         (defun current-user-homedir-pathname ()
           (ccl::findfolder #$kuserdomain #$kCurrentUserFolderType))
         (defun probe-posix (posix-namestring)
           \"If a file exists for the posix namestring, return the pathname\"
           (ccl::with-cstrs ((cpath posix-namestring))
             (ccl::rlet ((is-dir :boolean)
                         (fsref :fsref))
               (when (eq #$noerr (#_fspathmakeref cpath fsref is-dir))
                 (ccl::%path-from-fsref fsref is-dir))))))"))
  #+sbcl (progn
           (require :sb-posix)
           (proclaim '(sb-ext:muffle-conditions sb-ext:compiler-note)))
  (pushnew :xcvb-driver *features*))

;;;; ----- User-visible variables, 1: Control build in current process -----

;;; Variables used to control building in the current image

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

(defvar *stderr* #-clozure *error-output* #+clozure ccl::*stderr*
  "the original error output stream at startup")

(defvar *uninteresting-conditions*
  (append
   #+sbcl
   '(sb-c::simple-compiler-note
     "&OPTIONAL and &KEY found in the same lambda list: ~S"
     sb-int:package-at-variance
     sb-kernel:uninteresting-redefinition
     ;; the below four are controversial to include here;
     ;; however there are issues with the asdf upgrade if they are not present
     sb-kernel:redefinition-with-defun
     sb-kernel:redefinition-with-defgeneric
     sb-kernel:redefinition-with-defmethod
     sb-kernel::redefinition-with-defmacro ; not exported by old SBCLs
     sb-kernel:undefined-alien-style-warning
     sb-ext:implicit-generic-function-warning
     sb-kernel:lexical-environment-too-complex
     "Couldn't grovel for ~A (unknown to the C compiler).")
   ;;#+clozure '(ccl:compiler-warning)
   '("No generic function ~S present when encountering macroexpansion of defmethod. Assuming it will be an instance of standard-generic-function.") ;; from closer2mop
   )
  "Conditions that may be skipped. type symbols, predicates or strings")

(defvar *uninteresting-load-conditions*
  (append
   '("Overwriting already existing readtable ~S." ;; from named-readtables
     #(#:finalizers-off-warning :asdf-finalizers)) ;; from asdf-finalizers
   #+clisp '(clos::simple-gf-replacing-method-warning))
  "Additional conditions that may be skipped while loading. type symbols, predicates or strings")

(defvar *fatal-conditions*
  '(serious-condition)
  "Conditions to be considered fatal during compilation.")

(defvar *deferred-warnings* ()
  "Warnings the handling of which is deferred until the end of the compilation unit")

(defvar *initial-random-state* (make-random-state nil)
  "initial random state to preserve determinism")


;;;; ----- Basic Utilities, used to bootstrap further -----

;;; Dealing with future packages

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-symbol* (name package-name &optional (error t))
    "Find a symbol in a package of given string'ified NAME;
unless CL:FIND-SYMBOL, work well with 'modern' case sensitive syntax
by letting you supply a symbol or keyword for the name;
also works well when the package is not present.
If optional ERROR argument is NIL, return NIL instead of an error
when the symbol is not found."
    (let ((package (find-package (string package-name))))
      (if package
          (let ((symbol (find-symbol (string name) package)))
            (or symbol
                (when error
                  (error "There is no symbol ~A in package ~A" name package-name))))
          (when error
            (error "There is no package ~A" package-name))))))

(defun call (package name &rest args)
  "Call a function associated with symbol of given name in given package,
with given ARGS. Useful when the call is read before the package is loaded,
or when loading the package is optional."
  (apply (find-symbol* name package) args))

;;; Setting up the environment from shell variables

(defun getenv (x)
  "Query the libc runtime environment. See getenv(3)."
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
  #+mkcl (#.(or (find-symbol* 'getenv :si) (find-symbol* 'getenv :mk-ext)) x)
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (error "~S is not supported on your implementation" 'getenv))

(defun emptyp (x)
  "Predicate that is true for an empty sequence"
  (or (null x) (and (vectorp x) (zerop (length x)))))
(defun getenvp (x)
  "Predicate that is true if the named variable is present in the libc environment,
then returning the non-empty string value of the variable"
  (let ((g (getenv x))) (and (not (emptyp g)) g)))


;;; On ABCL at least, the Operating System is no compile-time constant.

(defun default-temporary-directory ()
  (flet ((f (s v d) (format nil "~A~A" (or (getenv v) d (error "No temporary directory!")) s)))
    (let ((dir (cond
                 ((os-unix-p) (f #\/ "TMPDIR" "/tmp"))
                 ((os-windows-p) (f #\\ "TEMP" nil))))
          #+mcl (dir (probe-posix dir)))
      (pathname dir))))


;;;; ----- User-visible variables, 2: Control XCVB -----

;;; These variables are shared with XCVB itself.

(defvar *lisp-implementation-type*
  ;; TODO: test on all OS and implementation platform combinations!
  #+abcl :abcl #+allegro :allegro
  #+clisp :clisp #+clozure :ccl #+cmu :cmucl #+cormanlisp :corman
  #+ecl :ecl #+gcl :gcl #+genera :genera
  #+lispworks-personal-edition :lispworks-personal
  #+(and lispworks (not lispworks-personal-edition)) :lispworks
  #+mcl :mcl #+mkcl :mkcl #+sbcl :sbcl #+scl :scl #+xcl :xcl
  #-(or abcl allegro clisp clozure cmu cormanlisp
        ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (error "Your Lisp implementation is not supported by the XCVB driver (yet). Please help.")
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
  (or #+clozure (namestring (ccl::ccl-directory))
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

(defvar *cache* nil
  "where to store object files, etc.
NIL: default to $XDG_CACHE_HOME/xcvb/ or $HOME/.cache/xcvb/, see docs")

(defvar *object-cache* nil
  "Path to the object cache.
NIL: default to *cache*/*implementation-identifier*/, see docs")

(defvar *workspace* nil
  "where to store test and intermediate files private to current run
NIL: default to <current-directory>/workspace/, see docs")

(defvar *install-prefix* nil
  "where to install files.
NIL: default to /usr/local/, see docs
\"/\": default to /, with special defaults for other paths.
T: use home directory with special defaults for other paths below.")

(defvar *install-program* nil
  "where to install program 'binary' (executable) files.
NIL: default to *install-prefix*/bin, see docs")

(defvar *install-configuration* nil
  "where to install configuration files.
NIL: default to *install-prefix*/etc, see docs")

(defvar *install-data* nil
  "where to install shared (architecture-independent) data files.
NIL: default to *install-prefix*/share, see docs")

(defvar *install-library* nil
  "where to install library (architecture-dependent) files.
NIL: default to *install-prefix*/lib, see docs")

(defvar *install-image* nil
  "where to install common-lisp image (architecture- and implementation- dependent) files.
NIL: default to *install-library*/common-lisp/images/, see docs")

(defvar *install-lisp* nil
  "where to install common-lisp source code and systems, etc.
NIL: default to *install-data*/common-lisp/, see docs")

(defvar *temporary-directory* (default-temporary-directory)
  "pathname of directory where to store temporary files")

(defvar *use-base-image* t
  "Should we be using a base image for all builds?")


;;; These variables are specific to a master controlling XCVB as a slave.

(defvar *xcvb-program* "xcvb"
  "Path to the XCVB binary (a string), OR t if you want to use an in-image XCVB")

(defvar *required-xcvb-version* "0.591"
  "Minimal version of XCVB required for use with this version of the xcvb-driver")

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


;;;; ---- More utilities -----

;;; To be portable to CCL and more, we need to explicitly flush stream buffers.

(defun finish-outputs ()
  "Finish output on the main output streams.
Useful for portably flushing I/O before user input or program exit."
  (dolist (s (list *stderr* *error-output* *standard-output* *trace-output*))
    (ignore-errors (finish-output s)))
  (values))

(defun format! (stream format &rest args)
  "Just like format, but call finish-outputs before and after the output."
  (finish-outputs)
  (apply 'format stream format args)
  (finish-output stream))


;;; Pathname helpers

(defun pathname-directory-pathname (pathname)
  "Pathname for the directory containing given PATHNAME"
  (make-pathname :name nil :type nil :version nil :defaults pathname))

(defun native-namestring (x)
  "From a CL pathname, a namestring suitable for use by the OS shell"
  (let ((p (pathname x)))
    #+clozure (let ((*default-pathname-defaults* #p"")) (ccl:native-translated-namestring p)) ; see ccl bug 978
    #+(or cmu scl) (ext:unix-namestring p nil)
    #+sbcl (sb-ext:native-namestring p)
    #-(or clozure cmu sbcl scl) (namestring p)))

(defun parse-native-namestring (x)
  "From a native namestring suitable for use by the OS shell, a CL pathname"
  (check-type x string)
  #+clozure (ccl:native-to-pathname x)
  #+sbcl (sb-ext:parse-native-namestring x)
  #-(or clozure sbcl) (parse-namestring x))


;;; Output helpers

(defgeneric call-with-output (x thunk)
  (:documentation
   ;; code from fare-utils base/streams where it's now named
   ;; call-with-output-stream to avoid the package clash in a lot of my code.
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
  "Bind X to an output stream, coercing VALUE (default: previous binding of X)
as per FORMAT, and evaluate BODY within the scope of this binding."
  `(call-with-output ,value #'(lambda (,x) ,@body)))


;;; Input helpers

(defvar *default-element-type* (or #+(or abcl cmu cormanlisp scl xcl) 'character :default)
  "default element-type for open (depends on the current CL implementation)")

(defun call-with-input-file (pathname thunk
                             &key (element-type *default-element-type*)
                             (external-format :default))
  "Open FILE for input with given options, call THUNK with the resulting stream."
  (with-open-file (s pathname :direction :input
                     :element-type element-type :external-format external-format
                     :if-does-not-exist :error)
    (funcall thunk s)))

(defmacro with-input-file ((var pathname &rest keys &key element-type external-format) &body body)
  (declare (ignore element-type external-format))
  `(call-with-input-file ,pathname #'(lambda (,var) ,@body) ,@keys))


;;; Using temporary files

(defun call-with-temporary-file
    (thunk &key
     prefix keep (direction :io)
     (element-type *default-element-type*)
     (external-format :default))
  (check-type direction (member :output :io))
  (loop
    :with prefix = (or prefix (format nil "~Axm" (native-namestring *temporary-directory*)))
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
                                prefix keep direction element-type external-format)
                               &body body)
  "Evaluate BODY where the symbols specified by keyword arguments
STREAM and PATHNAME are bound corresponding to a newly created temporary file
ready for I/O. Unless KEEP is specified, delete the file afterwards."
  (check-type stream symbol)
  (check-type pathname symbol)
  `(flet ((think (,stream ,pathname)
            ,@(unless pathnamep `((declare (ignore ,pathname))))
            ,@(unless streamp `((when ,stream (close ,stream))))
            ,@body))
     #-gcl (declare (dynamic-extent #'think))
     (call-with-temporary-file
      #'think
      ,@(when direction `(:direction ,direction))
      ,@(when prefix `(:prefix ,prefix))
      ,@(when keep `(:keep ,keep))
      ,@(when element-type `(:element-type ,element-type))
      ,@(when external-format `(:external-format external-format)))))


;;; Reading helpers

(defmacro with-safe-io-syntax ((&key (package :cl)) &body body)
  "Establish safe CL reader options around the evaluation of BODY"
  `(call-with-safe-io-syntax (lambda () ,@body) :package ,package))

(defun call-with-safe-io-syntax (thunk &key (package :cl))
  (with-standard-io-syntax ()
    (let ((*package* (find-package package))
          (*print-readably* nil)
	  (*read-eval* nil))
      (funcall thunk))))

(defun read-function (string)
  "Read a form from a string in function context, return a function"
  (eval `(function ,(read-from-string string))))

(defun read-first-file-form (pathname &key (package :cl) eof-error-p eof-value)
  "Reads the first form from the top of a file"
  (with-safe-io-syntax (:package package)
    (with-input-file (in pathname)
      (read in eof-error-p eof-value))))


;;; String utilities

(defun string-prefix-p (prefix string)
  "Does STRING begin with PREFIX?"
  (let* ((x (string prefix))
         (y (string string))
         (lx (length x))
         (ly (length y)))
    (and (<= lx ly) (string= x y :end2 lx))))

(defun string-suffix-p (string suffix)
  "Does STRING end with SUFFIX?"
  (let* ((x (string string))
         (y (string suffix))
         (lx (length x))
         (ly (length y)))
    (and (<= ly lx) (string= x y :start1 (- lx ly)))))

(defun string-enclosed-p (prefix string suffix)
  "Does STRING begin with PREFIX and end with SUFFIX?"
  (and (string-prefix-p prefix string)
       (string-suffix-p string suffix)))


;;;; Slurping streams

(defun copy-stream-to-stream (input output &key (element-type 'character))
  "Copy the contents of the INPUT stream into the OUTPUT stream,
using WRITE-SEQUENCE and a sensibly sized buffer."
  (with-open-stream (input input)
    (loop :with length = 8192
      :for buffer = (make-array length :element-type element-type)
      :for end = (read-sequence buffer input)
      :until (zerop end)
      :do (write-sequence buffer output :end end)
      :do (when (< end length) (return)))))

(defun copy-stream-to-stream-line-by-line (input output &key prefix)
  "Copy the contents of the INPUT stream into the OUTPUT stream,
reading contents line by line."
  (with-open-stream (input input)
    (loop :for (line eof) = (multiple-value-list (read-line input nil nil))
      :while line :do
      (when prefix (princ prefix output))
      (princ line output)
      (unless eof (terpri output))
      (finish-output output)
      (when eof (return)))))

(defun slurp-stream-string (input &key (element-type 'character))
  "Read the contents of the INPUT stream as a string"
  (with-open-stream (input input)
    (with-output-to-string (output)
      (copy-stream-to-stream input output :element-type element-type))))

(defun slurp-stream-lines (input)
  "Read the contents of the INPUT stream as a list of lines"
  (with-open-stream (input input)
    (loop :for l = (read-line input nil nil) :while l :collect l)))

(defun slurp-stream-forms (input)
  "Read the contents of the INPUT stream as a list of forms"
  (with-open-stream (input input)
    (loop :with eof = '#:eof
      :for form = (read input nil eof)
      :until (eq form eof) :collect form)))

(defun slurp-file-string (file &rest keys)
  "Open FILE with option KEYS, read its contents as a string"
  (apply 'call-with-input-file file 'slurp-stream-string keys))

(defun slurp-file-lines (file &rest keys)
  "Open FILE with option KEYS, read its contents as a list of lines"
  (apply 'call-with-input-file file 'slurp-stream-lines keys))

(defun slurp-file-forms (file &rest keys)
  "Open FILE with option KEYS, read its contents as a list of forms"
  (apply 'call-with-input-file file 'slurp-stream-forms keys))


;;;; ----- Current directory -----
;; TODO: make it work on all supported implementations

(defun getcwd ()
  "Get the current working directory as per POSIX getcwd(3)"
  (or #+clisp (ext:default-directory)
      #+clozure (ccl:current-directory)
      #+cormanlisp (pl::get-current-directory)
      #+mkcl (mk-ext:getcwd)
      #+sbcl (sb-unix:posix-getcwd/)
      (error "getcwd not supported on your implementation")))

(defun chdir (x)
  "Change current directory, as per POSIX chdir(2)"
  #-(or clisp clozure) (when (pathnamep x) (setf x (native-namestring x)))
  (or #+clisp (ext:cd x)
      #+clozure (setf (ccl:current-directory) x)
      #+cormanlisp (unless (zerop (win32::_chdir x))
                     (error "Could not set current directory to ~A" x))
      #+sbcl (sb-posix:chdir x)
      (error "chdir not supported on your implementation")))

(defun call-with-current-directory (dir thunk)
  (if dir
      (let* ((dir (truename (merge-pathnames (pathname-directory-pathname dir))))
             (*default-pathname-defaults* dir)
             (cwd (getcwd)))
        (chdir dir)
        (unwind-protect
             (funcall thunk)
          (chdir cwd)))
      (funcall thunk)))

(defmacro with-current-directory ((dir) &body body)
  "Call BODY while the POSIX current working directory is set to DIR"
  `(call-with-current-directory ,dir #'(lambda () ,@body)))


;;;; ---- Build and Execution control ----

;;; Optimization settings

(defvar *previous-optimization-settings* nil)
(defun get-optimization-settings ()
  "Get current compiler optimization settings, ready to PROCLAIM again"
  (let ((settings '(speed space safety debug compilation-speed #+(or cmu scl) c::brevity)))
    #-(or clisp clozure cmu sbcl scl)
    (warn "xcvb-driver::get-optimization-settings does not support your implementation. Please help me fix that.")
    #.`(loop :for x :in settings
         ,@(or #+clozure '(:for v :in '(ccl::*nx-speed* ccl::*nx-space* ccl::*nx-safety* ccl::*nx-debug* ccl::*nx-cspeed*))
               #+(or cmu scl) '(:for f :in '(c::cookie-speed c::cookie-space c::cookie-safety c::cookie-debug c::cookie-cspeed c::cookie-brevity)))
         :for y = (or #+clisp (gethash x system::*optimize*)
                      #+clozure (symbol-value v)
                      #+(or cmu scl) (funcall f c::*default-cookie*)
                      #+sbcl (cdr (assoc x sb-c::*policy*)))
         :when y :collect (list x y))))
(defun proclaim-optimization-settings ()
  "Proclaim the optimization settings in *OPTIMIZATION-SETTINGS*"
  (proclaim `(optimize ,@*optimization-settings*))
  (let ((settings (get-optimization-settings)))
    (unless (equal *previous-optimization-settings* settings)
      (setf *previous-optimization-settings* settings)
      (when (>= *xcvb-verbosity* 8)
	(format! *error-output* "~&Optimization settings: ~S~%" settings)))))

;;; Performance tweaks

(defun tweak-implementation ()
  "Common performance tweaks for various CL implementations."
  #+sbcl
  (progn
    ;; add ample margin between GC's: 400 MiB
    (setf (sb-ext:bytes-consed-between-gcs) (* 400 1024 1024))
    ;; add ample margin for *next* GC: 200 MiB
    (incf (sb-alien:extern-alien "auto_gc_trigger" sb-alien:long) (* 200 1024 1024))
    #|(sb-ext:gc :full t)|#)
  #+clozure
  (progn
    (ccl::configure-egc 32768 65536 98304)
    (ccl::set-lisp-heap-gc-threshold (* 384 1024 1024))
    (ccl::use-lisp-heap-gc-threshold)
    #|(ccl:gc)|#)
  nil)

;;; Debugging

(defun debugging (&optional (debug t))
  "Enable (or with NIL argument, disable) verbose debugging output from XCVB"
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

(defun print-backtrace (out)
  "Print a backtrace (implementation-defined)"
  (declare (ignorable out))
  #+clozure (let ((*debug-io* out))
	      (ccl:print-call-history :count 100 :start-frame-number 1)
	      (finish-output out))
  #+sbcl
  (sb-debug:backtrace
   #.(if (find-symbol* "*VERBOSITY*" "SB-DEBUG" nil) :stream 'most-positive-fixnum)
   out))

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
  "Macro to run a BODY of code, and
profile it under some profiling name when *PROFILING* is enabled."
  `(call-with-maybe-profiling #'(lambda () ,@body) ,what *goal*))

;;; Exiting properly or im-
(defun quit (&optional (code 0) (finish-output t))
  "Quits from the Lisp world, with the given exit status if provided.
This is designed to abstract away the implementation specific quit forms."
  (with-safe-io-syntax ()
    (when *debugging*
      (ignore-errors (format! *stderr* "~&Quitting with code ~A~%" code)))
    (when finish-output ;; essential, for ClozureCL, and for standard compliance.
      (ignore-errors (finish-outputs))))
  #+(or abcl xcl) (ext:quit :status code)
  #+allegro (excl:exit code :quiet t)
  #+clisp (ext:quit code)
  #+clozure (ccl:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+(or cmu scl) (unix:unix-exit code)
  #+ecl (si:quit code)
  #+gcl (lisp:quit code)
  #+genera (error "You probably don't want to Halt the Machine.")
  #+lispworks (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
  #+mcl (ccl:quit) ;; or should we use FFI to call libc's exit(3) ?
  #+mkcl (mk-ext:quit :exit-code code)
  #+sbcl #.(let ((exit (find-symbol* :exit :sb-ext nil))
		 (quit (find-symbol* :quit :sb-ext nil)))
	     (cond
	       (exit `(,exit :code code :abort (not finish-output)))
	       (quit `(,quit :unix-status code :recklessly-p (not finish-output)))))
  #-(or abcl allegro clisp clozure cmu ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (error "xcvb driver: Quitting not implemented"))

(defun shell-boolean (x)
  "Quit with a return code that is 0 iff argument X is true"
  (quit (if x 0 1)))

(defun die (format &rest arguments)
  "Die in error with some error message"
  (with-safe-io-syntax ()
    (ignore-errors
     (format! *stderr* "~&")
     (apply #'format! *stderr* format arguments)
     (format! *stderr* "~&"))
    (quit 99)))

(defun bork (condition)
  "Depending on whether *DEBUGGING* is set, enter debugger or die"
  (with-safe-io-syntax ()
    (ignore-errors (format! *stderr* "~&BORK:~%~A~%" condition)))
  (cond
    (*debugging*
     (invoke-debugger condition))
    (t
     (with-safe-io-syntax ()
       (ignore-errors (print-backtrace *stderr*)))
     (die "~A" condition))))

(defun call-with-coded-exit (thunk)
  (handler-bind ((error #'bork))
    (funcall thunk)
    (quit 0)))

(defmacro with-coded-exit ((&optional) &body body)
  "Run BODY, BORKing on error and otherwise exiting with a success status"
  `(call-with-coded-exit #'(lambda () ,@body)))


;;;; ----- Pathname mappings -----
;; TODO: make it work, test it.

(defvar *pathname-mappings* (make-hash-table :test 'equal)
  "Mappings from xcvb fullname to plist of
 (physical) :pathname, :logical-pathname, :tthsum digest, etc.")

(defun register-fullname (&key fullname pathname tthsum logical-pathname)
  (setf (gethash fullname *pathname-mappings*)
        (list :truename (truename (merge-pathnames pathname))
              :pathname pathname :logical-pathname logical-pathname
              :tthsum tthsum))
  (values))
(defun register-fullnames (mappings &key (defaults *load-truename*))
  (let ((*default-pathname-defaults*
         (or (and defaults (truename (pathname-directory-pathname defaults)))
             *default-pathname-defaults*)))
    (dolist (m mappings)
      (apply 'register-fullname m))))
(defun registered-fullname-pathname (fullname)
  (let ((plist (gethash fullname *pathname-mappings*)))
    (or (getf plist :logical-pathname) (getf plist :truename))))
(defun load-fullname-mappings (file)
  (let ((tn (truename file)))
    (register-fullnames (read-first-file-form tn) :defaults tn)))


;;;; ----- Filtering conditions while building -----

(defun match-condition-p (x condition)
  "Compare received CONDITION to some pattern X:
a symbol naming a condition class,
a simple vector of length 2, arguments to find-symbol* with result as above,
or a string describing the format-control of a simple-condition."
  (etypecase x
    (symbol (typep condition x))
    ((simple-vector 2) (typep condition (find-symbol* (svref x 0) (svref x 1) nil)))
    (function (funcall x condition))
    (string (and (typep condition 'simple-condition)
                 #+(or clozure cmu scl) ; Note: on SBCL, always bound, and testing triggers warning
		 (slot-boundp condition
			      #+clozure 'ccl::format-control
			      #+(or cmu scl) 'conditions::format-control)
                 (ignore-errors (equal (simple-condition-format-control condition) x))))))

(defun match-any-condition-p (condition conditions)
  "match CONDITION against any of the patterns of CONDITIONS supplied"
  (loop :for x :in conditions :thereis (match-condition-p x condition)))

(defun uninteresting-condition-p (condition)
  "match CONDITION against any of the patterns of *UNINTERESTING-CONDITIONS*"
  (match-any-condition-p condition *uninteresting-conditions*))

(defun fatal-condition-p (condition)
  "match CONDITION against any of the patterns of *FATAL-CONDITIONS*"
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

(defmacro with-controlled-compiler-conditions ((&optional) &body body)
  "Run BODY while suppressing conditions patterned after *UNINTERESTING-CONDITIONS*"
  `(call-with-controlled-compiler-conditions #'(lambda () ,@body)))

(defun call-with-controlled-loader-conditions (thunk)
  (let ((*uninteresting-conditions*
         (append
          *uninteresting-load-conditions*
          *uninteresting-conditions*)))
    (call-with-controlled-compiler-conditions thunk)))

(defmacro with-controlled-loader-conditions ((&optional) &body body)
  "Run BODY while suppressing conditions patterned after *UNINTERESTING-CONDITIONS* plus a few others that don't matter at load-time."
  `(call-with-controlled-loader-conditions #'(lambda () ,@body)))

(defun save-forward-references (forward-references)
  "Save forward reference conditions so they may be issued at a latter time,
possibly in a different process."
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
  "Like WITH-COMPILATION-UNIT, but saving forward-reference issues
for processing later (possibly in a different process)."
  `(call-with-xcvb-compilation-unit #'(lambda () ,@body) :forward-references ,forward-references))


;;;; ----- The xcvb-driver-command DSL for building Lisp code -----

(defun function-for-command (designator)
  (fdefinition (find-symbol* designator :xcvb-driver)))

(defun run-command (command)
  "Run a single command.
Entry point for XCVB-DRIVER when used by XCVB's farmer"
  (proclaim-optimization-settings)
  (multiple-value-bind (head args)
      (etypecase command
        (symbol (values command nil))
        (cons (values (car command) (cdr command))))
    (apply (function-for-command head) args)))

(defun run-commands (commands)
  (map () #'run-command commands))

(declaim (ftype (function () (values)) setup-environment))

(defun do-run (commands)
  (let ((*stderr* *error-output*))
    (setup-environment)
    (run-commands commands)))

(defmacro run (&rest commands)
  "Run a series of XCVB-DRIVER commands, then exit.
Entry point for XCVB-DRIVER when used by XCVB"
  `(with-coded-exit ()
    (do-run ',commands)))


;;;; ----- Simple build commands -----

;;; Loading and evaluating code

(defun do-load (x &key encoding)
  (with-controlled-loader-conditions ()
    (load x
	  :external-format (encoding-external-format encoding)
	  :verbose (>= *xcvb-verbosity* 8)
	  :print (>= *xcvb-verbosity* 9))))

(defun load-file (x &key encoding)
  (with-profiling `(:load-file ,x :encoding ,encoding)
    (unless (do-load x :encoding encoding)
      (error "Failed to load ~A" (list x)))))

(defun eval-string (string)
  "Evaluate a form read from a string"
  (with-controlled-loader-conditions ()
    (eval (read-from-string string))))

(defun cl-require (x)
  (with-profiling `(:require ,x)
    (require x)))

(defun load-stream (&optional (stream *standard-input*))
  "Portably read and evaluate forms from a STREAM."
  ;; GCL 2.6 can't load from a string-input-stream
  ;; ClozureCL 1.6 can only load from file input
  ;; Allegro 5, I don't remember but it must have been broken when I tested.
  #+(or gcl-pre2.7 clozure allegro)
  (with-controlled-loader-conditions ()
    (do ((eof '#:eof) (x t (read stream nil eof))) ((eq x eof)) (eval x)))
  #-(or gcl-pre2.7 clozure allegro)
  (do-load stream))

(defun load-string (string)
  "Portably read and evaluate forms from a STRING."
  (with-input-from-string (s string) (load-stream s)))


;;; ASDF support

(defun asdf-symbol (x)
  (find-symbol* x :asdf))

(defun load-asdf (x &key parallel (verbose *compile-verbose*)) ;; parallel loading requires POIU
  (with-profiling `(:asdf ,x)
    (with-controlled-loader-conditions ()
      (call :asdf :operate
            (asdf-symbol (if parallel :parallel-load-op :load-op))
            x :verbose verbose))))

(defparameter *asdf-version-required-for-xcvb* "2.019")

(defun require-asdf ()
  (funcall 'require "asdf") ;; work around CLISP annoyance
  (load-asdf :asdf) ;; upgrade early, avoid issues.
  (let ((required *asdf-version-required-for-xcvb*))
    (unless (call :asdf :version-satisfies (call :asdf :asdf-version) required)
      (error "XCVB requires ASDF ~A or later" required))))

(defun initialize-asdf ()
  (require-asdf)
  (call :asdf :clear-configuration))

(defun register-asdf-directory (x)
  (pushnew x (symbol-value (asdf-symbol :*central-registry*))))

(defun asdf-system-needs-compilation-p (system)
  "Takes a name of an asdf system (or the system itself) and a asdf operation
  and returns a boolean indicating whether or not anything needs to be done
  in order to perform the given operation on the given system.
  This returns whether or not the operation has already been performed,
  and none of the source files in the system have changed since then"
  (progv
      (list (asdf-symbol :*verbose-out*))
      (list (make-broadcast-stream))
    (let* ((op (make-instance (asdf-symbol :load-op)))
           (system (call :asdf :find-system system))
           (steps (call :asdf :traverse op system)))
      (and (member (asdf-symbol :compile-op) steps
                   :key (lambda (x) (type-of (car x)))) t))))

(defun asdf-systems-up-to-date-p (systems)
  "Takes a list of names of asdf systems, and
  exits lisp with a status code indicating
  whether or not all of those systems were up-to-date or not."
  (notany #'asdf-system-needs-compilation-p systems))

(defun asdf-systems-up-to-date (&rest systems)
  "Are all the loaded systems up to date?"
  (with-coded-exit ()
    (shell-boolean (asdf-systems-up-to-date-p systems))))


;;; Actually compiling

(defmacro with-determinism (goal &body body)
  "Attempt to recreate deterministic conditions for the building a component."
  `(call-with-determinism ,goal #'(lambda () ,@body)))

(defun seed-random-state (seed) ; seed is a integer
  (declare (ignorable seed))
  #+sbcl (sb-ext:seed-random-state seed)
  #+clozure
  (flet ((get-bits (&aux bits)
           (multiple-value-setq (seed bits) (floor seed ccl::mrg31k3p-limit))
           bits))
    (multiple-value-bind (x0 x1 x2 x3 x4 x5)
        (apply 'values (loop :repeat 6 :collect (get-bits)))
      (when (zerop (logior x0 x1 x2))
        (setf x0 (logior (get-bits) 1)))
      (when (zerop (logior x3 x4 x5))
        (setf x3 (logior (get-bits) 1)))
      (ccl::initialize-mrg31k3p-state x0 x1 x2 x3 x4 x5)))
  #-(or sbcl clozure) (make-random-state *initial-random-state*))

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
  ;;; (def!struct (source-info ...)) from main/compiler.lisp (package SB-C),
  ;;; and override the source location to point to some logical pathname.
  (let* ((hash (sxhash seed))
         (*gensym-counter* (* hash 10000))
         #+sbcl (sb-impl::*gentemp-counter* (* hash 10000))
         ;;; SBCL will hopefully export a better mechanism soon. See:
         ;;; https://bugs.launchpad.net/sbcl/+bug/310116
         (*random-state* (seed-random-state hash)))
    (funcall thunk)))

(defparameter *utf-8-external-format*
  #+(and xcvb-unicode (not clisp)) :utf-8
  #+(and xcvb-unicode clisp) charset:utf-8
  #-xcvb-unicode :default
  "Default :external-format argument to pass to CL:OPEN and also
CL:LOAD or CL:COMPILE-FILE to best process a UTF-8 encoded file.
On modern implementations, this will decode UTF-8 code points as CL characters.
On legacy implementations, it may fall back on some 8-bit encoding,
with non-ASCII code points being read as several CL characters;
hopefully, if done consistently, that won't affect program behavior too much.")

(defun encoding-external-format (encoding)
  (case encoding
    ((:utf-8 nil) *utf-8-external-format*) ;; Our recommended default.
    (:default :default) ;; for backwards compatibility only. Explicit usage discouraged.
    (otherwise
     (call :asdf-encodings :encoding-external-format encoding))))

(defun do-compile-lisp (dependencies source fasl
                        &key #+sbcl cfasl #+ecl lisp-object around-compile encoding)
  (let ((*goal* `(:compile-lisp ,source))
        (*default-pathname-defaults* (truename *default-pathname-defaults*)))
    (multiple-value-bind (output-truename warnings-p failure-p)
        (with-profiling `(:preparing-and-compiling ,source)
          (with-xcvb-compilation-unit ()
            (with-profiling `(:preparing-compilation-of ,source)
              (run-commands dependencies))
            (with-profiling `(:compiling ,source)
              (with-determinism `(:compiling ,source)
                (multiple-value-prog1
                    ((lambda (thunk)
                       (if around-compile
                           (funcall (read-function around-compile) thunk)
                           (funcall thunk)))
                     (lambda ()
                       (apply #'compile-file source
                           :output-file (merge-pathnames (or #+ecl lisp-object fasl))
			   :external-format (encoding-external-format encoding)
                           (append
                            #+sbcl (when cfasl `(:emit-cfasl ,(merge-pathnames cfasl)))
                            #+ecl (when lisp-object '(:system-p t))))))
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
      #-(or clisp cmu ecl)
      (when warnings-p
        (die "Compilation Warned for ~A" source))))
  (values))

(defun compile-lisp (spec &rest dependencies)
  (apply 'do-compile-lisp dependencies spec))


;;;; ----- Dumping an image and running it -----

;;; Resuming from an image with proper command-line arguments

(defvar *arguments* nil
  "Command-line arguments")

(defvar *dumped* nil
  "Is this a dumped image? As a standalone executable?")

(defun raw-command-line-arguments ()
  "Find what the actual command line for this process was."
  #+abcl ext:*command-line-argument-list* ; Use 1.0.0 or later!
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

(defun command-line-arguments (&optional (arguments (raw-command-line-arguments)))
  "Extract user arguments from command-line invocation of current process.
Assume the calling conventions of an XCVB-generated script
if we are not called from a directly executable image dumped by XCVB."
  #+abcl arguments
  #-abcl
  (let* (#-(or sbcl allegro)
	 (arguments
	  (if (eq *dumped* :executable)
	      arguments
	      (member "--" arguments :test 'string-equal))))
    (rest arguments)))

(defun do-resume (&key (post-image-restart *post-image-restart*) (entry-point *entry-point*))
  (with-safe-io-syntax ()
    (let ((*read-eval* t))
      (when post-image-restart (load-string post-image-restart))))
  (with-coded-exit ()
    (when entry-point
      (let ((ret (apply entry-point *arguments*)))
	(if (typep ret 'integer)
	    (quit ret)
	    (quit 99))))))

(defun resume ()
  (setf *arguments* (command-line-arguments))
  (do-resume))

;;; Dumping an image

#-ecl
(defun dump-image (filename &key output-name executable pre-image-dump post-image-restart entry-point package)
  (declare (ignorable filename output-name executable pre-image-dump post-image-restart entry-point))
  (setf *dumped* (if executable :executable t))
  (setf *package* (find-package (or package :cl-user)))
  (with-safe-io-syntax ()
    (let ((*read-eval* t))
      (when pre-image-dump (load-string pre-image-dump))
      (setf *entry-point* (when entry-point (read-function entry-point)))
      (when post-image-restart (setf *post-image-restart* post-image-restart))))
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

;;; DSL entry point to create images
#-ecl
(defun do-create-image (image dependencies &rest flags)
  (let ((*goal* `(create-image ,image))
        #+sbcl (*uninteresting-conditions*
                (cons "undefined ~(~A~): ~S" *uninteresting-conditions*)))
    (with-controlled-compiler-conditions ()
      (run-commands dependencies))
    (apply #'dump-image image flags)))

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
    (multiple-value-bind (object-files manifest)
        (case (first first-dep)
          ((:load-manifest)
           (assert (null (rest dependencies)))
           (let ((manifest (read-first-file-form (second first-dep))))
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


;;;; ----- CFFI-grovel support -----

(defun process-cffi-grovel-file (input c exe output &key cc-flags)
  (destructuring-bind (input c exe output)
      (mapcar 'fullname-pathname (list input c exe output))
    (with-current-directory (exe)
      (progv (list (find-symbol* :*cc-flags* :cffi-grovel)) (list cc-flags)
        (call :cffi-grovel :generate-c-file input c)
        (call :cffi-grovel :cc-compile-and-link c exe)
        (call :cffi-grovel :invoke exe output)))))

(defun process-cffi-wrapper-file (input c so output &key cc-flags)
  (declare (ignore output)); see below
  (flet ((f (x) (native-namestring (merge-pathnames x))))
    (let* ((input (f input))
           (c (f c))
           (so (f so))
           ;;(output (f output))
           (*default-pathname-defaults* (pathname-directory-pathname so)))
      (progv (list (find-symbol* :*cc-flags* :cffi-grovel)) (list cc-flags)
        (with-safe-io-syntax ()
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
(defvar +xcvb-slave-greeting+ #.(format nil "Dear Master, here are your build commands:~%"))
(defvar +xcvb-slave-farewell+ #.(format nil "~%Your desires are my orders, sincerely, XCVB.~%"))


;;;; ----- Escaping strings for the shell -----

(defun requires-escaping-p (token &key good-chars bad-chars)
  "Does this token require escaping, given the specification of
either good chars that don't need escaping or bad chars that do need escaping,
as either a recognizing function or a sequence of characters."
  (some
   (cond
     ((and good-chars bad-chars)
      (error "only one of good-chars and bad-chars can be provided"))
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

(defun output-string (string &optional stream)
  (if stream
      (with-output (stream) (princ string stream))
      string))

(defun escape-token (token &key stream quote good-chars bad-chars escaper)
  "Call the ESCAPER function on TOKEN string if it needs escaping as per
REQUIRES-ESCAPING-P using GOOD-CHARS and BAD-CHARS, otherwise output TOKEN,
using STREAM as output (or returning result as a string if NIL)"
  (if (requires-escaping-p token :good-chars good-chars :bad-chars bad-chars)
      (with-output (stream)
        (apply escaper token stream (when quote `(:quote ,quote))))
      (output-string token stream)))

(defun escape-windows-token-within-double-quotes (x &optional s)
  "Escape a string token X within double-quotes
for use within a MS Windows command-line, outputing to S."
  (labels ((issue (c) (princ c s))
           (issue-backslash (n) (loop :repeat n :do (issue #\\))))
    (loop
      :initially (issue #\") :finally (issue #\")
      :with l = (length x) :with i = 0
      :for i+1 = (1+ i) :while (< i l) :do
      (case (char x i)
        ((#\") (issue-backslash 1) (issue #\") (setf i i+1))
        ((#\\)
         (let* ((j (and (< i+1 l) (position-if-not
                                   (lambda (c) (eql c #\\)) x :start i+1)))
                (n (- (or j l) i)))
           (cond
             ((null j)
              (issue-backslash (* 2 n)) (setf i l))
             ((and (< j l) (eql (char x j) #\"))
              (issue-backslash (1+ (* 2 n))) (issue #\") (setf i (1+ j)))
             (t
              (issue-backslash n) (setf i j)))))
        (otherwise
         (issue (char x i)) (setf i i+1))))))

(defun escape-windows-token (token &optional s)
  "Escape a string TOKEN within double-quotes if needed
for use within a MS Windows command-line, outputing to S."
  (escape-token token :stream s :bad-chars #(#\space #\tab #\") :quote nil
                :escaper 'escape-windows-token-within-double-quotes))

(defun escape-sh-token-within-double-quotes (x s &key (quote t))
  "Escape a string TOKEN within double-quotes
for use within a POSIX Bourne shell, outputing to S;
omit the outer double-quotes if key argument :QUOTE is NIL"
  (when quote (princ #\" s))
  (loop :for c :across x :do
    (when (find c "$`\\\"") (princ #\\ s))
    (princ c s))
  (when quote (princ #\" s)))

(defun easy-sh-character-p (x)
  (or (alphanumericp x) (find x "+-_.,%@:/")))

(defun escape-sh-token (token &optional s)
  "Escape a string TOKEN within double-quotes if needed
for use within a POSIX Bourne shell, outputing to S."
  (escape-token token :stream s :quote #\" :good-chars
                #'easy-sh-character-p
                :escaper 'escape-sh-token-within-double-quotes))

(defun escape-shell-token (token &optional s)
  (cond
    ((os-unix-p) (escape-sh-token token s))
    ((os-windows-p) (escape-windows-token token s))))

(defun escape-command (command &optional s
                       (escaper 'escape-shell-token))
  "Given a COMMAND as a list of tokens, return a string of the
spaced, escaped tokens, using ESCAPER to escape."
  (etypecase command
    (string (output-string command s))
    (list (with-output (s)
            (loop :for first = t :then nil :for token :in command :do
              (unless first (princ #\space s))
              (funcall escaper token s))))))

(defun escape-windows-command (command &optional s)
  "Escape a list of command-line arguments into a string suitable for parsing
by CommandLineToArgv in MS Windows"
    ;; http://msdn.microsoft.com/en-us/library/bb776391(v=vs.85).aspx
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft(v=vs.85).aspx
  (escape-command command s 'escape-windows-token))

(defun escape-sh-command (command &optional s)
  "Escape a list of command-line arguments into a string suitable for parsing
by /bin/sh in POSIX"
  (escape-command command s 'escape-sh-token))

(defun escape-shell-command (command &optional stream)
  "Escape a command for the current operating system's shell"
  (escape-command command stream 'escape-shell-token))

;;;; ----- Running an external program -----
;;; Simple variant of run-program with no input, and capturing output
;;; On some implementations, may output to a temporary file...

(defgeneric slurp-input-stream (processor input-stream &key &allow-other-keys))

(defmethod slurp-input-stream ((function function) input-stream &key &allow-other-keys)
  (funcall function input-stream))

(defmethod slurp-input-stream ((list cons) input-stream &key &allow-other-keys)
  (apply (first list) (cons input-stream (rest list))))

(defmethod slurp-input-stream ((output-stream stream) input-stream
                               &key (element-type 'character) &allow-other-keys)
  (copy-stream-to-stream
   input-stream output-stream :element-type element-type))

(defmethod slurp-input-stream ((x (eql 'string)) stream &key &allow-other-keys)
  (declare (ignorable x))
  (slurp-stream-string stream))

(defmethod slurp-input-stream ((x (eql :string)) stream &key &allow-other-keys)
  (declare (ignorable x))
  (slurp-stream-string stream))

(defmethod slurp-input-stream ((x (eql :lines)) stream &key &allow-other-keys)
  (declare (ignorable x))
  (slurp-stream-lines stream))

(defmethod slurp-input-stream ((x (eql :form)) stream &key &allow-other-keys)
  (declare (ignorable x))
  (read stream))

(defmethod slurp-input-stream ((x (eql :forms)) stream &key &allow-other-keys)
  (declare (ignorable x))
  (slurp-stream-forms stream))

(define-condition subprocess-error (error)
  ((code :initform nil :initarg :code :reader subprocess-error-code)
   (command :initform nil :initarg :command :reader subprocess-error-command)
   (process :initform nil :initarg :process :reader subprocess-error-process))
  (:report (lambda (condition stream)
             (format stream "Subprocess~@[ ~S~]~@[ run with command ~S~] exited with error~@[ code ~D~]"
                     (subprocess-error-process condition)
                     (subprocess-error-command condition)
                     (subprocess-error-code condition)))))

(defun run-program/ (command
                     &rest keys
                     &key output ignore-error-status force-shell
                     (element-type *default-element-type*)
                     (external-format :default)
                     &allow-other-keys)
  "Run program specified by COMMAND,
either a list of strings specifying a program and list of arguments,
or a string specifying a shell command (/bin/sh on Unix, CMD.EXE on Windows);
have its output processed by the OUTPUT processor function
as per SLURP-INPUT-STREAM,
or merely output to the inherited standard output if it's NIL.
Always call a shell (rather than directly execute the command)
if FORCE-SHELL is specified.
Issue an error if the process wasn't successful unless IGNORE-ERROR-STATUS
is specified.
Return the exit status code of the process that was called.
Use ELEMENT-TYPE and EXTERNAL-FORMAT for the stream passed to the OUTPUT processor."
  (declare (ignorable ignore-error-status element-type external-format))
  (let ((s (find-symbol* 'run-program/ :quux-iolib nil)))
    (when s (return-from run-program/ (apply s command keys))))
  #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl lispworks mcl sbcl scl xcl)
  (error "RUN-PROGRAM/PROCESS-OUTPUT-STREAM not implemented for this Lisp")
  (labels (#+(or allegro clisp clozure cmu ecl (and lispworks os-unix) sbcl scl)
           (run-program (command &key pipe interactive)
             "runs the specified command (a list of program and arguments).
              If using a pipe, returns two values: process and stream
              If not using a pipe, returns one values: the process result;
              also, inherits the output stream."
             ;; NB: these implementations have unix vs windows set at compile-time.
	     (assert (not (and pipe interactive)))
             (let* ((wait (not pipe))
                    #-(and clisp os-windows)
                    (command
                     (etypecase command
                       #+os-unix (string `("/bin/sh" "-c" ,command))
                       #+os-unix (list command)
                       #+os-windows
                       (string
                        ;; NB: We do NOT add cmd /c here. You might want to.
                        #+allegro command
			;; On ClozureCL for Windows, we assume you are using
			;; r15398 or later in 1.9 or later,
			;; so that bug 858 is fixed http://trac.clozure.com/ccl/ticket/858
			#+clozure (cons "cmd" (strcat "/c " command))
                        ;; NB: On other Windows implementations, this is utterly bogus
                        ;; except in the most trivial cases where no quoting is needed.
                        ;; Use at your own risk.
                        #-(or allegro clozure) (list "cmd" "/c" command))
                       #+os-windows
                       (list
                        #+(or allegro clozure) (escape-windows-command command)
                        #-(or allegro clozure) command)))
                    #+(and clozure os-windows) (command (list command))
                    (process*
                     (multiple-value-list
                      #+allegro
                      (excl:run-shell-command
                       #+os-unix (coerce (cons (first command) command) 'vector)
                       #+os-windows command
                       :input interactive :output (or (and pipe :stream) interactive) :wait wait
                       #+os-windows :show-window #+os-windows (and pipe :hide))
                      #+clisp
                      (flet ((run (f &rest args)
                               (apply f `(,@args :input ,(when interactive :terminal) :wait ,wait :output
                                          ,(if pipe :stream :terminal)))))
                        (etypecase command
                          #+os-windows (run 'ext:run-shell-command command)
                          (list (run 'ext:run-program (car command)
                                     :arguments (cdr command)))))
                      #+lispworks
                      (system:run-shell-command
                       (cons "/usr/bin/env" command) ; lispworks wants a full path.
                       :input interactive :output (or (and pipe :stream) interactive)
                       :wait wait :save-exit-status (and pipe t))
                      #+(or clozure cmu ecl sbcl scl)
                      (#+(or cmu ecl scl) ext:run-program
                       #+clozure ccl:run-program
                       #+sbcl sb-ext:run-program
                       (car command) (cdr command)
                       :input interactive :wait wait
                       :output (if pipe :stream t)
                       . #.(append
                            #+(or clozure cmu ecl sbcl scl) '(:error t)
                            #+sbcl '(:search t
                                     #|:external-format external-format ; not in old SBCLs|#)))))
                    (process
                     #+(or allegro lispworks) (if pipe (third process*) (first process*))
                     #+ecl (third process*)
                     #-(or allegro lispworks ecl) (first process*))
                    (stream
                     (when pipe
                       #+(or allegro lispworks ecl) (first process*)
                       #+clisp (first process*)
                       #+clozure (ccl::external-process-output process)
                       #+(or cmu scl) (ext:process-output process)
                       #+sbcl (sb-ext:process-output process))))
               (values process stream)))
           #+(or allegro clisp clozure cmu ecl (and lispworks os-unix) sbcl scl)
           (process-result (process)
             ;; 1- wait
             #+(and clozure os-unix) (ccl::external-process-wait process)
             #+(or cmu scl) (ext:process-wait process)
             #+(and ecl os-unix) (ext:external-process-wait process)
             #+sbcl (sb-ext:process-wait process)
             ;; 2- extract result
             #+allegro (sys:reap-os-subprocess :pid process :wait t)
             #+clisp process
             #+clozure (nth-value 1 (ccl:external-process-status process))
             #+(or cmu scl) (ext:process-exit-code process)
             #+ecl (nth-value 1 (ext:external-process-status process))
             #+lispworks (system:pid-exit-status process :wait t)
             #+sbcl (sb-ext:process-exit-code process))
           (check-result (exit-code process)
             #+clisp
             (setf exit-code
                   (typecase exit-code (integer exit-code) (null 0) (t -1)))
             (unless (or ignore-error-status
                         (equal exit-code 0))
               (error 'subprocess-error :command command :code exit-code :process process))
	     exit-code)
           (use-run-program ()
             #-(or abcl cormanlisp gcl (and lispworks os-windows) mcl xcl)
             (let* ((interactive (eq output :interactive))
		    (pipe (and output (not interactive))))
               (multiple-value-bind (process stream)
                   (run-program command :pipe pipe :interactive interactive)
                 (if (and output (not interactive))
                     (unwind-protect
                          (slurp-input-stream output stream)
                       (when stream (close stream))
                       (check-result (process-result process) process))
                     (unwind-protect
                          (check-result
                           #+(or allegro lispworks) ; when not capturing, returns the exit code!
                           process
                           #-(or allegro lispworks) (process-result process)
                           process))))))
           (system-command (command)
             (etypecase command
               (string (if (os-windows-p) (format nil "cmd /c ~A" command) command))
               (list (escape-shell-command
                      (if (os-unix-p) (cons "exec" command) command)))))
           (redirected-system-command (command out)
             (format nil (if (os-unix-p) "exec > ~*~A ; ~2:*~A" "~A > ~A")
                     (system-command command) (native-namestring out)))
           (system (command &key interactive)
             #+(or abcl xcl) (ext:run-shell-command command)
             #+allegro
             (excl:run-shell-command command :input interactive :output interactive :wait t)
             #+(or clisp clozure cmu (and lispworks os-unix) sbcl scl)
             (run-program command :pipe nil :interactive interactive)
             #+ecl (ext:system command)
             #+cormanlisp (win32:system command)
             #+gcl (lisp:system command)
             #+(and lispworks os-windows)
             (system:call-system-showing-output
              command :show-cmd interactive :prefix "" :output-stream nil)
             #+mcl (ccl::with-cstrs ((%command command)) (_system %command)))
           (call-system (command-string &key interactive)
             (check-result (system command-string :interactive interactive) nil))
           (use-system ()
	     (let ((interactive (eq output :interactive)))
	       (if (and output (not interactive))
		   (with-temporary-file (:pathname tmp :direction :output)
		     (call-system (redirected-system-command command tmp))
		     (with-open-file (stream tmp
					     :direction :input
					     :if-does-not-exist :error
					     :element-type element-type
					     :external-format external-format)
		       (slurp-input-stream output stream)))
		   (call-system (system-command command) :interactive interactive)))))
    (if (and (not force-shell)
             #+(or clisp ecl) ignore-error-status
             #+(or abcl cormanlisp gcl (and lispworks os-windows) mcl xcl) nil)
        (use-run-program)
        (use-system))))


;;;; ----- Common things to do with an external program -----

(defmacro run-program/process-output-stream (command output-processor &rest keys)
  (warn "run-program/process-output-stream has been superseded by run-program/")
  `(run-program/ ,command :output ,output-processor ,@keys))

(defmacro run-program/read-output-lines (command &rest keys)
  (warn "run-program/read-output-lines has been superseded by run-program/ ... :output :lines")
  `(run-program/ ,command :output :lines ,@keys))

(defmacro run-program/read-output-string (command &rest keys)
  (warn "run-program/read-output-string has been superseded by run-program/ ... :output :string")
  `(run-program/ ,command :output :string ,@keys))

(defmacro run-program/read-output-form (command &rest keys)
  (warn "run-program/read-output-form has been superseded by run-program/ ... :output :form")
  `(run-program/ ,command :output :form ,@keys))

(defmacro run-program/read-output-forms (command &rest keys)
  (warn "run-program/read-output-forms has been superseded by run-program/ ... :output :forms")
  `(run-program/ ,command :output :forms ,@keys))

(defmacro run-program/for-side-effects (command &rest keys)
  (warn "run-program/for-side-effects has been superseded by run-program/ ... :output nil")
  `(run-program/ ,command :output :forms ,@keys))

(defun run-program/echo-output (command &rest keys &key prefix (stream t) &allow-other-keys)
  (apply
   'run-program/ command
   :output `(copy-stream-to-stream-line-by-line ,stream :prefix ,prefix)
   keys))

;;;; ----- Manifest: representing how an image was built or is to be built -----

;;; Maintaining memory of which grains have been loaded in the current image.
;; TODO: fix brokenness. We need to distinguish
;; 1- either a grain or a virtual command that we issue, e.g. (:load-file (:fasl "/foo/bar"))
;; 2- the actual thing that the driver runs, e.g. (:load-file "/path/to/foo/bar.fasl")
;; The mapping can be done at one place or the other, but currently there's a big confusion!
(defun process-manifest-entry (&rest entry &key command pathname tthsum encoding &allow-other-keys)
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
       (load pathname
	     :external-format (encoding-external-format encoding)
	     :verbose (>= *xcvb-verbosity* 8)
	     :print (>= *xcvb-verbosity* 9)))
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

;;;; ----- XCVB automagic bootstrap: creating XCVB if not there yet -----
(defvar *xcvb-present* nil)

(defun default-xcvb-program ()
  (require-asdf)
  (native-namestring
   (call :asdf :subpathname (call :asdf :user-homedir)
         (format nil ".cache/common-lisp/bin/~(~A~@[-~A~]~)/xcvb"
                 (call :asdf :operating-system) (call :asdf :architecture)))))

(defun xcvb-present-p (&optional (program *xcvb-program*))
  ;; returns the resolved path to xcvb if present
  (or (and (equal program *xcvb-present*) program)
      (etypecase program
	((eql t) (and (find-package :xcvb) (setf *xcvb-present* t)))
	(string
         (and
          (string-prefix-p "XCVB version "
                           (run-program/
                            (list program "version")
                            :ignore-error-status t :output :string))
          (setf *xcvb-present* program)))
        (pathname
         (xcvb-present-p (native-namestring program))))
      (when (equal program "xcvb")
	(let ((default (default-xcvb-program)))
          (assert (not (equal default "xcvb")))
          (xcvb-present-p default)))
      (setf *xcvb-present* nil)))

(declaim (ftype (function (t) string) build-xcvb)) ; avoid warning on forward reference.

(defun create-xcvb-program (&optional (program *xcvb-program*))
  ;; Ugly: May side-effect *xcvb-program* to point to the resolved location of xcvb.
  (when (equal program "xcvb")
    (setf program (default-xcvb-program))
    (when (equal *xcvb-program* "xcvb")
      (setf *xcvb-program* program)))
  (require-asdf)
  (load-asdf :xcvb-bootstrap)
  (funcall 'build-xcvb program))

(defun require-xcvb ()
  (require-asdf)
  (call :asdf :load-system :xcvb)
  t)

(defun ensure-xcvb-present (&optional (program *xcvb-program*))
  ;; returns the resolved path to the xcvb binary
  (or (xcvb-present-p program)
      (etypecase program
        ((eql t) (require-xcvb))
        ((or string pathname) (create-xcvb-program program)))))


;;;; ----- XCVB master: calling XCVB -----
;;; Run a slave, obey its orders. (who's the master?)
;;; TODO: detect whether XCVB is installed or reachable, have fall back plan
;;;  1- fall back to executing a lisp that invokes asdf to bootstrap xcvb
;;;   (requires a merge of lisp-invocation into driver) (use SBCL? clisp? ccl?)
;;;  2- fall back to loading xcvb in the current image

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *bnl-keys-with-defaults*
    '((xcvb-program *xcvb-program*)
      (required-xcvb-version *required-xcvb-version*)
      (setup *xcvb-setup*)
      (source-registry *source-registry*)
      (output-path nil)
      (lisp-implementation *lisp-implementation-type*)
      (lisp-binary-path *lisp-executable-pathname*)
      (lisp-image-path *lisp-image-pathname*)
      (features-defined *features-defined*)
      (features-undefined *features-undefined*)
      (disable-cfasl *disable-cfasls*)
      (use-base-image *use-base-image*)
      (cache *cache*)
      (object-cache *object-cache*)
      (workspace *workspace*)
      (install-prefix *install-prefix*)
      (install-program *install-program*)
      (install-configuration *install-configuration*)
      (install-data *install-data*)
      (install-library *install-library*)
      (install-image *install-image*)
      (install-lisp *install-lisp*)
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
              (list (to-option-name ,var) (native-namestring ,var))))
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
       (list "slave-builder")
       (string-options build setup lisp-implementation source-registry
                       verbosity required-xcvb-version)
       (pathname-options output-path lisp-binary-path lisp-image-path
                         xcvb-program cache object-cache workspace
                         install-prefix install-program install-configuration
                         install-data install-library install-image install-lisp)
       (list-option-arguments "define-feature" features-defined)
       (list-option-arguments "undefine-feature" features-undefined)
       (boolean-options disable-cfasl use-base-image debugging profiling)))))

(defun run-xcvb-command (program command)
  (etypecase program
    (string
     ;; Ugly: rely on the above having side-effected *xcvb-program*
     (with-safe-io-syntax ()
       (run-program/
        (cons program command) :output :string :ignore-error-status t)))
    (pathname
     (run-xcvb-command (namestring program) command))
    ((eql t)
     (unless (find-symbol* :cmd :xvcb nil)
       (require-xcvb))
     (with-safe-io-syntax ()
       (with-output-to-string (*standard-output*)
         (apply 'call :xcvb :cmd command))))))

(defun build-in-slave (build &rest args &key . #.*bnl-keys-with-defaults*)
  "Entry point to call XCVB to build (but not necessarily load) a system."
  (declare (ignore . #.(set-difference *bnl-keys* '(xcvb-program verbosity))))
  (let* ((xcvb-program (ensure-xcvb-present xcvb-program))
         (slave-command (apply 'build-slave-command-line build :xcvb-program xcvb-program args))
         (slave-output (run-xcvb-command xcvb-program slave-command))
         (slave-greeting-pos (search +xcvb-slave-greeting+ slave-output :from-end t))
         (manifest
          (progn
            (unless (and slave-output
                         slave-greeting-pos
                         (string-suffix-p slave-output +xcvb-slave-farewell+))
              (format! *error-output*
                       "Failed to execute a build slave.~%~
			Slave command:~%  ~S~%~
			Slave output:~%~A~%~
			(If using SLIME, you might have useful error output in your *inferior-lisp* buffer~%in which case next time you may M-x slime-redirect-inferior-output.)"
                       slave-command slave-output)
              (error "XCVB slave failed"))
            (read-from-string
             slave-output t nil
             :start (+ (length +xcvb-slave-greeting+) slave-greeting-pos)
             :end (- (length slave-output) (length +xcvb-slave-farewell+)))))
         (*xcvb-verbosity* (+ (or verbosity *xcvb-verbosity*) 2)))
    (when (>= *xcvb-verbosity* 9)
      (format! *error-output* "~&Slave XCVB returned following manifest:~%~S~%" manifest))
    manifest))

(defun build-and-load (build &rest args &key . #.*bnl-keys*)
  "Entry point for users to call XCVB to build and load a system."
  (declare (ignore . #.*bnl-keys*))
  (process-manifest (apply 'build-in-slave build args)))

(defun bnl (build &rest keys &key . #.*bnl-keys*)
  "Short hand for BUILD-AND-LOAD"
  (declare (ignore . #.*bnl-keys*))
  (apply 'build-and-load build keys))

;;; Build initialization

(defun setup-environment ()
  "Setup the XCVB environment with respect to debugging, profiling, performance"
  (debugging (getenvp "XCVB_DEBUGGING"))
  (setf *profiling* (getenvp "XCVB_PROFILING")
	*temporary-directory* (default-temporary-directory)
	*stderr* #-clozure *error-output* #+clozure ccl::*stderr*)
  (tweak-implementation)
  (values))

;;;; ----- The End -----
