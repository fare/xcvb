;;;;; XCVB driver. Load it in your Lisp image and build with XCVB.

;;;; ----- Prelude -----
#+xcvb
(module
 (:description "XCVB Driver"
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT" ;; MIT-style license. See LICENSE
  :build-depends-on nil))

#.(setf *load-verbose* () *load-print* () *compile-verbose* () *compile-print* ()) ;; Hush!

;;;; First, try very hard to load a recent enough ASDF.

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *asdf-version-required-by-xcvb* "2.26.149")
  (defvar *asdf-directory*
    (merge-pathnames #p"cl/asdf/" (user-homedir-pathname))
    "Directory in which your favorite and/or latest version
     of the ASDF source code is located")
  (defun get-asdf-version ()
    (when (find-package :asdf)
      (let ((ver (symbol-value
                  (or (find-symbol (string :*asdf-version*) :asdf)
                      (find-symbol (string :*asdf-revision*) :asdf)))))
        (etypecase ver
          (string ver)
          (cons (format nil "~{~D~^.~}" ver))
          (null "1.0"))))))

;;; Doing our best to load ASDF
;; First, try loading asdf from your implementation.
;; Use funcall to not fail on old CLISP.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member :asdf *features*)
    (ignore-errors (funcall 'require "asdf"))))

;; If not found, load asdf from wherever the user specified it
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member :asdf *features*)
    (ignore-errors
     (handler-bind ((warning #'muffle-warning))
       (let* ((build-asdf-lisp
                (merge-pathnames
                 (make-pathname :directory '(#-gcl :relative "build")
                                :name "asdf" :type "lisp"
                                :defaults *asdf-directory*)
                 *asdf-directory*))
              (asdf-lisp
                (make-pathname :directory (pathname-directory *asdf-directory*)
                               :defaults build-asdf-lisp)))
         (cond
           ((probe-file build-asdf-lisp)
            (load build-asdf-lisp))
           ((probe-file asdf-lisp)
            (load asdf-lisp))))))))

;; If still not found, error out.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member :asdf *features*)
    (error "Could not load ASDF.
Please install ASDF2 and in your ~~/.swank.lisp specify:
 (defparameter swank::*asdf-directory* #p\"/path/containing/asdf/\")")))

;;; If ASDF is found, try to upgrade it to the latest installed version.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind ((warning #'muffle-warning))
    (when *asdf-directory*
      (pushnew *asdf-directory* asdf:*central-registry*))
    (ignore-errors (asdf:operate 'asdf:load-op :asdf :verbose nil))))

;;; If ASDF is too old, punt.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((ver (get-asdf-version)))
    (unless (or #+asdf3
                (or (<= 3 (first (asdf/driver:parse-version ver)))
                    (asdf:version-satisfies ver *asdf-version-required-by-xcvb*)))
      (error "Your ASDF version ~A is too old for XCVB, which requires ~A.
Please upgrade to the latest stable ASDF and register it in your source-registry."
           ver *asdf-version-required-by-xcvb*))))


;;; We may now assume we have a recent enough ASDF with all the basic driver functions.

(declaim (optimize (speed 2) (space 2) (safety 3) (debug 3) (compilation-speed 0))
         #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

(asdf/package:define-package :xcvb-driver
  (:nicknames :xcvbd :xd)
  (:use :asdf/common-lisp :asdf/driver :asdf)
  (:reexport :asdf/driver)
  (:shadow #:create-image)
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

   ;; Magic strings
   #:+xcvb-slave-greeting+ #:+xcvb-slave-farewell+

   ;;; Using an inferior XCVB
   #:build-and-load #:bnl #:build-in-slave

   ;;; Build-time variables
   #:*goal* #:*stderr* #:*profiling*
   #:*post-image-restart* #:*entry-point*

   ;;; Environment support
   #:debugging #:with-profiling
   ;; #:run #:do-run ;; -- clashes with inferior-shell
   ;; #:run-commands #:run-command
   #-ecl #:dump-image #+ecl #:create-bundle
   #:register-fullname #:register-fullnames #:load-fullname-mappings
   #:registered-fullname-pathname))

(in-package :xcvb-driver)

;;; Initial implementation-dependent setup
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; otherwise ACL 5.0 may crap out on ASDF dependencies,
  ;; but even other implementations may have "fun" debugging.
  #+ecl
  (progn
    (let ((*load-verbose* nil)) (require :cmp))
    (setf c::*compile-in-constants* t))
  #+sbcl (progn
           (require :sb-posix)
           (proclaim '(sb-ext:muffle-conditions sb-ext:compiler-note)))
  (pushnew :xcvb-driver *features*))

;;;; ----- User-visible variables, 1: Control build in current process -----

;;; Variables used to control building in the current image

(defvar *profiling* nil
  "boolean: should we compute and display the time spend in each command?")

(defvar *goal* nil
  "what is the name of the goal toward which we execute commands?")

(defvar *initial-random-state* (make-random-state nil)
  "initial random state to preserve determinism")

;;;; ----- User-visible variables, 2: Control XCVB -----

;;; These variables are shared with XCVB itself.

(defvar *lisp-implementation-type*
  (nth-value 1 (implementation-type))
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
  (lisp-implementation-directory :truename t)
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

(defvar *required-xcvb-version* "0.600"
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

;;;; ---- Build and Execution control ----

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
  "Enable (or with NIL argument, disable) verbose debugging output from ASDF"
  (setf *lisp-interaction* debug
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


;;; Profiling
(defun call-with-maybe-profiling (thunk what goal)
  (when *lisp-interaction*
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


;;;; ----- Pathname mappings -----
;; TODO: make it work, test it.

(defvar *pathname-mappings* (make-hash-table :test 'equal)
  "Mappings from xcvb fullname to plist of
 (physical) :pathname, :logical-pathname, :tthsum digest, etc.")

(defun register-fullname (&key fullname pathname tthsum logical-pathname)
  (setf (gethash fullname *pathname-mappings*)
        (list :truename (truename* pathname)
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
    (register-fullnames (read-file-form tn) :defaults tn)))


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

(defun cl-require (x)
  (with-profiling `(:require ,x)
    (require x)))


;;; ASDF support

(defun initialize-asdf (&key source-registry output-translations)
  (asdf:clear-configuration)
  (asdf:initialize-source-registry source-registry)
  (asdf:initialize-output-translations output-translations))

(defun register-asdf-directory (x)
  (pushnew x asdf:*central-registry*))

(defun asdf-systems-up-to-date-p (systems &optional (operation 'asdf:load-op))
  "Are all the ASDF systems up to date (for loading)?"
  (let* ((op (asdf/operation:find-operation () operation))
         (plan (asdf/plan:traverse-actions
                (loop :for s :in systems
                      :collect (cons op (find-component () s)))
                :plan-class 'asdf/plan:sequential-plan)))
    (loop :for (o . c) :in plan
          :always (asdf:needed-in-image-p o c))))

(defun asdf-systems-up-to-date (&rest systems)
  (shell-boolean-exit (asdf-systems-up-to-date-p systems)))


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

(defun do-compile-lisp (dependencies source fasl
                        &key #+sbcl cfasl #+ecl lisp-object around-compile encoding warnings-file)
  (let ((*goal* `(:compile-lisp ,source))
        (*default-pathname-defaults* (truename *default-pathname-defaults*)))
    (multiple-value-bind (output-truename warnings-p failure-p)
        (with-profiling `(:preparing-and-compiling ,source)
          (with-profiling `(:preparing-compilation-of ,source)
            (run-commands dependencies))
          (with-profiling `(:compiling ,source)
            (with-determinism `(:compiling ,source)
              (multiple-value-prog1
                  (call-around-hook
                   around-compile
                   (lambda ()
                     (apply 'compile-file* source
                            :output-file (merge-pathnames* fasl)
                            :external-format (encoding-external-format encoding)
                            :warnings-file warnings-file
                            (append
                             #+sbcl (when cfasl `(:emit-cfasl ,(merge-pathnames* cfasl)))
                             #+ecl (when lisp-object
                                     `(:object-file #+ecl (merge-pathnames* lisp-object)))))))))))
      (declare (ignorable warnings-p failure-p))
      (unless output-truename
        (die "Compilation Failed for ~A, no fasl created" source))
      (values))))

(defun compile-lisp (spec &rest dependencies)
  (apply 'do-compile-lisp dependencies spec))

;;; DSL entry point to create images
#-ecl
(defun do-create-image (image dependencies &rest flags)
  (let ((*goal* `(create-image ,image))
        #+sbcl (*uninteresting-compiler-conditions*
                (cons "undefined ~(~A~): ~S" *uninteresting-compiler-conditions*)))
    (with-muffled-compiler-conditions ()
      (run-commands dependencies))
    (apply #'dump-image image flags)))

#+ecl ;; wholly untested and probably buggy.
(defun do-create-image (image dependencies &rest keys
                        &key kind executable output-name prelude entry-point)
  (let ((*goal* `(create-image ,image ,dependencies ,@keys))
        (kind (or kind (if executable :program :shared-library)))
        (first-dep (car dependencies)))
    (multiple-value-bind (object-files manifest)
        (case (first first-dep)
          ((:load-manifest)
           (assert (null (rest dependencies)))
           (let ((manifest (read-file-form (second first-dep))))
             (values
              (loop :for l :in manifest
                    :collect
                    (destructuring-bind (&key command parent pathname
                                           tthsum source-pathname source-tthsum) l
                      (declare (ignore tthsum source-pathname source-tthsum))
                      (assert (eq (car command) :load-file))
                      pathname))
              manifest)))
          (:load-file
           (loop :for l :in dependencies
                 :collect
                 (destructuring-bind (link-file pathname) l
                   (assert (eq link-file :load-file))
                   pathname)))
          (t
           (assert (null dependencies))))
      (create-image
       kind (pathname image)
       :lisp-files object-files
       :init-name (c::compute-init-name (or output-name image) :kind kind)
       :prelude
       (when (eq kind :program)
         `(progn
            (setf xcvb-driver:*manifest* ',(reverse manifest))
            ,@(etypecase prelude
                (null)
                (cons (list prelude))
                (string `((standard-eval-text ',prelude))))))
       :entry-point entry-point))))

(defun create-image (spec &rest dependencies)
  (destructuring-bind (image &rest keys) spec
    (apply 'do-create-image image dependencies keys)))


;;;; ----- CFFI-grovel support -----

(defun process-cffi-grovel-file (input c exe output &key cc-flags)
  (destructuring-bind (input c exe output)
      (mapcar 'fullname-pathname (list input c exe output))
    (with-current-directory (exe)
      (progv (list (find-symbol* :*cc-flags* :cffi-grovel)) (list cc-flags)
        (symbol-call :cffi-grovel :generate-c-file input c)
        (symbol-call :cffi-grovel :cc-compile-and-link c exe)
        (symbol-call :cffi-grovel :invoke exe output)))))

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
              (symbol-call :cffi-grovel :generate-c-lib-file input c)
            (declare (ignore c-file))
            (symbol-call :cffi-grovel :cc-compile-and-link c so :library t)
            (values (symbol-call :cffi-grovel :generate-bindings-file
                          c so lisp-forms c)
                    ;; currently use C instead of OUTPUT, due to output locations.
                    ;; ugly, but generate-bindings-file already adds .grovel-tmp.lisp
                    ;; to the output name, so we reuse the c name here. Sigh.
                    so)))))))

;;; Magic strings. Do not change. Constants, except we can't portably use defconstant here.
(defvar +xcvb-slave-greeting+ #.(format nil "Dear Master, here are your build commands:~%"))
(defvar +xcvb-slave-farewell+ #.(format nil "~%Your desires are my orders, sincerely, XCVB.~%"))

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
  (setf *manifest* (reverse (read-file-form pathname))))
(defun load-manifest (pathname)
  (process-manifest (read-file-form pathname)))

;;;; ----- XCVB automagic bootstrap: creating XCVB if not there yet -----
(defvar *xcvb-present* nil)

(defun default-xcvb-program ()
  (native-namestring
   (subpathname
    (user-homedir-pathname)
    (format nil ".cache/common-lisp/bin/~(~A~@[-~A~]~)/xcvb"
            (operating-system) (architecture)))))

(defun xcvb-present-p (&optional (program *xcvb-program*))
  ;; returns the resolved path to xcvb if present
  (or (and (equal program *xcvb-present*) program)
      (etypecase program
	((eql t) (and (find-package :xcvb) (setf *xcvb-present* t)))
	(string
         (and
          (string-prefix-p "XCVB version "
                           (run-program
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
  (asdf:load-system :xcvb-bootstrap)
  (funcall 'build-xcvb program))

(defun require-xcvb ()
  (asdf:load-system :xcvb)
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
      (debugging *lisp-interaction*)
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
       (run-program
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
  (setf *lisp-interaction* (getenvp "XCVB_DEBUGGING"))
  (setf *profiling* (getenvp "XCVB_PROFILING"))
  (tweak-implementation)
  (values))

;;;; ----- The End -----
