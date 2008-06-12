#| ;;; cl-launch 2.07 lisp header
|# ;;;; Silence our lisp implementation for quiet batch use...

#| We'd like to evaluate as little as possible of the code without compilation.
 This poses a typical bootstrapping problem: the more sophistication we want
 to distinguish what to put where in what dynamic environment, the more code
 we have to evaluate before we may actually load compiled files. And, then,
 it is a waste of time to try to compile said code into a file. Moving things
 to the shell can only help so much, and reduces flexibility. Our best bet is
 to tell sbcl or cmucl to not try to optimize too hard.
|#
(eval-when (:load-toplevel :execute :compile-toplevel)
  (declaim (optimize (speed 1) (safety 2) (compilation-speed 3) #-gcl (debug 1)
       	   #+sbcl (sb-ext:inhibit-warnings 3)
           #+sbcl (sb-c::merge-tail-calls 3) ;-- this plus debug 1 (or sb-c::insert-debug-catch 0 ???) should ensure all tail calls are optimized, says jsnell
	   #+cmu (ext:inhibit-warnings 3)))
  #+gcl ;;; If using GCL, do some safety checks
  (when (or #-ansi-cl t)
    (format *error-output*
     "CL-Launch only supports GCL in ANSI mode. Aborting.~%")
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
  (unless (find-package :cl-launch)
    (if (find-package :common-lisp)
       (defpackage :cl-launch (:use :common-lisp))
       (make-package :cl-launch :use '(:lisp))))
  (in-package :cl-launch))
(defmacro dbg (tag &rest exprs)
  "simple debug statement macro:
outputs a tag plus a list of source expressions and their resulting values, returns the last values"
  (let ((res (gensym))(f (gensym)))
  `(let ((,res))
    (flet ((,f (fmt &rest args) (apply #'format *error-output* fmt args)))
      (,f "~&~A~%" ,tag)
      ,@(mapcan
         #'(lambda (x)
            `((,f "~&  ~S => " ',x)
              (,f "~{~S~^ ~}~%" (setf ,res (multiple-value-list ,x)))))
         exprs)
      (apply 'values ,res)))))
(eval-when (:load-toplevel :execute :compile-toplevel)
  ;; Import a few symbols if needed
  #+common-lisp-controller
  (map () #'import
       '(clc::*source-root*
         clc::*fasl-root*
         clc::calculate-fasl-root
         clc::source-root-path-to-fasl-path
         clc::alternative-root-path-to-fasl-path
         clc::*redirect-fasl-files-to-cache*))
  #+ecl
  (map () #'import
       '(c::system-ld-flag
         c::library-type-p
         c::built-type-p
         c::builder
         c::build-fasl
         c::wt-filtered-data
         c::init-function-name
         c::data-init
         c::compiler-cc
         c::linker-cc
         c::shared-cc
         c::bundle-cc
         c::safe-system
         c::cmp-delete-file
         c::+lisp-program-header+
         c::+lisp-program-init+
         c::+lisp-program-main+
         c::+static-library-prefix+
         c::+lisp-program-init+
         ))
  ;;; define getenv and quit in ways that minimize package conflicts
  ;;; (use-package :cl-launch) while in cl-user.
  #+(or openmcl allegro gcl clisp ecl)
    (import '#+openmcl ccl::getenv
             #+allegro sys:getenv
             #+gcl system:getenv
             #+clisp ext:getenv
             #+ecl si:getenv
      :cl-launch)
  #+(or cmu sbcl lispworks)
    (defun getenv (x)
      #+sbcl (sb-ext:posix-getenv x)
      #+lispworks (lispworks:environment-variable x)
      #+cmu (cdr (assoc (intern x :keyword) ext:*environment-list*)))
  (defun quit (&optional (code 0) (finish-output t))
    (when finish-output ;; essential, for openmcl, and for standard compliance.
      (finish-outputs))
    #+cmu (unix:unix-exit code)
    #+clisp (ext:quit code)
    #+sbcl (sb-unix:unix-exit code)
    #+openmcl (ccl:quit code)
    #+gcl (lisp:quit code)
    #+allegro (excl:exit code :quiet t)
    #+ecl (si:quit code)
    #+lispworks (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
    #-(or cmu clisp sbcl openmcl gcl allegro ecl lispworks)
    (error "Quitting not implemented")))
(eval-when (:load-toplevel :execute :compile-toplevel)
  ;;;; Load ASDF
  (ignore-errors (require :asdf))
  ;;; Here is a fallback plan in case the lisp implementation isn't asdf-aware.
  (unless (and (find-package :asdf) (find-symbol "OUTPUT-FILES" :asdf))
    (defvar *asdf-path*
      (or (and (getenv "ASDF_PATH") (probe-file (getenv "ASDF_PATH")))
          (probe-file (merge-pathnames "src/asdf/asdf.lisp"
                                       (user-homedir-pathname)))
          (probe-file "/usr/share/common-lisp/source/asdf/asdf.lisp")))
    (when *asdf-path*
      (ignore-errors (load *asdf-path* :verbose nil :print nil)))))
(eval-when (:load-toplevel :execute :compile-toplevel)
  ;;; Even in absence of asdf, at least create a package asdf.
  (unless (find-package :asdf)
    (make-package :asdf)))
(eval-when (:load-toplevel :execute :compile-toplevel)
  ;;; Try to share this with asdf, in case we get asdf to support it one day.
  (map () #'import
       '(asdf::*output-pathname-translations*
         asdf::resolve-symlinks
         asdf::oos asdf::load-op asdf::find-system)))

;;;; CL-Launch Initialization code
(progn

(pushnew :cl-launch *features*)

;;#+ecl (require 'cmp) ; ensure we use the compiler (we use e.g. *ecl-library-directory*)

(dolist (s '(*arguments* getenv quit compile-and-load-file
             compile-file-pathname* apply-pathname-translations
	     *output-pathname-translations*
             apply-output-pathname-translations))
  (export s))

;; To dynamically recompute from the environment at each invocation
(defvar *cl-launch-file* nil)
(defvar *verbose* nil)
(defvar *lisp-fasl-cache* nil "lisp fasl cache hierarchy")
(defvar *lisp-fasl-root* nil "top path for the fasl cache for current implementation")
;; To dynamically recompute from the command-line at each invocation
(defvar *arguments* nil "command-line parameters")

;; Variables that define the current system
(defvar *dumped* nil)
(defvar *restart* nil)
(defvar *init-forms* nil)
(defvar *quit* t)

;; Provide compatibility with clc 6.2
(defvar *redirect-fasl-files-to-cache* t)

#+ecl
(defun command-line-arguments ()
  (loop for i from 1 below (si:argc) collect (si:argv i)))

(defun compute-arguments ()
  #+gcl (setf system::*tmp-dir* (ensure-directory-name (or (getenv "TMP") "/tmp"))) ; basic lack fixed after gcl 2.7.0-61, but ending / required still on 2.7.0-64.1
  (setf *cl-launch-file* (getenv "CL_LAUNCH_FILE")
        *verbose* (when (getenv "CL_LAUNCH_VERBOSE") t)
        *lisp-fasl-cache* (let* ((cache-env (getenv "LISP_FASL_CACHE"))
                            (cache-spec
                             (cond
                               ((null cache-env)
                                (merge-pathnames
                                 #p".cache/lisp-fasl/"
                                 ;;(make-pathname :directory (list :relative ".cache" "lisp-fasl"))
                                 (user-homedir-pathname)))
                               ((equal cache-env "NIL") nil)
                               (t (dirname->pathname cache-env)))))
                       #+gcl-pre2.7 cache-spec #-gcl-pre2.7
		       (when cache-spec
                         (ensure-directories-exist cache-spec)
                         (resolve-symlinks cache-spec)))
        *lisp-fasl-root* (let* ((root-env
                                 (when (getenv "LISP")
                                   (let ((r (getenv "LISP_FASL_ROOT")))
                                     (when r (if (equal r "NIL") :disabled
                                                 (dirname->pathname r))))))
                                (root-spec
                                 (or root-env
                                     (when *lisp-fasl-cache*
                                       (merge-pathnames
                                        (make-pathname
                                         :directory (list :relative *implementation-name*))
                                        *lisp-fasl-cache*)))))
			   #+gcl-pre2.7 root-spec #-gcl-pre2.7
                           (when root-spec
                             (ensure-directories-exist root-spec)
                             (resolve-symlinks root-spec))))
  (calculate-output-pathname-translations)
  (setf *arguments*
   (or *arguments*
       #+(or cmu gcl ecl lispworks)
         (cdr (member "--"
		      #+gcl si:*command-args*
                      #+ecl (command-line-arguments)
		      #+cmu extensions:*command-line-strings*
                      #+lispworks system:*line-arguments-list*
		      :test 'equal))
       #+openmcl ccl:*unprocessed-command-line-arguments*
       #+sbcl (cdr sb-ext:*posix-argv*)
       #+allegro (cdr (sys:command-line-arguments))
       #+clisp (cdr ext:*args*))))

(defun register-paths (paths)
  #-asdf (declare (ignore paths))
  #+asdf
  (dolist (p (reverse paths))
    (pushnew p asdf::*central-registry* :test 'equal)))

(defun load-stream (&optional (s #-clisp *standard-input*
				 #+clisp *terminal-io*))
  ;; GCL 2.6 can't load from a string-input-stream
  ;; OpenMCL 1.1-pre cannot load from either *standard-input* or *terminal-io*
  ;; Allegro 5, I don't remember but it must have been broken when I tested.
  #+(or gcl-pre2.7 allegro)
  (do ((eof '#:eof) (x t (read s nil eof))) ((eq x eof)) (eval x))
  #-(or gcl-pre2.7 allegro)
  (load s :verbose nil :print nil))

(defun load-string (string)
  (with-input-from-string (s string) (load-stream s)))

(defun finish-outputs ()
  (finish-output *error-output*)
  (finish-output))

(defun %abort (code fmt &rest args)
  (apply #'format *error-output* fmt args)
  (quit code))

(defun resume ()
  (compute-arguments)
  (do-resume))

(defun do-resume ()
  (when *restart* (funcall *restart*))
  (when *init-forms* (load-string *init-forms*))
  (finish-outputs)
  (when *quit* (quit 0)))

(defun dump-image (filename &key executable (package :cl-user))
  (declare (ignorable filename executable package))
  (setf *dumped* (if executable :executable t)
        *arguments* nil)
  #+clisp
  (ext:saveinitmem filename
   :executable executable
   :init-function (when executable #'resume)
   ;; :parse-options (not executable) ;--- requires a patch to clisp
   :script t
   :quiet t
   :norc t
   :start-package package
   :keep-global-handlers nil)
  #+sbcl
  (progn
    ;;(sb-pcl::precompile-random-code-segments) ;--- it is ugly slow at compile-time (!) when the initial core is a big CLOS program. If you want it, do it yourself
   (setf sb-ext::*gc-run-time* 0)
   (apply 'sb-ext:save-lisp-and-die filename
    :executable executable
    (when executable (list :toplevel #'resume))))
  #+cmu
  (progn
   (ext:gc :full t)
   (setf ext:*batch-mode* nil)
   (setf ext::*gc-run-time* 0)
   (extensions:save-lisp filename))
  #+openmcl
  (ccl:save-application filename)
  #+allegro
  (progn
   (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) ; :new 5000000
   (excl:dumplisp :name filename :suppress-allegro-cl-banner t))
  #+lispworks
  (save-image filename :environment nil) ; XXXXX
  #+gcl
  (progn
   (si::set-hole-size 500) (si::gbc nil) (si::sgc-on t)
   (si::save-system filename))
  #-(or clisp sbcl cmu openmcl allegro gcl lispworks)
  (%abort 11 "CL-Launch doesn't supports image dumping with this Lisp implementation.~%"))

(defun run (&key paths load system dump restart init (quit 0))
  (pushnew :cl-launched *features*)
  (compute-arguments)
  (when paths (register-paths paths))
  (if dump
      (build-and-dump dump load system restart init quit)
      (build-and-run load system restart init quit)))

(defun read-function (string)
  `(function ,(read-from-string string)))

(defun build-and-load (load system restart init quit)
  (when load
    (cond
     ((eq load t) (load-stream))
     ((streamp load) (load-stream load))
     ((eq load :self) (load-file *cl-launch-file*))
     (t (load-file load))))
  (when system
    #+asdf
    (load-system system :verbose *verbose*)
    #-asdf
    (%abort 10 "ERROR: asdf requested, but not found~%"))
  (setf *restart* (when restart (eval (read-function restart)))
        *init-forms* init
        *quit* quit))

(defun build-and-run (load system restart init quit)
  (build-and-load load system restart init quit)
  (do-resume))

#-ecl
(defun build-and-dump (dump load system restart init quit)
  (build-and-load load system restart init quit)
  (dump-image dump :executable (getenv "CL_LAUNCH_EXECUTABLE"))
  (quit))

#+ecl (progn ;;; ECL PATCH: modifies and adds functions into ecl*/src/cmp/cmpmain.lsp
;;; necessary for ecl 0.9i. The patch since made it to the development branch and so
;;; will have to be removed from here when new ecl releases make it to our target
;;; linux distributions.

(defun system-ld-flag (library)
  (let ((asdf (find-package "ASDF"))
        system)
    (labels ((asdfsym (x) (find-symbol (string x) asdf))
             (asdfcall (fun &rest rest) (apply (asdfsym fun) rest))
             (system-output (system type)
               (let ((build (make-instance (asdfsym :build-op) :type type)))
                 (first (asdfcall :output-files build system))))
             (existing-system-output (system type)
               (let ((o (system-output system type)))
                 (and o (probe-file o))))
             (find-archive (system)
                 (or (existing-system-output system :library)
                     (existing-system-output system :shared-library)))
             (fallback () (format nil #-msvc "-l~A" #+msvc "~A.lib" (string-downcase library))))
      (or (and asdf
               (setf system (asdfcall :find-system library nil))
               (find-archive system))
        (fallback)))))

(defun library-type-p (type)
  (member type
          #-msvc '("a" "so")
          #+msvc '("lib" "dll")
          :test #'equal))

(defun built-type-p (type)
  (or (equal type #-msvc "o" #+msvc "obj")
      (library-type-p type)))

(defun builder (target output-name &key lisp-files ld-flags shared-data-file
		(init-name nil)
		(prologue-code "")
		(epilogue-code (when (eq target :program) '(SI::TOP-LEVEL)))
		#+:win32 (system :console))
  ;;
  ;; The epilogue-code can be either a string made of C code, or a
  ;; lisp form.  In the latter case we add some additional C code to
  ;; clean up, and the lisp form is stored in a text representation,
  ;; to avoid using the compiler.
  ;;
  (cond ((null epilogue-code)
	 (setf epilogue-code ""))
	((stringp epilogue-code)
	 )
	(t
	 (with-standard-io-syntax
	   (setq epilogue-code
		 (with-output-to-string (stream)
		   (princ "{ const char *lisp_code = " stream)
		   (wt-filtered-data (write-to-string epilogue-code) stream)
		   (princ ";
cl_object output;
si_select_package(make_simple_base_string(\"CL-USER\"));
output = cl_safe_eval(c_string_to_object(lisp_code), Cnil, OBJNULL);
" stream)
		   (when (eq target :program)
		     (princ "cl_shutdown(); return (output != OBJNULL);" stream))
		   (princ #\} stream)
		   )))))
  ;;
  ;; When a module is built out of several object files, we have to
  ;; create an additional object file that initializes those ones.
  ;; This routine is responsible for creating this file.
  ;;
  ;; To avoid name clashes, this object file will have a temporary
  ;; file name (tmp-name).
  ;;
  (let* ((tmp-name (si::mkstemp #P"TMP:ECLINIT"))
	 (c-name (si::coerce-to-filename
		  (compile-file-pathname tmp-name :type :c)))
	 (o-name (si::coerce-to-filename
		  (compile-file-pathname tmp-name :type :object)))
	 submodules
	 c-file)
    (dolist (item (reverse lisp-files))
      (etypecase item
        (symbol
         (push (system-ld-flag item) ld-flags)
         ;;---*** NOTE: reduce clashes by having a system prefix in the init-name
         (push (init-function-name item "system") submodules))
        ((or string pathname)
         (let* ((pn (parse-namestring item))
                (type (pathname-type pn))
                (module (pathname-name pn))
                (init-fn
                 ;;---*** NOTE: it would reduce clashes to keep/add a
                 ;;---*** system/library prefix in/to the init-name
                 (if (library-type-p type)
                   (let ((name
                          (if (equal (subseq module 0 (length +static-library-prefix+))
                                     +static-library-prefix+)
                           (subseq module (length +static-library-prefix+))
                           module)))
                     (init-function-name name "system"))
                   (init-function-name module)))
                (built-pn
                 (if (built-type-p type) pn
                   (compile-file-pathname pn :type :object)))
                (filename (si::coerce-to-filename built-pn)))
           (push filename ld-flags)
           (push init-fn submodules)))))
    (setq c-file (open c-name :direction :output))
    (format c-file +lisp-program-header+
            #-(or :win32 :mingw32 :darwin) (if (eq :fasl target) nil submodules)
            #+(or :win32 :mingw32 :darwin) submodules)
    (cond (shared-data-file
	   (data-init shared-data-file)
	   (format c-file "
#define VM ~A
#ifdef ECL_DYNAMIC_VV
static cl_object *VV;
#else
static cl_object VV[VM];
#endif
#define ECL_SHARED_DATA_FILE 1
" (data-permanent-storage-size))
	   (data-dump c-file))
	  (t
	   (format c-file "
#define compiler_data_text NULL
#define compiler_data_text_size 0
#define VV NULL
#define VM 0" c-file)))
    (ecase target
      (:program
       (when (or (symbolp output-name) (stringp output-name))
	 (setf output-name (compile-file-pathname output-name :type :program)))
       (unless init-name
	 (setf init-name (init-function-name (pathname-name output-name) nil)))
       (format c-file +lisp-program-init+ init-name "" shared-data-file
	       submodules "")
       (format c-file #+:win32 (ecase system (:console +lisp-program-main+)
				             (:windows +lisp-program-winmain+))
	              #-:win32 +lisp-program-main+
		      prologue-code init-name epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (apply #'linker-cc output-name (namestring o-name) ld-flags))
      ((:library :static-library :lib)
       (when (or (symbolp output-name) (stringp output-name))
	 (setf output-name (compile-file-pathname output-name :type :lib)))
       (unless init-name
	 ;; Remove the leading "lib"
	 (setf init-name (subseq (pathname-name output-name) (length +static-library-prefix+)))
	 (setf init-name (init-function-name init-name "system")))
       (format c-file +lisp-program-init+ init-name prologue-code
	       shared-data-file submodules epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       #-msvc
       (progn
       (safe-system (format nil "ar cr ~A ~A ~{~A ~}"
			    output-name o-name ld-flags))
       (safe-system (format nil "ranlib ~A" output-name)))
       #+msvc
       (unwind-protect
         (progn
           (with-open-file (f "static_lib.tmp" :direction :output :if-does-not-exist :create :if-exists :supersede)
             (format f "/DEBUGTYPE:CV /OUT:~A ~A ~{~&\"~A\"~}"
                     output-name o-name ld-flags))
           (safe-system "link -lib @static_lib.tmp"))
         (when (probe-file "static_lib.tmp")
           (cmp-delete-file "static_lib.tmp")))
       )
      #+dlopen
      ((:shared-library :dll)
       (when (or (symbolp output-name) (stringp output-name))
	 (setf output-name (compile-file-pathname output-name :type :dll)))
       (unless init-name
	 ;; Remove the leading "lib"
	 (setf init-name (subseq (pathname-name output-name)
				 (length +static-library-prefix+)))
	 (setf init-name (init-function-name init-name nil)))
       (format c-file +lisp-program-init+ init-name prologue-code
	       shared-data-file submodules epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (apply #'shared-cc output-name o-name ld-flags))
      #+dlopen
      (:fasl
       (when (or (symbolp output-name) (stringp output-name))
	 (setf output-name (compile-file-pathname output-name :type :fasl)))
       (unless init-name
	 (setf init-name (init-function-name "CODE" nil)))
       #-(or :win32 :mingw32 :darwin)
       (setf submodules
	     (mapcar #'(lambda (sm)
			 (format nil "((ecl_init_function_t) ecl_library_symbol(Cblock, \"~A\", 0))" sm))
		     submodules))
       (format c-file +lisp-program-init+ init-name prologue-code shared-data-file
	       submodules epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (apply #'bundle-cc output-name o-name ld-flags)))
    (cmp-delete-file tmp-name)
    (cmp-delete-file c-name)
    (cmp-delete-file o-name)
    output-name))
);END OF ECL PATCH

#+ecl
(defun build-and-dump (dump load system restart init quit)
  (setf *compile-verbose* *verbose*
        c::*suppress-compiler-warnings* (not *verbose*)
        c::*suppress-compiler-notes* (not *verbose*))
  (let* ((library-type :library) ; :library :shared-library
         (program-type :program) ; :program :fasl
         (cl-launch-objects
	  (let ((*features* (remove :cl-launch *features*))
                (header (or *compile-file-pathname* *load-pathname* (getenv "CL_LAUNCH_HEADER"))))
	    (list
	     (compile-and-load-file header :verbose *verbose* :load nil :system-p t))))
	 (file-objects
	  (when load
	    (list
             (labels ((x (file)
                        (compile-and-load-file file :verbose *verbose* :system-p t :load t))
                      (xwt (s)
                        (error "dumping image from a stream is unsupported")
                        ;; should be dumping the stream to a temporary file then compiling
                        ))
               (cond
                 ((eq load t) (xwt *standard-input*))
                 ((streamp load) (xwt load))
                 ((eq load :self) (x *cl-launch-file*))
                 (t (x load)))))))
	 (system-objects
	  (when system
	    (let* ((target (find-system system))
                   (build (make-instance 'asdf:build-op :type library-type))
                   (sysdep ()))
              (loop for (op . component) in (asdf::traverse build target)
                    when (typep component 'asdf:system)
                    do (pushnew component sysdep)
                    finally (setf sysdep (nreverse sysdep)))
              (loop for system in sysdep
                    nconc (asdf:output-files build system)
                    do (asdf:oos 'asdf:compile-op system)
                    do (asdf:oos 'asdf:build-op system :type library-type)))))
         (executable (getenv "CL_LAUNCH_EXECUTABLE"))
	 (init-code
	  `(setf
	    *load-verbose* nil
            *dumped* ,(if executable :executable t)
            ,@(when executable
                '(*arguments* (command-line-arguments)))
	    ,@(when restart
		    `(*restart* ,(read-function restart)))
	    ,@(when init
		    `(*init-forms* ,init))
	    ,@(unless quit
		      `(*quit* nil))))
         (epilogue-code
          (ecase program-type
            (:fasl init-code)
            (:program `(progn ,init-code (resume)))))
	 (fasl
	  (builder program-type (parse-namestring dump)
                   :lisp-files
                   (append cl-launch-objects file-objects system-objects)
                   :epilogue-code epilogue-code)))
  (quit)))

;;;; Find a unique directory name for current implementation for the fasl cache
;;; (modified from SLIME's swank-loader.lisp)

(defparameter *implementation-features*
  '(:allegro :lispworks :sbcl :openmcl :cmu :clisp :ccl :corman :cormanlisp
    :armedbear :gcl :ecl :scl))

(defparameter *os-features*
  '(:macosx :linux :windows :mswindows :win32
    :solaris :darwin :sunos :hpux :unix))

(defparameter *architecture-features*
  '(:powerpc :ppc
    :x86-64 :amd64 :x86 :i686 :i586 :i486 :pc386 :iapx386
    :sparc64 :sparc :hppa64 :hppa))

(defun lisp-version-string ()
  #+cmu       (substitute-if #\_ (lambda (x) (find x " /"))
                             (lisp-implementation-version))
  #+scl       (lisp-implementation-version)
  #+sbcl      (lisp-implementation-version)
  #+ecl       (lisp-implementation-version)
  #+openmcl   (format nil "~d.~d.fasl~d"
                      ccl::*openmcl-major-version*
                      ccl::*openmcl-minor-version*
                      (logand ccl::fasl-version #xFF))
  #+lispworks (lisp-implementation-version)
  #+allegro   (format nil
                      "~A~A~A"
                      excl::*common-lisp-version-number*
                      (if (eq 'h 'H) "A" "M")     ; ANSI vs MoDeRn
                      (if (member :64bit *features*) "-64bit" ""))
  #+clisp     (let ((s (lisp-implementation-version)))
                (subseq s 0 (position #\space s)))
  #+armedbear (lisp-implementation-version)
  #+cormanlisp (lisp-implementation-version)
  #+gcl       (let ((s (lisp-implementation-version))) (subseq s 4)))

(defun ensure-directory-name (dn)
   (if (eql #\/ (char dn (1- (length dn)))) dn
      (concatenate 'string dn "/")))

(defun dirname->pathname (dn)
  (parse-namestring (ensure-directory-name dn)))

(defun unique-directory-name (&optional warn)
  "Return a name that can be used as a directory name that is
unique to a Lisp implementation, Lisp implementation version,
operating system, and hardware architecture."
  (flet ((first-of (features)
           (find-if #'(lambda (f) (find f *features*)) features))
         (maybe-warn (value fstring &rest args)
           (cond (value)
                 (t (when warn (apply #'warn fstring args))
                    "unknown"))))
    (let ((lisp (maybe-warn (first-of *implementation-features*)
                            "No implementation feature found in ~a."
                            *implementation-features*))
          (os   (maybe-warn (first-of *os-features*)
                            "No os feature found in ~a." *os-features*))
          (arch	(maybe-warn (first-of *architecture-features*)
                            "No architecture feature found in ~a."
                            *architecture-features*))
          (version (maybe-warn (lisp-version-string)
                               "Don't know how to get Lisp ~
                                implementation version.")))
      (format nil "~(~@{~a~^-~}~)" lisp version os arch))))

(defvar *implementation-name* (unique-directory-name *verbose*)
  "The name of the implementation, used to make a directory hierarchy for fasl files")

;;;; Redefine the ASDF output-files method to put fasl's under the fasl cache.
;;; (taken from common-lisp-controller's post-sysdef-install.lisp)

;;#-common-lisp-controller (progn ; BEGIN of progn to disable caching when clc is detected

(defparameter *wild-path*
   (make-pathname :directory '(:relative :wild-inferiors)
		  :name :wild :type :wild :version nil))

(defun wilden (path)
   (merge-pathnames *wild-path* path))

#-asdf
(defun resolve-symlinks (x)
  #+allegro (excl:pathname-resolve-symbolic-links x)
  #+gcl-pre2.7 (truename (merge-pathnames x *default-pathname-defaults*))
  #-(or allegro gcl-pre2.7)
  (truename x))

(defvar *output-pathname-translations* nil
  "a list of pathname translations, where every translation is a list
of a source pathname and destination pathname.")

(defun exclude-from-cache (&rest dirs)
  (dolist (dir dirs)
    (when dir
      (let* ((p (if (pathnamep dir) dir (dirname->pathname dir)))
             (n #+asdf (resolve-symlinks p) #-asdf p)
             (w (wilden n)))
        (pushnew (list w w)
                 cl-launch::*output-pathname-translations*
                 :test #'equal)))))

(defun calculate-output-pathname-translations ()
  (setf *output-pathname-translations*
        `(#+(and common-lisp-controller (not gcl))
          ,@(progn
              #-gcl-pre2.7 (ensure-directories-exist (calculate-fasl-root))
              (let* ((sr (resolve-symlinks *source-root*))
                     (fr (resolve-symlinks *fasl-root*))
                     (sp (wilden sr))
                     (fp (wilden fr)))
                `((,sp ,fp)
                  (,fp ,fp)
                  ,@(when *redirect-fasl-files-to-cache*
                      `((,(wilden "/")
                          ,(wilden (merge-pathnames
                                    (make-pathname :directory '(:relative "local")) fr))))))))
          #-(and common-lisp-controller (not gcl))
          ,@(when (and *lisp-fasl-root* (not (eq *lisp-fasl-root* :disabled)))
              `((,(wilden "/") ,(wilden *lisp-fasl-root*))))))

  ;; Do not recompile in private cache system-installed sources
  ;; that already have their accompanying precompiled fasls.
  #+(or clisp sbcl cmucl gcl) ; no need for ECL: no source/fasl couples there.
  (exclude-from-cache
   #p"/usr/lib/"
   #+clisp ext:*lib-directory*
   #+gcl system::*lib-directory*
   #+ecl c::*ecl-library-directory*
   #+sbcl (getenv "SBCL_HOME")
   #+cmu (truename #p"library:")))


(defun apply-pathname-translations
  (path &optional (translations *output-pathname-translations*))
#+gcl-pre2.7 path ;;; gcl 2.6 lacks pathname-match-p, anyway
#-gcl-pre2.7
  (loop
    for (source destination) in translations
    when (pathname-match-p path source)
    do (return (translate-pathname path source destination))
    finally (return path)))

#+asdf
(handler-bind ((warning #'muffle-warning))
  (defmethod asdf:output-files :around ((op asdf:operation) (c asdf:component))
    "Method to rewrite output files to fasl-path"
    (let ((orig (call-next-method)))
       (mapcar #'apply-pathname-translations orig))))

;; We provide cl-launch, no need to go looking for it further!
#+asdf
(unless (find-system :cl-launch nil)
  (asdf:defsystem :cl-launch
      #+gcl :pathname #+gcl "/dev/null"
      :depends-on () :serial t :components ()))

;);;END of progn to disable caching when clc is detected

#|
#+common-lisp-controller
(defun beneath-clc-source-root? (pn)
  "Returns T if pn's directory below *source-root*"
  (when pn
    (let ((root-dir (pathname-directory (resolve-symlinks *source-root*)))
          (comp-dir (pathname-directory pn)))
      (and (>= (length comp-dir)
               (length root-dir))
           (equalp root-dir (subseq comp-dir 0 (length root-dir)))))))
|#

(defun apply-output-pathname-translations (path)
#|  #+common-lisp-controller
  (progn
    (if (beneath-clc-source-root? path)
      (source-root-path-to-fasl-path path)
      (alternative-root-path-to-fasl-path path)))
  #-common-lisp-controller |#
  (apply-pathname-translations path))

#+asdf
(defun load-system (system &key verbose)
  (asdf:oos 'asdf:load-op system :verbose verbose))

#+asdf
(defun load-systems (&rest systems)
  (dolist (s systems) (load-system s :verbose *verbose*)))

(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is strictly newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun compile-file-pathname* (source &rest args)
  #-(or gcl ecl)
  (apply-output-pathname-translations
   (apply #'compile-file-pathname source args))
  #+(or gcl ecl) ;; ECL BUG: compile-file-pathname doesn't accept system-p
  (let* ((system-p (getf args :system-p))
         (args (loop for (x y . z) on args by #'cddr nconc
                     (unless (eq x :system-p)
                       (list x y))))
         (path (apply-output-pathname-translations
                (apply #'compile-file-pathname source args))))
    (if system-p
        (make-pathname :type "o" :defaults path)
        path)))

#-(or cormanlisp)
(defun compile-and-load-file (source &key force-recompile verbose (load t)
                                     #+(or ecl gcl) system-p)
  "compiles and load specified SOURCE file, if either required by keyword
argument FORCE-RECOMPILE, or not yet existing, or not up-to-date.
Keyword argument VERBOSE specifies whether to be verbose.
Returns two values: the fasl path, and T if the file was (re)compiled"
  (let* ((truesource (truename source))
         (fasl
          (compile-file-pathname* truesource
                                  #+(or ecl gcl) :system-p #+(or ecl gcl) system-p))
         (compiled-p
          (when (or force-recompile
                    (not (probe-file fasl))
                    (not (file-newer-p fasl source)))
            ;; When in doubt, don't trust and recompile, even though there are cases
            ;; when on the first time of compiling a simple auto-generated file
            ;; (e.g. from the automated test suite), the fasl ends up being written
            ;; to disk within the same second as the source was produced, which cannot
            ;; be distinguished from the reverse case where the source code was produced
            ;; in the same split second as the previous version was done compiling.
            ;; Could be tricky if a big system needs be recompiled as a dependency on
            ;; an automatically generated file, but for cl-launch those dependencies are
            ;; not detected anyway (BAD). If/when they are, and lacking better timestamps
            ;; than the filesystem provides, you should sleep after you generate your source code.
            #-gcl-pre2.7 (ensure-directories-exist fasl)
            (multiple-value-bind (path warnings failures)
                (compile-file truesource
                              :output-file fasl
                              #+ecl :system-p #+ecl system-p
                              #-gcl-pre2.7 :print #-gcl-pre2.7 verbose
                              #-gcl-pre2.7 :verbose #-gcl-pre2.7 verbose)
              (declare (ignore warnings))
              (unless (equal (truename fasl) (truename path))
                (error "CL-Launch: file compiled to ~A, expected ~A" path fasl))
              (when failures
                (error "CL-Launch: failures while compiling ~A" source)))
            t)))
    (when load
      (load #-(and ecl (not dlopen)) fasl
            #+(and ecl (not dlopen)) (if system-p source fasl)
            :verbose verbose))
    (values fasl compiled-p)))

#+(or cormanlisp)
(defun compile-and-load-file (source &key force-recompile verbose load)
  "Corman Lisp has trouble with compiled files (says SLIME)."
  (declare (ignore force-recompile))
  (when load
    (load source :verbose verbose))
  (force-output)
  (values nil t))

(defun load-file (source)
  #-(or gcl-pre2.7 (and ecl (not dlopen)))
  (compile-and-load-file source :verbose *verbose*)
  #+gcl-pre2.7
  (let* ((pn (parse-namestring source))) ; when compiling, gcl 2.6 will always
    (if (pathname-type pn) ; add a type .lsp if type is missing, so avoid compilation
      (compile-and-load-file source :verbose *verbose*)
      (load source :verbose *verbose*)))
  #+(and ecl (not dlopen))
  (load source :verbose *verbose*)))

(compute-arguments)

;;; END OF CL-LAUNCH LISP HEADER
