;;; Lisp implementations
#+xcvb (module (:build-depends-on ("/asdf" "/xcvb/driver")))

(defpackage :lisp-invocation
  (:use :cl :xcvb-driver)
  (:export
   #:get-lisp-implementation
   #:ensure-path-executable
   #:lisp-implementation-fullname
   #:lisp-implementation-name
   #:lisp-implementation-feature
   #:lisp-implementation-flags
   #:lisp-implementation-eval-flag
   #:lisp-implementation-load-flag
   #:lisp-implementation-arguments-end
   #:lisp-implementation-image-flag
   #:lisp-implementation-image-executable-p
   #:lisp-implementation-standalone-executable
   #:lisp-implementation-argument-control
   #:lisp-implementation-disable-debugger
   #:lisp-implementation-directory-variable
   #:lisp-environment-variable-name
   #:lisp-invocation-arglist
   #:quit-form
   #:save-image-form))

(in-package :lisp-invocation)

(defvar *lisp-implementations* (make-hash-table :test 'equal)
  "Dictionary of known Lisp implementations")

(defstruct (lisp-implementation)
  fullname
  name
  feature
  flags
  eval-flag
  load-flag
  arguments-end
  image-flag
  image-executable-p
  standalone-executable
  argument-control
  disable-debugger
  directory-variable
  ;; fasl-type cfasl-type
  quit-format
  dump-format)

(defmacro define-lisp-implementation (key () &rest keys)
  `(setf (gethash ,key *lisp-implementations*)
    (apply #'make-lisp-implementation ',keys)))

(defun get-lisp-implementation (&optional (implementation-type *lisp-implementation-type*))
  (or (gethash implementation-type *lisp-implementations*)
      (error "Unknown Lisp implementation type ~S" implementation-type)))

(define-lisp-implementation :abcl ()
  :fullname "Armed Bear Common Lisp"
  :name "abcl"
  :feature :abcl
  :flags ("--noinform" "--noinit" "--nosystem")
  :eval-flag "--eval"
  :load-flag "--load"
  :arguments-end "--"
  :image-flag nil
  :image-executable-p t
  :standalone-executable nil
  :argument-control t
  :disable-debugger ("--batch") ;; ???
  :quit-format "(ext:quit :status ~A)"
  :dump-format nil)

(define-lisp-implementation :allegro ()
  :fullname "Allegro CL"
  :name "alisp"
  :feature :allegro
  :flags ("-qq") ; on windows, +c ? On Allegro 5 and earlier, -Q and/or -QQ ?
  :eval-flag "-e"
  :load-flag "-L"
  ; :quit-flags ("-kill")
  :arguments-end "--"
  :image-flag "-I"
  :image-executable-p nil
  :standalone-executable nil
  :argument-control t
  :disable-debugger ("-batch") ; see also -#D -#C -#!
  :quit-format "(excl:exit ~A :quiet t)"
  :dump-format "(progn (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) (excl:dumplisp :name ~A :suppress-allegro-cl-banner t))")

(define-lisp-implementation :ccl () ;; demand 1.4 or later.
  :fullname "Clozure Common Lisp"
  ;; formerly OpenMCL, forked from MCL, formerly Macintosh Common Lisp, nee Coral Common Lisp
  ;; Random note: (finish-output) is essential for ccl, that won't do it by default,
  ;; unlike the other lisp implementations tested.
  :name "ccl"
  :feature :clozure
  :flags ("--no-init" "--quiet")
  :eval-flag "--eval" ; -e
  :load-flag "--load"
  :image-flag "--image-name" ; -I
  :image-executable-p t
  :standalone-executable t
  :arguments-end "--"
  :argument-control t ;; must be fixed now, but double-checking needed.
  :disable-debugger ("--batch")
  :directory-variable "CCL_DEFAULT_DIRECTORY"
  :quit-format "(let ((x ~A)) (finish-output *standard-output*) (finish-output *error-output*) (ccl:quit x))"
  :dump-format "(save-application ~S :prepend-kernel t)")

(define-lisp-implementation :clisp ()
  :fullname "GNU CLISP"
  :name "clisp"
  :feature :clisp
  :flags ("-norc" "--quiet" "--quiet")
  :eval-flag "-x"
  :load-flag "-i"
  :arguments-end "--"
  :image-executable-p t
  :image-flag "-M"
  :standalone-executable t ;; requires clisp 2.48 or later
  :argument-control t ;; *BUT* even a standalone-executable always accepts --clisp-x and such.
  :disable-debugger ("-on-error" "exit") ;; otherwise, -on-error debug
  :quit-format "(ext:quit ~A)"
  :dump-format "(ext:saveinitmem ~S :quiet t :executable t)")

(define-lisp-implementation :cmucl ()
  :fullname "CMU CL"
  :name "cmucl"
  :feature :cmu
  :flags ("-quiet" "-noinit")
  :eval-flag "-eval"
  :load-flag "-load"
  :arguments-end "--"
  :image-executable-p t
  :image-flag "-core"
  :argument-control t
  :disable-debugger ("-batch")
  :quit-format "(unix:unix-exit ~A)"
  :dump-format "(extensions:save-lisp ~S :executable t)")

(define-lisp-implementation :corman () ;; someone please add more complete support
  :fullname "Corman Lisp"
  :name () ;; There's a clconsole.exe, but what are the options?
  :feature :cormanlisp
  :quit-format "(win:exitprocess ~A)")

(define-lisp-implementation :ecl () ;; demand 10.4.2 or later.
  :fullname "Embeddable Common-Lisp"
  :name "ecl"
  :feature :ecl
  :flags ("-norc")
  :eval-flag "-eval" ; -e
  :load-flag "-load"
  :image-flag nil
  :image-executable-p t
  :arguments-end "--"
  :argument-control t ;; must be fixed now, but double-checking needed.
  :disable-debugger ()
  :quit-format "(si:quit ~A)"
  :dump-format nil) ;; Cannot dump with ECL. Link instead.

(define-lisp-implementation :gcl () ;; Demand 2.7.0, if it is ever released. In ANSI mode.
  :fullname "GNU Common Lisp"
  :name "gcl" ;; we might export GCL_ANSI=t or something
  :feature :gcl
  :flags ()
  :eval-flag "-eval" ; -e
  :load-flag "-load"
  :image-flag nil
  :image-executable-p t
  :arguments-end "--" ;; -f ?
  :disable-debugger ("-batch")
  :quit-format "(lisp:quit ~A)"
  :dump-format "(progn (si::set-hole-size 500) (si::gbc nil) (si::sgc-on t) (si::save-system ~A))")

(define-lisp-implementation :lispworks ()
  :fullname "LispWorks"
  :name "lispworks" ;; This assumes you dumped a proper image for batch processing...
  :feature :lispworks
  :flags ("-site-init" "-" "-init" "-")
  :eval-flag "-eval"
  :load-flag "-load" ;; Is this what we want? See also -build as magic load.
  :arguments-end nil ; What's the deal with THIS? "--"
  :image-flag nil
  :image-executable-p t
  :standalone-executable t
  :argument-control t
  :disable-debugger ()
  :quit-format "(lispworks:quit :status ~A :confirm nil :return nil :ignore-errors-p t)"
  ;; when you dump, you may also have to (system::copy-file ".../lwlicense" (make-pathname :name "lwlicense" :type nil :defaults filename))
  :dump-format "(lispworks:deliver 'xcvb-driver:resume ~A 0 :interface nil)") ; "(hcl:save-image ~A :environment nil)"

(define-lisp-implementation :lispworks-personal ()
  :fullname "LispWorks Personal Edition"
  :name () ;; In LispWorks Personal, the slave worker executes you!
  :feature :lispworks-personal-edition)

(define-lisp-implementation :mkcl ()
  :fullname "ManKai Common-Lisp"
  :name "mkcl"
  :feature :mkcl
  :flags ("-norc")
  :eval-flag "-eval" ; -e
  :load-flag "-load"
  :image-flag nil
  :image-executable-p t
  :arguments-end "--"
  :argument-control t ;; must be fixed now, but double-checking needed.
  :disable-debugger ()
  :quit-format "(mk-ext:quit :exit-code ~A)"
  :dump-format nil) ;; Cannot dump with ECL. Link instead.

(define-lisp-implementation :sbcl ()
  :fullname "Steel Bank Common Lisp"
  :name "sbcl"
  :feature :sbcl
  :flags ("--noinform" "--no-userinit" "--no-sysinit") ;; minimize non-determinism form user's env
  :eval-flag "--eval" ;; Note: SBCL's eval can only handle one form per argument.
  :load-flag "--load"
  :arguments-end "--end-toplevel-options"
  :image-flag "--core"
  :image-executable-p t
  :standalone-executable t ;; requires sbcl 1.0.21.24 or later.
  :argument-control t
  :disable-debugger ("--disable-debugger")
  :directory-variable "SBCL_HOME"
  :quit-format "(let ((exit (find-symbol \"EXIT\" :sb-ext)) (quit (find-symbol \"QUIT\" :sb-ext)) (code ~A)) (cond (exit (funcall exit :code code)) (quit (funcall quit :unix-status code))))"
  :dump-format "(sb-ext:save-lisp-and-die ~S :executable t)")

(define-lisp-implementation :scl ()
  :fullname "Scieneer Common Lisp" ; use 1.3.9 or later
  :name "scl"
  :feature :scl
  :flags ("-quiet" "-noinit")
  :eval-flag "-eval"
  :load-flag "-load"
  :arguments-end "--"
  :image-flag "-core"
  :argument-control nil ;; cmucl will always scan all the arguments for -eval... EVIL!
  :disable-debugger ("-batch")
  :quit-format "(unix:unix-exit ~A)"
  :dump-format "(extensions:save-lisp ~S)")

(define-lisp-implementation :xcl ()
  :fullname "XCL"
  :name "xcl"
  :feature :xcl
  :flags ("--no-userinit")
  :eval-flag "--eval"
  :load-flag "--load"
  :arguments-end "--"
  :image-flag nil
  :image-executable-p nil
  :standalone-executable nil
  :disable-debugger ()
  :quit-format "(ext:quit :status ~A)"
  :dump-format nil)

(defun ensure-path-executable (x)
  (when x
    (let ((n (native-namestring x)))
      (cond
	((asdf::absolute-pathname-p x) n)
	((asdf::os-unix-p) (format nil "./~A" n))
	(t n)))))

(defun lisp-environment-variable-name (&key (type *lisp-implementation-type*) prefix)
  (if (eq prefix t) (setf prefix "X"))
  (format nil "~@[~A~]~:@(~A~)" prefix type))

(defun lisp-invocation-arglist
    (&key (implementation-type *lisp-implementation-type*)
	  (lisp-path *lisp-executable-pathname*)
	  (lisp-flags :default)
	  (image-path *lisp-image-pathname*)
          load
	  eval
	  arguments
	  (debugger *lisp-allow-debugger*)
          (cross-compile t))
  (with-slots (name flags disable-debugger load-flag eval-flag
	       image-flag image-executable-p standalone-executable
	       arguments-end argument-control)
      (get-lisp-implementation implementation-type)
    (append
     (when (or (null image-path) (not image-executable-p))
       (list (or
              (when (consp lisp-path) lisp-path)
              (ensure-path-executable lisp-path)
              (getenv (lisp-environment-variable-name
                       :type implementation-type :prefix (when cross-compile "X")))
              name)))
     (when (and image-path (not image-executable-p))
       (list image-flag))
     (when image-path
       (list
        (if image-executable-p
          (ensure-path-executable image-path)
          image-path)))
     (if (eq lisp-flags :default)
	 flags
	 lisp-flags)
     (unless debugger
       disable-debugger)
     (mapcan (if load-flag
                 (lambda (x) (list load-flag x))
                 (lambda (x) (list eval-flag (format nil "(load ~S)" x))))
             (if (listp load) load (list load)))
     (when eval
       (list eval-flag eval))
     (when arguments
       (unless argument-control
	 (error "Can't reliably pass arguments to Lisp implementation ~A" implementation-type))
       (cons arguments-end arguments)))))

;;; Avoiding use of a compiled-in driver in the build process

(defun quit-form (&key code (implementation-type *lisp-implementation-type*))
  "Returns the correct form to quit lisp, based on the value of lisp-implementation.
Can optionally be given a unix status CODE to exit with"
  (format nil (slot-value (get-lisp-implementation implementation-type) 'quit-format)
	  (or code 0)))

(defun save-image-form (filepath &optional (implementation-type *lisp-implementation-type*))
  "Returns the lisp form to save the lisp image to the given filepath"
  (format nil (slot-value (get-lisp-implementation implementation-type) 'dump-format)
	  filepath))
