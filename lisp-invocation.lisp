;;; Lisp implementations

(in-package :xcvb)

(eval-when (:compile-toplevel :load-toplevel :execute)

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
  ;; fasl-type cfasl-type
  quit-format
  dump-format)

(defmacro define-lisp-implementation (key () &rest keys)
  `(setf (gethash ,key *lisp-implementations*)
    (apply #'make-lisp-implementation ',keys)))
)

(defun get-lisp-implementation (implementation-type)
  (or (gethash implementation-type *lisp-implementations*)
      (error "Unknown Lisp implementation type ~S" implementation-type)))

(define-lisp-implementation :sbcl ()
  :fullname "Steel Bank Common Lisp"
  :name "sbcl"
  :feature :sbcl
  ;; We purposefully specify --userinit /dev/null but NOT --sysinit /dev/null
  :flags ("--noinform" "--userinit" "/dev/null")
  :eval-flag "--eval" ;; Note: SBCL's eval can only handle one form per argument.
  :load-flag "--load"
  :arguments-end "--end-toplevel-options"
  :image-flag "--core"
  :image-executable-p t
  :standalone-executable t ;; requires sbcl 1.0.21.24 or later.
  :argument-control t
  :disable-debugger ("--disable-debugger")
  :quit-format "(sb-ext:quit :unix-status ~A)"
  :dump-format "(sb-ext:save-lisp-and-die ~S :executable t)")

(define-lisp-implementation :clisp ()
  :fullname "GNU clisp"
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

(define-lisp-implementation :ccl () ;; demand 1.2 or later.
  :fullname "Clozure CL"
  ;; formerly OpenMCL, forked from MCL, formerly Macintosh Common Lisp, nee Coral Common Lisp
  ;; Random note: (finish-output) is essential for ccl, that won't do it by default,
  ;; unlike the other lisp implementations tested.
  :name "ccl"
  :feature :clozure
  :flags ("--no-init")
  :eval-flag "--eval" ; -e
  :image-flag "--image-name" ; -I
  :image-executable-p t
  :arguments-end "--"
  :argument-control t ;; must be fixed now, but double-checking needed.
  :disable-debugger ("--batch")
  :quit-format "(ccl:quit ~A)"
  :dump-format "(save-application ~S :prepend-kernel t)")

(define-lisp-implementation :cmucl ()
  :fullname "CMU CL"
  :name "cmucl"
  :feature :cmu
  :flags ("-quiet" "-noinit")
  :eval-flag "-eval"
  :arguments-end "--"
  :image-flag "-core"
  :argument-control nil ;; cmucl will always scan all the arguments for -eval... EVIL!
  :disable-debugger ("-batch")
  :quit-format "(unix:unix-exit ~A)"
  :dump-format "(extensions:save-lisp ~S)")

(defun lisp-invocation-arglist
    (&key (implementation-type *lisp-implementation-type*)
	  (lisp-path *lisp-executable-pathname*)
	  (lisp-flags :default)
	  (image-path *lisp-image-pathname*)
          load
	  eval
	  arguments
	  (debugger *lisp-allow-debugger*))
  (with-slots (name flags disable-debugger load-flag eval-flag
	       image-flag image-executable-p standalone-executable
	       arguments-end argument-control)
      (get-lisp-implementation implementation-type)
    (append
     (when (or (null image-path) (not image-executable-p))
       (list (or lisp-path name)))
     (when (and image-path (not image-executable-p))
       (list image-flag))
     (when image-path
       (list image-path))
     (if (eq lisp-flags :default)
	 flags
	 lisp-flags)
     (unless debugger
       disable-debugger)
     (mapcan (lambda (x) (list load-flag x)) (if (listp load) load (list load)))
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

