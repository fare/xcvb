;;; Lisp implementations

(in-package :xcvb)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *lisp-implementations* (make-hash-table :test 'equal))

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
  quit-format
  dump-format)

(defmacro define-lisp-implementation (key () &rest keys)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (gethash key *lisp-implementations*)
	  (apply #'make-lisp-implementation keys))))
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
  :load-flag "--load" ;; Note: SBCL's eval can only handle one form per argument.
  :arguments-end "--end-toplevel-options"
  :image-flag "--core"
  :image-executable-p t
  :standalone-executable t ;; requires sbcl 1.0.21.24 or later.
  :argument-control t ;; a standalone-executable can control arguments
  :disable-debugger ("--disable-debugger")
  :quit-format "(sb-ext:quit ~A)"
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
  :argument-control nil ;; even a standalone-executable always accepts --clisp-x and such.
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
  :argument-control nil ;; haven't checked in a while - probably fixed now.
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
  :argument-control :evil ;; cmucl will always scan all the arguments for -eval... oops!
  :disable-debugger ("-batch")
  :quit-format "(unix:unix-exit ~A)"
  :dump-format "(extensions:save-lisp ~S)")

(defun lisp-invocation-arglist
    (&key (implementation-type *lisp-implementation-type*)
	  lisp-path
	  (lisp-flags :default)
	  image-path
	  eval
	  arguments
	  debugger)
  (with-slots (name flags disable-debugger eval-flag
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
     (when eval
       (list eval-flag eval))
     (when arguments
       (when (eq argument-control :evil)
	 (error "Can't reliably pass arguments to Lisp implementation ~A" implementation-type))
       (cons arguments-end arguments)))))

;;; Avoiding use of a compiled-in driver in the build process

(defun quit-form (&key exit-status (implementation-type *lisp-implementation-type*))
  "Returns the correct form to quit lisp, based on the value of lisp-implementation.
Can optionally be given a unix status code to exit with"
  (format nil (slot-value (get-lisp-implementation implementation-type) 'quit-format)
	  (or exit-status 0)))

(defun save-image-form (filepath &optional (implementation-type *lisp-implementation-type*))
  "Returns the lisp form to save the lisp image to the given filepath"
  (format nil (slot-value (get-lisp-implementation implementation-type) 'dump-format)
	  filepath))

