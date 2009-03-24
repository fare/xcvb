;;; xcvb driver to be compiled in buildee images (largely copy-pasted from cl-launch)

(in-package :cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 2) (safety 3) (compilation-speed 0) #-gcl (debug 3)
;;     	   #+sbcl (sb-ext:inhibit-warnings 3)
           #+sbcl (sb-c::merge-tail-calls 3) ;-- this plus debug 1 (or sb-c::insert-debug-catch 0 ???) should ensure all tail calls are optimized, says jsnell
	   #+cmu (ext:inhibit-warnings 3)))
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
;;  (setf *print-readably* nil ; allegro 5.0 notably will bork without this
;;        *load-verbose* nil *compile-verbose* nil *compile-print* nil)
  #+cmu (setf ext:*gc-verbose* nil)
  #+clisp (setf custom:*source-file-types* nil custom:*compiled-file-types* nil)
  #+gcl (setf compiler::*compiler-default-type* (pathname "")
              compiler::*lsp-ext* "")
  #+ecl (require 'cmp)

  ;;;; Ensure package hygiene
  (unless (find-package :xcvb-driver)
    (if (find-package :common-lisp)
       (defpackage :xcvb-driver (:use :common-lisp))
       (make-package :xcvb-driver :use '(:lisp)))))

(in-package :xcvb-driver)

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

(defun load* (dependencies)
  (map nil #'load dependencies))

(defun lcq (dependencies source object &rest args)
  "load dependencies, compile source to object, quit"
  (load* dependencies)
  (apply #'compile-file source :output-file object
	 ;; #+(or ecl gcl) :system-p #+(or ecl gcl) t
	 args)
  (quit))

(defun do-resume ()
  (when *restart* (funcall *restart*))
  (quit))

(defun resume ()
  (do-resume))

#-ecl
(defun dump-image (filename &key standalone (package :cl-user))
  (declare (ignorable filename standalone package))
  (setf *package* (find-package package))
  #+clisp
  (apply #'ext:saveinitmem filename
   :quiet t
   :start-package package
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

#-ecl
(defun create-image (image dependencies &rest flags)
  (load* dependencies)
  (apply #'dump-image image flags))

#+ecl
(defun create-image (image dependencies &key standalone)
  (let ((epilogue-code
          `(progn
            ,(if standalone '(resume) '(si::top-level)))))
    (c::builder :program (parse-namestring dump)
		:lisp-files dependencies
		:epilogue-code epilogue-code))
  (quit))
