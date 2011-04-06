#+xcvb
(module
  (:author ("Francois-Rene Rideau" "Peter Keller")
   :maintainer "Francois-Rene Rideau"
   ;; :run-depends-on ("string-escape")
   :depends-on ("profiling" "static-traversal" "computations"
                "virtual-pathnames" "driver-commands" "main")))

(in-package :xcvb)

(defclass run-program-traversal (static-traversal makefile-traversal)
  ())

(defvar *makefile-target-directories-to-mkdir* ())
(defvar *makefile-target-directories* (make-hash-table :test 'equal))
(defvar *makefile-phonies* ())

(defun simple-build (fullname &key output-path)
  "Write a Makefile to output-path with information about how to compile the specified BUILD."
  (multiple-value-bind (target-dependency build) (handle-target fullname)
    (let* ((default-output-path (merge-pathnames "xcvb.mk" (grain-pathname build)))
           (actual-output-path
            (if output-path
                (merge-pathnames output-path default-output-path)
                default-output-path))
           (makefile-path (ensure-absolute-pathname actual-output-path))
           (makefile-dir (pathname-directory-pathname makefile-path))
           (*default-pathname-defaults* makefile-dir)
           (*print-pretty* nil); otherwise SBCL will slow us down a lot.
           (env (make-instance 'run-program-traversal)))
      (log-format 9 "output-path: ~S" output-path)
      (log-format 9 "default-output-path: ~S" default-output-path)
      (log-format 9 "actual-output-path: ~S" actual-output-path)
      (log-format 6 "makefile-path: ~S" makefile-path)
      (log-format 9 "*default-pathname-defaults*: ~S" *default-pathname-defaults*)
      (log-format 7 "object-directory: ~S" *object-directory*)
      ;; Pass 1: Traverse the graph of dependencies
      (log-format 8 "T=~A building dependency graph" (get-universal-time))
      (graph-for env target-dependency)
      ;; Pass 2: Execute the ordered computations contained in *computations*
      (log-format 8 "Attempting to serially execute *computations*")
      (run-computations-serially env))))

;; FIXME to do the right thing.
(defun run-computations-serially (env)
  (log-format 8 "All *computations* = ~S" (reverse *computations*))
  (dolist (computation (reverse *computations*))
    (log-format 8 "Running computation: ~S" computation)
    (run-computation env computation)))

;; XXX Broken when files are written in subsecond intervals, Fix for when
;; output-files don't even exist yet.
(defun all-newer-pathnames-p (output-pn input-pn)
  (every #'(lambda (out-pn in-pn)
	     (> (file-write-date in-pn) (file-write-date out-pn)))
	 output-pn
	 input-pn))
  
;; FIXME XXX Test this! Fix why I get an unbound symbol in finding the
;; grain-pathname of the outputs list...
(defun run-computation (env computation)
  (let* ((inputs (computation-inputs computation)) ; a grain
	 (outputs (computation-outputs computation)) ; a grain
	 (command (computation-command computation)) ; in the xcvb command language, as understood by external-commands-for-computation
	 (input-pathnames (progn (log-format 9 "input-grain = ~S" inputs)
				 (mapcar #'grain-pathname inputs)))
	 (output-pathnames (progn (log-format 9 "output-grain = ~S" outputs)
				  (mapcar #'grain-pathname outputs))))
    
    ;; first, check the outputs are newew than the inputs, if so, bail.
    (when (and (all-newer-pathnames-p output-pathnames input-pathnames) ; same checking as with make
	       #|(not *force*)|#) ; special force computation -- TBD
      (return-from run-computation t))
    ;; create the output directories
    (dolist (output-pn output-pathnames)
      (ensure-directories-exist output-pn))
    (dolist (external-command (external-commands-for-computation env command))
      (run-program/read-output-lines external-command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make-Makefile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +simple-build-option-spec+
  `((("build" #\b) :type string :optional nil :documentation "specify what system to build")
    ,@+setup-option-spec+
    ,@+source-registry-option-spec+
    (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
    (("object-directory" #\O) :type string :initial-value "obj" :documentation "specify object directory")
    ,@+lisp-implementation-option-spec+
    ,@+cfasl-option-spec+
    (("use-base-image" #\B) :type boolean :optional t :initial-value t :documentation "use a base image")
    (("master" #\m) :type boolean :optional t :initial-value t :documentation "enable XCVB-master")
    ,@+verbosity-option-spec+
    ,@+profiling-option-spec+))

(defun simple-build-command
    (&rest keys &key
     source-registry setup verbosity output-path
     build lisp-implementation lisp-binary-path define-feature undefine-feature
     disable-cfasl master object-directory use-base-image profiling debugging)
  (declare (ignore source-registry setup verbosity
                   lisp-implementation lisp-binary-path define-feature undefine-feature
                   disable-cfasl master object-directory use-base-image debugging))
  (with-maybe-profiling (profiling)
    (apply 'handle-global-options keys)
    (simple-build build :output-path output-path)))
