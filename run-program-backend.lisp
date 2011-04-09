#+xcvb
(module
  (:author ("Francois-Rene Rideau" "Peter Keller")
   :maintainer "Francois-Rene Rideau"
   :depends-on ("static-traversal" "main")))

(in-package :xcvb)

(defclass run-program-traversal (static-traversal timestamp-based-change-detection)
  ())

(defun simple-build (fullname &key output-path force)
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
      (run-computations-serially env :force force))))

(defun run-computations-serially (env &key force)
  (log-format 8 "All *computations* = ~S" (reverse *computations*))
  (dolist (computation (reverse *computations*))
    (log-format 8 "Running computation: ~S" computation)
    (run-computation env computation :force force)))

(defun run-computation (env computation &key force)
  (when (and (not force) (already-computed-p env computation))
    ;; first, check the outputs are newew than the inputs, if so, bail.
    ;; same checking as with make
    (log-format 5 "Nothing to regenerate!~%")
    (return-from run-computation t))
  (let* ((outputs (computation-outputs computation))
         (output-pathnames (loop :for o :in outputs :when (file-grain-p o)
                             :collect (grain-pathname-text env o)))
         (command (computation-command computation))) ; in the xcvb command language
    ;; create the output directories
    (dolist (output-pn output-pathnames)
      (ensure-directories-exist output-pn))
    (dolist (external-command (external-commands-for-computation env command))
      (log-format 5 "running:~{ ~A~}" (mapcar #'escape-shell-token external-command))
      (run-program/echo-output external-command :prefix "command output: "))
    (mapcar/ 'update-change-information env outputs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make-Makefile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: have only one build command. Have it multiplex the various build backends.

(defparameter +simple-build-option-spec+
  `((("build" #\b) :type string :optional nil :documentation "specify what system to build")
    ,@+setup-option-spec+
    ,@+source-registry-option-spec+
    (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
    (("object-directory" #\O) :type string :initial-value "obj" :documentation "specify object directory")
    ,@+lisp-implementation-option-spec+
    ,@+cfasl-option-spec+
    (("force" #\F) :type boolean :optional t :initial-value nil :documentation "force building everything")
    (("use-base-image" #\B) :type boolean :optional t :initial-value t :documentation "use a base image")
    (("master" #\m) :type boolean :optional t :initial-value t :documentation "enable XCVB-master")
    ,@+verbosity-option-spec+
    ,@+profiling-option-spec+))

(defun simple-build-command
    (&rest keys &key
     source-registry setup verbosity output-path
     build lisp-implementation lisp-binary-path define-feature undefine-feature
     disable-cfasl master object-directory use-base-image profiling debugging
     force)
  (declare (ignore source-registry setup verbosity
                   lisp-implementation lisp-binary-path define-feature
		   undefine-feature disable-cfasl master object-directory
		   use-base-image debugging))
  (with-maybe-profiling (profiling)
    (apply 'handle-global-options keys)
    (simple-build build :output-path output-path :force force)))
