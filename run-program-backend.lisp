#+xcvb
(module
  (:author ("Francois-Rene Rideau" "Peter Keller")
   :maintainer "Francois-Rene Rideau"
   :depends-on ("static-traversal" "main")))

(in-package :xcvb)

(defclass run-program-traversal (static-traversal makefile-traversal)
  ())

(defun run-program/echo-output (command &key prefix (stream t) ignore-error-status)
  (run-program/process-output-stream
   command #'(lambda (s) (loop :for line = (read-line s nil nil) :while line
                           :do (format stream "~@[~A~]~A~&" prefix line)))
   :ignore-error-status ignore-error-status))

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

;; FIXME to do the right thing.
(defun run-computations-serially (env &key force)
  (log-format 8 "All *computations* = ~S" (reverse *computations*))
  (dolist (computation (reverse *computations*))
    (log-format 8 "Running computation: ~S" computation)
    (run-computation env computation :force force)))

(defun safe-file-write-date (p &optional error)
  (or (and p (asdf::probe-file* p) (ignore-errors (file-write-date p)))
      (error-behaviour error)))

(defparameter *newest-time* most-positive-single-float) ; behold the Y1e31 bug!
(defparameter *oldest-time* most-negative-single-float)
(defun oldest-timestamp (pathnames)
  (or (loop :for p :in pathnames
        :minimize (safe-file-write-date p #'(lambda () (return *oldest-time*))))
      *newest-time*))
(defun newest-timestamp (pathnames &key (error t))
  (or (loop :for p :in pathnames
        :maximize (safe-file-write-date
                   p (if error
                         #'(lambda () (error "Missing input file ~S" p))
                         #'(lambda () (return *newest-time*)))))
      *oldest-time*))

;; We rely on the same approximation as make and asdf.
;; If the modified file is a generated file a previous version of which
;; was last generated and compiled in the same second, you lose. Unlikely, though.
;; More likely, if you have object files from the recent past and
;; unpack a source code update from a further past (as archived), you lose.
;; Or, if your (file)system clock is skewed and produces object files in the past
;; of the source code, you may lose in strange ways by rebuilding too much.
(defun all-newer-pathnames-p (output-pn input-pn)
  (<= (newest-timestamp input-pn) (oldest-timestamp output-pn)))


(defun run-computation (env computation &key force)
  (let* ((inputs (computation-inputs computation)) ; a grain
	 (outputs (computation-outputs computation)) ; a grain
	 (command (computation-command computation)) ; in the xcvb command language, as understood by external-commands-for-computation
	 (input-pathnames (progn (log-format 9 "input-grain = ~S" inputs)
                                 (loop :for i :in inputs :when (file-grain-p i)
                                   :collect (grain-pathname-text env i))))
	 (output-pathnames (progn (log-format 9 "output-grain = ~S" outputs)
                                 (loop :for o :in outputs :when (file-grain-p o)
                                   :collect (grain-pathname-text env o)))))

    ;; first, check the outputs are newew than the inputs, if so, bail.
    ;; same checking as with make
    (when (and (all-newer-pathnames-p output-pathnames input-pathnames)
	       (not force)) ; force computation
      (log-format 5 "Nothing to regenerate!~%")
      (return-from run-computation t))

    ;; create the output directories
    (dolist (output-pn output-pathnames)
      (ensure-directories-exist output-pn))

    (dolist (external-command (external-commands-for-computation env command))
      (log-format 5 "running:~{ ~A~}" (mapcar #'escape-shell-token external-command))
      (run-program/echo-output external-command :prefix "command output: "))))

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
