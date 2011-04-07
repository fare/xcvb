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

(defun safe-file-write-date (p)
  (and p (asdf::probe-file* p) (ignore-errors (file-write-date p))))

;; XXX I am suspicious of this code since if you pass it a pathname to
;; a file which doesn't exist, the oldest timestamp is the most future
;; timestamp which could exist.
(defun oldest-timestamp (pathnames)
  (loop :with ot = most-positive-single-float
    :for p :in pathnames
    :for pt = (safe-file-write-date p)
    :when (and pt (< pt ot)) :do (setf ot pt)
    :finally (return ot)))

;; XXX I am suspicious of this code since if you pass it a pathname to
;; a file which doesn't exist, the newest timestamp is the most in the
;; past timestamp which could exist.
(defun newest-timestamp (pathnames)
  (loop :with nt = most-negative-single-float
    :for p :in pathnames
    :for pt = (safe-file-write-date p)
    :when (and pt (> pt nt)) :do (setf nt pt)
    :finally (return nt)))

;; XXX Broken when files are written in subsecond intervals.
;; XXX Touches the disk too much to get timestamps.
(defun all-newer-pathnames-p (output-pn input-pn)
  (and
   ;; If any output files don't exist, then they can't possibly be
   ;; newer than the inputs!
   (every #'safe-file-write-date output-pn)
   ;; If any input files have newer timestamps than output files...
   (>= (newest-timestamp input-pn)
       (oldest-timestamp output-pn))))

;; FIXME XXX Test this!
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
      (let ((text (run-program/read-output-string external-command)))
        (when (plusp (length text))
          (log-format 5 "command emitted following text:~%~A~&" text))))))

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
