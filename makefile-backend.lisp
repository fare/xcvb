#+xcvb
(module
  (:author ("Francois-Rene Rideau" "Stas Boukarev")
   :maintainer "Francois-Rene Rideau"
   ;; :run-depends-on ("string-escape")
   :depends-on ("profiling" "specials" "virtual-pathnames"
                "static-traversal" "computations" "extract-target-properties"
                "external-commands" "target-lisp-commands" "commands")))

(in-package :xcvb)

(declaim (optimize (debug 3) (safety 3) (speed 2) (compilation-speed 0)))

(defclass makefile-traversal ()
  ())

(defclass static-makefile-traversal (static-traversal makefile-traversal)
  ())

(defvar *makefile-target-directories-to-mkdir* ())
(defvar *makefile-target-directories* (make-hash-table :test 'equal))
(defvar *makefile-phonies* ())

(defmethod effective-namestring ((env makefile-traversal) fullname)
  (fullname-enough-namestring env fullname))
(defmethod pseudo-effective-namestring ((env makefile-traversal) fullname)
  (pseudo-fullname-enough-namestring env fullname))

(defun computations-to-Makefile (env)
  (with-output-to-string (s)
    (dolist (computation *computations*)
      (write-computation-to-makefile env s computation))))

(defun write-makefile (fullname &key output-path)
  "Write a Makefile to output-path with information about how to compile the specified BUILD."
  (multiple-value-bind (target-dependency build directory) (handle-target fullname)
    (declare (ignore build directory))
    (let* ((env (make-instance 'static-makefile-traversal))
           (default-output-path (subpathname *workspace* "xcvb.mk"))
           (actual-output-path
            (if output-path
                (merge-pathnames* output-path default-output-path)
                default-output-path))
           (makefile-path (ensure-pathname-absolute actual-output-path))
           (makefile-dir (pathname-directory-pathname makefile-path))
           (*default-pathname-defaults* makefile-dir)
           (*print-pretty* nil); otherwise SBCL will slow us down a lot.
           (*makefile-target-directories* (make-hash-table :test 'equal))
           (*makefile-target-directories-to-mkdir* nil)
           (*makefile-phonies* nil)
           (lisp-env-var (lisp-environment-variable-name :prefix nil))
           (*lisp-executable-pathname* ;; magic escape!
            (list :makefile "${" lisp-env-var "}")))
      (log-format 9 "output-path: ~S" output-path)
      (log-format 9 "default-output-path: ~S" default-output-path)
      (log-format 9 "actual-output-path: ~S" actual-output-path)
      (log-format 6 "makefile-path: ~S" makefile-path)
      (log-format 9 "*default-pathname-defaults*: ~S" *default-pathname-defaults*)
      (log-format 7 "workspace: ~S" *workspace*)
      (log-format 7 "cache: ~S" *cache*)
      (log-format 7 "object-cache: ~S" *object-cache*)
      ;; Pass 1: Traverse the graph of dependencies
      (log-format 8 "T=~A building dependency graph" (get-universal-time))
      (graph-for env target-dependency)
      ;; Pass 2: Build a Makefile out of the *computations*
      (log-format 8 "T=~A computing makefile body" (get-universal-time))
      (log-format 8 "All *computations*=~%~S" (reverse *computations*))
      (let ((body (computations-to-Makefile env)))
        (log-format 8 "T=~A creating makefile" (get-universal-time))
        (ensure-directories-exist makefile-path)
        (with-open-file (out makefile-path
                             :direction :output
                             :if-exists :supersede)
          (log-format 8 "T=~A printing makefile" (get-universal-time))
          (write-makefile-prelude
           :stream out :lisp-env-var lisp-env-var)
          (princ body out)
          (write-makefile-conclusion out)))
      (log-format 8 "T=~A done" (get-universal-time))
      ;; Return data for use by the non-enforcing Makefile backend.
      (values makefile-path makefile-dir))))

(defparameter +generated-file-warning-start+
  "### This file was automatically created by XCVB")

(defun write-generated-file-warning (stream implementation-pathname)
  (format stream "~
~A ~A with the arguments~%~
### ~{~A~^ ~}~%~
### It may have been specialized to the target implementation ~A~%~
### from ~A with the following features:~%~
###   ~S~%~%~
### DO NOT EDIT! Changes will be lost when XCVB overwrites this file.~%~%"
          +generated-file-warning-start+
          *xcvb-version* *arguments* *lisp-implementation-type*
          implementation-pathname *target-system-features*))

(defun write-makefile-prelude (&key stream lisp-env-var)
  (let ((vars (list lisp-env-var))
        (implementation-pathname
         (or *target-lisp-executable-pathname*
             (lisp-implementation-name (get-lisp-implementation)))))
    (write-generated-file-warning stream implementation-pathname)
    (format stream "X~A ?= ~A~%~2:*~A ?= ${X~:*~A}~%" lisp-env-var implementation-pathname)
    (case *lisp-implementation-type*
      ((:ccl :sbcl)
       (let ((dir-var (lisp-implementation-directory-variable (get-lisp-implementation))))
         (format stream "_o_ = ~A~%~A ?= $(shell $(_o_))~%"
                 (escape-string-hashes
                  (shell-tokens-to-Makefile
                   (lisp-invocation-arglist
                    :cross-compile nil
                    :eval (format nil "(progn (princ ~A)(terpri)~A)"
                                  (association '*lisp-implementation-directory*
                                               *target-properties-variables*)
                                  (quit-form)))))
                 dir-var)
         (append1f vars dir-var))))
    (format stream "export~{ ~A~}~%~%" vars))
  (format stream "
XCVB_EOD :=
ifneq ($(wildcard ~A),~:*~A)
  XCVB_EOD := xcvb-ensure-object-directories
endif~2%"
          (join-strings
           (mapcar #'escape-string-for-Makefile
                   (mapcar 'enough-namestring
                           *makefile-target-directories-to-mkdir*))
           :separator " ")))

;; TODO: clean
;; * a clean-xcvb target that removes the object directory
(defun write-makefile-conclusion (&optional stream)
  (format stream "
xcvb-ensure-object-directories:
	mkdir -p ~A

.PHONY: force xcvb-ensure-object-directories~{ ~A~}~2%"
          (shell-tokens-to-Makefile
           (mapcar 'enough-namestring *makefile-target-directories-to-mkdir*))
          *makefile-phonies*))

(defun ensure-makefile-will-make-pathname (env namestring)
  (declare (ignore env))
  (let* ((p (position #\/ namestring :from-end t :end nil))
         (dir (subseq namestring 0 p)))
    (unless (gethash dir *makefile-target-directories*)
      (setf (gethash dir *makefile-target-directories*) t)
      (unless (find-if (lambda (d) (portable-namestring-prefix<= dir d))
                       *makefile-target-directories-to-mkdir*)
        (setf *makefile-target-directories-to-mkdir*
              (cons dir
                    (remove-if (lambda (d) (portable-namestring-prefix<= d dir))
                               *makefile-target-directories-to-mkdir*))))))
  (values))

(defmethod vp-namestring :around ((env makefile-traversal) vp)
  (let ((namestring (call-next-method)))
    (when (eq (vp-root vp) :obj)
      (ensure-makefile-will-make-pathname env namestring))
    namestring))

(defmethod grain-pathname-text ((env makefile-traversal) (grain file-grain))
  (let ((pathname (call-next-method)))
    (values (escape-sh-token-for-Makefile (enough-namestring pathname)) pathname)))

(defmethod grain-pathname-text :around ((env makefile-traversal) grain)
  (declare (ignorable env grain))
  (or (call-next-method) ""))

(defun Makefile-commands-for-computation (env computation-command)
  (mapcar 'shell-tokens-to-Makefile
          (external-commands-for-computation env computation-command)))

(defun write-computation-to-makefile (env stream computation)
  (with-accessors ((command computation-command)
                   (inputs computation-inputs)
                   (outputs computation-outputs)) computation
    (let* ((first-output (first outputs))
           (dependencies (mapcar #'grain-computation-target inputs))
           (target (grain-pathname-text env first-output))
           (other-outputs (rest outputs)))
      (dolist (o other-outputs)
        (format stream "~&~A: ~A~%" (grain-pathname-text env o) target))
      (format stream "~&~A:~{~@[ ~A~]~}~@[~A~] ${XCVB_EOD}~%"
              target
              (mapcar/ #'grain-pathname-text env dependencies)
              (asdf-dependency-text env first-output dependencies))
      (when command
        (dolist (c (cons
                    (format nil "echo Building ~A" target)
                    (Makefile-commands-for-computation env command)))
          (format stream "~C@~A~%" #\Tab c)))
      (terpri stream))))

;;; This is only done for images, not for individual files.
;;; For finer granularity, we could possibly define for each ASDF system
;;; (and implementation) a variable
;;; ASDF_CL_PPCRE_UP_TO_DATE := $(shell ...)
;;; but that would require more work.
;;; Also, it doesn't make sense to try to beat ASDF at its own game:
;;; if you really want proper dependencies,
;;; you'll migrate from ASDF to XCVB anyway.
(defun asdf-dependency-text (env output inputs)
  (with-nesting ()
    (when (image-grain-p output))
    (let ((asdf-grains (remove-if-not #'asdf-grain-p inputs))))
    (when asdf-grains)
    (let* ((image-namestring (grain-namestring env output))
           (pathname-text (escape-sh-token-for-Makefile
                           (enough-namestring image-namestring)))))
    (with-output-to-string (s)
      (format s " $(shell [ -f ~A ] && " pathname-text)
      (shell-tokens-to-Makefile
       (lisp-invocation-arglist
        :image-path image-namestring
        :eval (format nil "(xcvb-driver::asdf-systems-up-to-date~{ ~S~})"
                      (mapcar #'asdf-grain-system-name asdf-grains)))
       s)
      (format s " || echo force)"))))

(defmethod grain-pathname-text ((env makefile-traversal) (grain phony-grain))
  (declare (ignore env))
  (let ((n (normalize-name-for-makefile (princ-to-string (fullname grain)))))
    (pushnew n *makefile-phonies* :test 'equal)
    n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make-Makefile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command make-makefile
    (("make-makefile" "mkmk" "mm")
     (&rest keys &key)
     `(,@+build-option-spec+
       ,@+setup-option-spec+
       ,@+base-image-option-spec+
       ,@+source-registry-option-spec+
       (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
       ,@+xcvb-program-option-spec+
       ,@+workspace-option-spec+
       ,@+install-option-spec+
       ,@+lisp-implementation-option-spec+
       ,@+cfasl-option-spec+
       (("master" #\m) :type boolean :optional t :initial-value t :documentation "enable XCVB-master")
       ,@+verbosity-option-spec+
       ,@+profiling-option-spec+)
     "Create some Makefile"
     "Create Makefile rules to build a project." ignore)
  (apply 'make-build :makefile-only t keys))


(defun read-integer (x)
  (parse-integer x :junk-allowed t))

(defun slurp-stream-integer (input-stream)
  (read-integer (slurp-stream-string input-stream)))

(defmethod slurp-input-stream ((x (eql :integer)) input-stream
                               &key &allow-other-keys)
  (slurp-stream-integer input-stream))

(defun ncpus ()
  (ignore-errors
    (cond
      ((featurep :linux)
       (run-program/ '("grep" "-c" "^processor " "/proc/cpuinfo") :output :integer))
      ((featurep :darwin)
       (run-program/ '("sysctl" "-n" "hw.ncpu") :output :integer))
      ((os-windows-p)
       (read-integer (getenv "NUMBER_OF_PROCESSORS"))))))

(defun make-parallel-flag ()
  (if-bind (ncpus) (ncpus)
    (format nil "-l~A" (1+ ncpus))
    "-j"))

(defun invoke-make (&key target directory makefile parallel ignore-error-status env)
  (let* ((make (or (getenv "MAKE") "make"))
         (make-command
          `(,@(when env `("env" ,@env))
            ,make
            ,@(when parallel (list (make-parallel-flag)))
            ,@(when directory `("-C" ,(namestring directory)))
            ,@(when makefile `("-f" ,(namestring makefile)))
            ,@(when target (ensure-list target)))))
      (log-format 6 "Building with ~S" make-command)
      (run-program/ ;; for side-effects
       make-command ; (strcat (escape-shell-command make-command) " >&2")
       :ignore-error-status ignore-error-status)))

(define-command make-build
    (("make-build" "mkb" "mb")
     (&rest keys &key makefile-only (retry t) (exit t))
     `(,@+make-makefile-option-spec+
       (("parallel" #\j) :type boolean :optional t :initial-value t :documentation "build in parallel"))
     "Use Make to build your project (in parallel)"
     "Create Makefile rules to build a project, use them."
     (build output-path parallel))
  (apply 'handle-global-options keys)
  (with-maybe-profiling ()
    (multiple-value-bind (makefile-path makefile-dir)
        (write-makefile build :output-path output-path)
      (if makefile-only
          (values makefile-path makefile-dir)
          (let ((code (invoke-make
                       :directory makefile-dir :makefile makefile-path :parallel parallel
                       :ignore-error-status t)))
            (unless (zerop code)
              (when retry
                (invoke-make
                 :directory makefile-dir :makefile makefile-path :parallel parallel
                 :ignore-error-status t :env '("XCVB_DEBUGGING=t"))))
            (if exit
                (exit code)
                (values code makefile-dir makefile-path)))))))
