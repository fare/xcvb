#+xcvb
(module
  (:author ("Francois-Rene Rideau")
   :maintainer "Francois-Rene Rideau"
   ;; :run-depends-on ("string-escape")
   :depends-on ("profiling" "specials" "virtual-pathnames"
                "static-traversal" "computations"
                "external-commands" "target-lisp-commands" "commands")))

(in-package :xcvb)

(defclass blaze-traversal (static-traversal)
  ())

#|
;;; TODO: find labels!
(defmethod effective-namestring ((env blaze-traversal) fullname)
  (fullname-enough-namestring env fullname))
(defmethod pseudo-effective-namestring ((env blaze-traversal) fullname)
  (pseudo-fullname-enough-namestring env fullname))
|#
(defun computations-to-blaze-BUILD (env)
  (with-output-to-string (s)
    (dolist (computation *computations*)
      (write-computation-to-blaze-BUILD env s computation))))

(defun write-blaze-BUILD (fullname &key output-path)
  "Write a blaze BUILD file to output-path"
  (multiple-value-bind (target-dependency build directory) (handle-target fullname)
    (declare (ignore build directory))
    (let* ((env (make-instance 'blaze-traversal))
           (default-output-path (subpathname *default-pathname-defaults* "BUILD"))
           (actual-output-path
            (if output-path
                (merge-pathnames* output-path default-output-path)
                default-output-path))
           (blaze-BUILD-path (ensure-absolute-pathname actual-output-path))
           (blaze-BUILD-dir (pathname-directory-pathname blaze-BUILD-path))
           (*default-pathname-defaults* blaze-BUILD-dir)
           (*print-pretty* nil); otherwise SBCL will slow us down a lot.
           (lisp-env-var (lisp-environment-variable-name :prefix nil))
           (*lisp-executable-pathname* ;; magic escape!
            (list :blaze-BUILD "${" lisp-env-var "}")))
      (log-format 9 "output-path: ~S" output-path)
      (log-format 9 "default-output-path: ~S" default-output-path)
      (log-format 9 "actual-output-path: ~S" actual-output-path)
      (log-format 6 "blaze-BUILD-path: ~S" blaze-BUILD-path)
      (log-format 9 "*default-pathname-defaults*: ~S" *default-pathname-defaults*)
      (log-format 7 "workspace: ~S" *workspace*)
      (log-format 7 "cache: ~S" *cache*)
      (log-format 7 "object-cache: ~S" *object-cache*)
      ;; Pass 1: Traverse the graph of dependencies
      (log-format 8 "T=~A building dependency graph" (get-universal-time))
      (graph-for env target-dependency)
      ;; Pass 2: Build a Blaze-BUILD out of the *computations*
      (log-format 8 "T=~A computing blaze-BUILD body" (get-universal-time))
      (log-format 8 "All *computations*=~%~S" (reverse *computations*))
      (let ((body (computations-to-blaze-BUILD env)))
        (log-format 8 "T=~A creating blaze-BUILD" (get-universal-time))
        (ensure-directories-exist blaze-BUILD-path)
        (with-open-file (out blaze-BUILD-path
                             :direction :output
                             :if-exists :supersede)
          (log-format 8 "T=~A printing blaze-BUILD" (get-universal-time))
          (write-blaze-BUILD-prelude
           :stream out :lisp-env-var lisp-env-var)
          (princ body out)
          (write-blaze-BUILD-conclusion out)))
      (log-format 8 "T=~A done" (get-universal-time))
      ;; Return data for use by the non-enforcing Blaze-BUILD backend.
      (values blaze-BUILD-path blaze-BUILD-dir))))

(defun write-blaze-BUILD-prelude (&key stream lisp-env-var)
  (let ((vars (list lisp-env-var))
        (implementation-pathname
         (or *target-lisp-executable-pathname*
             (lisp-implementation-name (get-lisp-implementation)))))
    (write-generated-file-warning stream implementation-pathname)
    vars))

;; TODO: clean
;; * a clean-xcvb target that removes the object directory
(defun write-blaze-BUILD-conclusion (&optional stream)
  (format stream "~%"))

(defmethod vp-namestring :around ((env blaze-traversal) vp)
  (let ((namestring (call-next-method)))
    namestring))

#|
(defmethod grain-pathname-text ((env blaze-traversal) (grain file-grain))
  (let ((pathname (call-next-method)))
    (values (escape-sh-token-for-blaze-BUILD (enough-namestring pathname)) pathname)))
|#

(defmethod grain-pathname-text :around ((env blaze-traversal) grain)
  (declare (ignorable env grain))
  (or (call-next-method) ""))

(defun blaze-BUILD-commands-for-computation (env computation-command)
  (mapcar 'shell-tokens-to-Makefile ;; BOGUS
          (external-commands-for-computation env computation-command)))

(defun write-computation-to-blaze-BUILD (env stream computation)
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
                    (Blaze-BUILD-commands-for-computation env command)))
          (format stream "~C@~A~%" #\Tab c)))
      (terpri stream))))

(defmethod grain-pathname-text ((env blaze-traversal) (grain phony-grain))
  (declare (ignore env))
  (let ((n #|(normalize-name-for-blaze-BUILD|# (princ-to-string (fullname grain))));)
    n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make-Blaze-BUILD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command make-blaze-BUILD
    (("make-blaze-BUILD" "mkmk" "mm")
     (&rest keys &key)
     `(,@+build-option-spec+
       ,@+setup-option-spec+
       ,@+base-image-option-spec+
       ,@+source-registry-option-spec+
       (("output-path" #\o) :type string :initial-value "BUILD" :documentation "specify output path")
       ,@+xcvb-program-option-spec+
       ,@+workspace-option-spec+
       ,@+install-option-spec+
       ,@+lisp-implementation-option-spec+
       ,@+cfasl-option-spec+
       (("master" #\m) :type boolean :optional t :initial-value t :documentation "enable XCVB-master")
       ,@+verbosity-option-spec+
       ,@+profiling-option-spec+)
     "Create some Blaze-BUILD"
     "Create Blaze-BUILD rules to build a project." ignore)
  (apply 'make-build :blaze-BUILD-only t keys))

(define-command blaze-build
    (("blaze-build" "bb")
     (&rest keys &key blaze-BUILD-only (retry t) (exit t))
     `(,@+make-blaze-BUILD-option-spec+
       (("parallel" #\j) :type boolean :optional t :initial-value t :documentation "build in parallel"))
     "Use Make to build your project (in parallel)"
     "Create Blaze-BUILD rules to build a project, use them."
     (build output-path parallel))
  (apply 'handle-global-options keys)
  (with-maybe-profiling ()
    (multiple-value-bind (blaze-BUILD-path blaze-BUILD-dir)
        (write-blaze-BUILD build :output-path output-path)
      (if blaze-BUILD-only
          (values blaze-BUILD-path blaze-BUILD-dir)
          (let ((code (invoke-make
                       :directory blaze-BUILD-dir :blaze-BUILD blaze-BUILD-path :parallel parallel
                       :ignore-error-status t)))
            (unless (zerop code)
              (when retry
                (invoke-make
                 :directory blaze-BUILD-dir :blaze-BUILD blaze-BUILD-path :parallel parallel
                 :ignore-error-status t :env '("XCVB_DEBUGGING=t"))))
            (if exit
                (exit code)
                (values code blaze-BUILD-dir blaze-BUILD-path)))))))
