#+xcvb
(module
  (:author ("Francois-Rene Rideau" "Stas Boukarev")
   :maintainer "Francois-Rene Rideau"
   ;; :run-depends-on ("string-escape")
   :depends-on ("profiling" "static-traversal" "computations" "specials"
                "virtual-pathnames" "driver-commands" "external-commands" "main")))

(in-package :xcvb)

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
  (multiple-value-bind (target-dependency build) (handle-target fullname)
    (let* ((env (make-instance 'static-makefile-traversal))
           (default-output-path (merge-pathnames "xcvb.mk" (grain-pathname build)))
           (actual-output-path
            (if output-path
                (merge-pathnames output-path default-output-path)
                default-output-path))
           (makefile-path (ensure-absolute-pathname actual-output-path))
           (makefile-dir (pathname-directory-pathname makefile-path))
           (*default-pathname-defaults* makefile-dir)
           (*print-pretty* nil); otherwise SBCL will slow us down a lot.
           (*makefile-target-directories* (make-hash-table :test 'equal))
           (*makefile-target-directories-to-mkdir* nil)
           (*makefile-phonies* nil)
           (lisp-env-var (lisp-environment-variable-name :prefix t))
           (*lisp-executable-pathname* ;; magic escape!
            (list :makefile "${" lisp-env-var "}")))
      (log-format 9 "output-path: ~S" output-path)
      (log-format 9 "default-output-path: ~S" default-output-path)
      (log-format 9 "actual-output-path: ~S" actual-output-path)
      (log-format 6 "makefile-path: ~S" makefile-path)
      (log-format 9 "*default-pathname-defaults*: ~S" *default-pathname-defaults*)
      (log-format 7 "object-directory: ~S" *object-directory*)
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

(defun write-makefile-prelude (&key stream lisp-env-var)
  (let ((vars (list lisp-env-var))
        (implementation-pathname
         (or *target-lisp-executable-pathname*
             (lisp-implementation-name (get-lisp-implementation)))))
    (format stream "~
### This file was automatically created by XCVB ~A with the arguments~%~
### ~{~A~^ ~}~%~
### It may have been specialized to the target implementation ~A~%~
### from ~A with the following features:~%~
###   ~S~%~%~
### DO NOT EDIT! Changes will be lost when XCVB overwrites this file.~%~%"
            *xcvb-version* *arguments* *lisp-implementation-type*
            implementation-pathname *features*)
    (format stream "~A ?= ~A~%" lisp-env-var implementation-pathname)
    (case *lisp-implementation-type*
      ((:ccl :sbcl)
       (let ((dir-var (lisp-implementation-directory-variable (get-lisp-implementation))))
         (format stream "~A ?= $(shell ~A)~%"
                 dir-var
                 (shell-tokens-to-Makefile
                  (lisp-invocation-arglist
                   :eval (format nil "(progn (princ ~A)(terpri)~A)"
                                 (association '*lisp-implementation-directory*
                                              *target-properties-variables*)
                                 (quit-form)))))
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

(defparameter +make-makefile-option-spec+
  `(,@+build-option-spec+
    ,@+setup-option-spec+
    ,@+base-image-option-spec+
    ,@+source-registry-option-spec+
    (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
    ,@+object-directory-option-spec+
    ,@+lisp-implementation-option-spec+
    ,@+cfasl-option-spec+
    (("master" #\m) :type boolean :optional t :initial-value t :documentation "enable XCVB-master")
    ,@+verbosity-option-spec+
    ,@+profiling-option-spec+))

(defun make-makefile (&rest keys &key
                      source-registry setup verbosity output-path
                      build lisp-implementation lisp-binary-path define-feature undefine-feature
                      disable-cfasl master object-directory use-base-image profiling debugging)
  (declare (ignore source-registry setup verbosity
                   lisp-implementation lisp-binary-path define-feature undefine-feature
                   disable-cfasl master object-directory use-base-image debugging))
  (with-maybe-profiling (profiling)
    (apply 'handle-global-options keys)
    (write-makefile build :output-path output-path)))
