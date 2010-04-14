#+xcvb
(module
  (:author ("Francois-Rene Rideau" "Stas Boukarev")
   :maintainer "Francois-Rene Rideau"
   ;; :run-depends-on ("string-escape")
   :depends-on ("profiling" "static-traversal" "computations"
                "virtual-pathnames" "driver-commands" "main")))

(in-package :xcvb)

(defclass makefile-traversal ()
  ())

(defclass static-makefile-traversal (static-traversal makefile-traversal)
  ())

(defvar *makefile-target-directories* ())
(defvar *makefile-phonies* ())

(defun computations-to-Makefile (env)
  (with-output-to-string (s)
    (dolist (computation *computations*)
      (write-computation-to-makefile env s computation))))

(defun write-makefile (fullname &key output-path)
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
           (*makefile-target-directories* nil)
           (*makefile-phonies* nil)
           (env (make-instance 'static-makefile-traversal)))
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
      (let ((body (computations-to-Makefile env)))
        (log-format 8 "T=~A creating makefile" (get-universal-time))
        (with-open-file (out makefile-path
                             :direction :output
                             :if-exists :supersede)
          (log-format 8 "T=~A printing makefile" (get-universal-time))
          (write-makefile-prelude out)
          (princ body out)
          (write-makefile-conclusion out)))
      (log-format 8 "T=~A done" (get-universal-time))
      ;; Return data for use by the non-enforcing Makefile backend.
      (values makefile-path makefile-dir))))

(defun write-makefile-prelude (&optional stream)
  (let ((directories
         (join-strings
          (mapcar #'escape-string-for-Makefile
                  *makefile-target-directories*)
          :separator " ")))
    (format stream "~
### This file was automatically created by XCVB ~A with the arguments~%~
### ~{~A~^ ~}~%~
### It may have been specialized to the target implementation ~A~%~
### with the following features:~%~
###   ~S~%~%~
### DO NOT EDIT! Changes will be lost when XCVB overwrites this file.

XCVB_EOD :=
ifneq ($(wildcard ~A),~A)
  XCVB_EOD := xcvb-ensure-object-directories
endif~2%"
            *xcvb-version* *arguments* *lisp-implementation-type* *features*
            directories directories)
    (flet ((export-directory (x)
             (format stream "~%~A ?= ~A~%export ~A~%~%"
                     x (but-last-char (namestring *lisp-implementation-directory*)) x)))
      (case *lisp-implementation-type*
        ((:sbcl) (export-directory "SBCL_HOME"))
        ((:ccl) (export-directory "CCL_DEFAULT_DIRECTORY"))))))

;; TODO: clean
;; * a clean-xcvb target that removes the object directory
(defun write-makefile-conclusion (&optional stream)
  (format stream "
xcvb-ensure-object-directories:
	mkdir -p ~A

.PHONY: force xcvb-ensure-object-directories~{ ~A~}~2%"
          (shell-tokens-to-Makefile *makefile-target-directories*)
          *makefile-phonies*))

(defun ensure-makefile-will-make-pathname (env namestring)
  (declare (ignore env))
  (let* ((p (position #\/ namestring :from-end t :end nil))
         (dir (subseq namestring 0 p)))
    (unless (find-if (lambda (d) (portable-namestring-prefix<= dir d))
                     *makefile-target-directories*)
      (setf *makefile-target-directories*
            (remove-if (lambda (d) (portable-namestring-prefix<= d dir))
                       *makefile-target-directories*))
      (push dir *makefile-target-directories*)))
  (values))

(defmethod vp-namestring :around ((env makefile-traversal) vp)
  (let ((namestring (call-next-method)))
    (when (eq (vp-root vp) :obj)
      (ensure-makefile-will-make-pathname env namestring))
    namestring))

;; Renaming of targets ensures reasonable atomicity
;; whereas CL implementations may create bad invalid stale output files
;; when interrupted in the middle of their computation,
;; -- whether a bad bug is found in the way the user stresses the compiler,
;; or the process is killed in the midst of an unsuccessful debug attempt,
;; or the plug is simply pulled on the computer.
;; This isn't done in the target Lisp side, because
;; CL implementations don't usually do that for you implicitly, and
;; while we could do it explicitly for :compile-lisp,
;; doing it for :create-image would be a pain in at least SBCL,
;; where we would have to fork and wait for a subprocess to SAVE-LISP-AND-DIE
;; which would make the target driver much more complex than desired.

(defvar *renamed-targets* ()
  "alist of targets really desired, and the temporary names under which the XCVB driver commands
will create the desired content. An atomic rename() will have to be performed afterwards.")

(defun register-renamed-target (target tempname)
  (push (cons target tempname) *renamed-targets*))

(defun rename-target (target tempname)
  (register-renamed-target target tempname)
  tempname)

(defun tempname-target (target)
  (let* ((path (pathname target))
         (tempname (namestring
                    (make-pathname :name (strcat (pathname-name path) "__temp")
                                   :defaults path))))
    (rename-target target tempname)))

(define-simple-dispatcher Makefile-commands-for-computation #'Makefile-commands-for-computation-atom)

(defun Makefile-commands-for-computation-atom (env computation-command)
  (declare (ignore env))
  (error "Invalid computation ~S" computation-command))

(defun Makefile-commands-for-computation (env computation-command)
  ;; We rename secondary targets first, according to the theory that
  ;; in case of interruption, the primary target will be re-built which will
  ;; cause the secondary targets to be implicitly re-built before success.
  (let* ((*renamed-targets* nil)
         (commands (Makefile-commands-for-computation-dispatcher env computation-command)))
    (append commands
	    (loop :for (target . tempname) :in *renamed-targets*
		  :collect (shell-tokens-to-Makefile (list "mv" tempname target))))))

(define-Makefile-commands-for-computation :xcvb-driver-command (env keys &rest commands)
  (list
   (lisp-invocation-for env keys (xcvb-driver-commands-to-shell-token env commands))))

(define-Makefile-commands-for-computation :compile-file-directly (env fullname &optional cfasl)
  (list
   (lisp-invocation-for env ()
    (compile-file-directly-shell-token env fullname cfasl))))

(define-Makefile-commands-for-computation :progn (env &rest commands)
  (loop :for command :in commands
    :append (Makefile-commands-for-computation env command)))

#|
(define-Makefile-commands-for-computation :shell-command (env command)
  (declare (ignore env))
  (list (escape-string-for-Makefile command)))

(define-Makefile-commands-for-computation :exec-command (env &rest argv)
  (declare (ignore env))
  (list (shell-tokens-to-Makefile argv)))
|#

(define-Makefile-commands-for-computation :make-manifest (env manifest &rest commands)
  (list (shell-tokens-to-Makefile
         (cmdize 'xcvb 'make-manifest
                 :output (pseudo-fullname-enough-namestring env manifest)
                 :spec (let ((manifest-spec (commands-to-manifest-spec env commands)))
                         (with-safe-io-syntax ()
                           (write-to-string manifest-spec :case :downcase)))))))

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
           (pathname-text (escape-shell-token-for-Makefile
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
  `((("build" #\b) :type string :optional nil :documentation "specify what system to build")
    (("setup" #\s) :type string :optional t :documentation "specify a Lisp setup file")
    ,@+source-registry-option-spec+
    (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
    (("object-directory" #\O) :type string :initial-value "obj" :documentation "specify object directory")
    (("lisp-implementation" #\i) :type string :initial-value "sbcl" :documentation "specify type of Lisp implementation")
    (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
    (("disable-cfasl" #\C) :type boolean :optional t :documentation "disable the CFASL feature")
    (("debugging" #\Z) :type boolean :optional t :documentation "enable debugging")
    (("verbosity" #\v) :type integer :initial-value 5 :documentation "set verbosity")
    (("base-image" #\B) :type boolean :optional t :initial-value t :documentation "use a base image")
    (("master" #\m) :type boolean :optional t :initial-value t :documentation "enable XCVB-master")
    (("profiling" #\P) :type boolean :optional t :documentation "profiling")))

(defun make-makefile (&rest keys &key
                      source-registry setup verbosity output-path
                      build lisp-implementation lisp-binary-path
                      disable-cfasl master object-directory base-image profiling debugging)
  (declare (ignore source-registry setup verbosity
                   lisp-implementation lisp-binary-path
                   disable-cfasl master object-directory base-image debugging))
  (with-maybe-profiling (profiling)
    (apply 'handle-global-options keys)
    (write-makefile build :output-path output-path)))
