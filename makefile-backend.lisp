#+xcvb
(module
  (:author ("Francois-Rene Rideau" "Stas Boukarev")
   :maintainer "Francois-Rene Rideau"
   ;; :run-depends-on ("string-escape")
   :depends-on ("profiling" "static-traversal" "computations")))

(in-package :xcvb)

(defclass makefile-traversal ()
  ())

(defclass static-makefile-traversal (static-traversal makefile-traversal)
  ())

(defvar +lisp-pathname+ (make-pathname :type "lisp"))
(defvar +fasl-pathname+ (make-pathname :type "fasl"))
(defvar +cfasl-pathname+ (make-pathname :type "cfasl"))
(defvar +image-pathname+ (make-pathname :type "image"))

(defvar *makefile-target-directories* ())
(defvar *makefile-phonies* ())

(defun computations-to-Makefile (env)
  (with-output-to-string (s)
    (dolist (computation *computations*)
      (write-computation-to-makefile env s computation))))

(defun write-makefile (fullname &key output-path)
  "Write a Makefile to output-path with information about how to compile the specified BUILD."
  (let* ((*print-pretty* nil); otherwise SBCL will slow us down a lot.
         (fullname (canonicalize-fullname fullname))
         (target (resolve-absolute-module-name fullname))
         (build (if target (build-grain-for target)
                    (errexit 3 "User requested build ~S but it can't be found.~%~
				You may check available builds with xcvb ssp.~%" fullname)))
         (default-output-path (merge-pathnames "xcvb.mk" (grain-pathname build)))
         (output-path (if output-path (merge-pathnames output-path default-output-path) default-output-path))
         (makefile-path (ensure-absolute-pathname output-path))
         (makefile-dir (pathname-directory-pathname makefile-path))
         (*default-pathname-defaults* makefile-dir)
         (*makefile-target-directories* nil)
         (*makefile-phonies* nil)
         (env (make-instance 'static-makefile-traversal)))
    (log-format 6 "T=~A building dependency graph~%" (get-universal-time))
    (etypecase target
      (build-grain
       (graph-for-build-grain env build))
      (lisp-grain
       (graph-for-fasl env fullname)))
    (log-format 6 "T=~A building makefile~%" (get-universal-time))
    (let ((body (computations-to-Makefile env)))
      (with-open-file (out makefile-path
                           :direction :output
                           :if-exists :supersede)
        (log-format 6 "T=~A printing makefile~%" (get-universal-time))
        (write-makefile-prelude out)
        (princ body out)
        (write-makefile-conclusion out)))
    (log-format 6 "T=~A done~%" (get-universal-time))
    (values makefile-path makefile-dir)))

(defun write-makefile-prelude (&optional stream)
  (let ((directories
         (join-strings " "
                       (mapcar #'escape-string-for-Makefile
                               *makefile-target-directories*))))
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
            *xcvb-version* cl-launch:*arguments* *lisp-implementation-type* *features*
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


;;; dependency-namestring: extract Makefile-printable pathname from dependency spec
(define-simple-dispatcher dependency-namestring #'dependency-namestring-for-atom)

(defun dependency-pathname (env fullname)
  (ensure-absolute-pathname (dependency-namestring env fullname)))

(defun dependency-namestring (env fullname)
  ;; TODO: double pcheck that a namestring is only used by one fullname,
  ;; using a table to record namestring => fullname mappings
  ;; maybe also have a table the other way to cache these computations?
  (dependency-namestring-dispatcher env fullname))

(defun dependency-namestring-for-atom (env name)
  (declare (ignore env))
  (enough-namestring (grain-pathname (resolve-absolute-module-name name))))

(define-dependency-namestring :lisp (env name)
  (dependency-namestring-for-atom env name))

(define-dependency-namestring :file (env name)
  (declare (ignore env))
  name)

(defun ensure-makefile-will-make-pathname (env namestring)
  (declare (ignore env))
  (let* ((p (position #\/ namestring :from-end t :end nil))
         (dir (subseq namestring 0 p)))
    (unless (find-if (lambda (d) (portable-namestring-prefix<= dir d)) *makefile-target-directories*)
      (setf *makefile-target-directories*
            (remove-if (lambda (d) (portable-namestring-prefix<= d dir)) *makefile-target-directories*))
      (push dir *makefile-target-directories*)))
  (values))

(defmethod object-namestring ((env makefile-traversal) name &optional merge)
  (let* ((pathname (portable-pathname-from-string name))
         (merged (if merge (merge-pathnames merge pathname) pathname))
         (namestring (strcat *object-directory* (portable-namestring merged))))
    (ensure-makefile-will-make-pathname env namestring)
    namestring))

(define-dependency-namestring :fasl (env name)
  (object-namestring env name +fasl-pathname+))

(define-dependency-namestring :cfasl (env name)
  (object-namestring env name +cfasl-pathname+))

(define-dependency-namestring :image (env name)
  (object-namestring env name +image-pathname+))

(define-dependency-namestring :object (env name)
  (object-namestring env name))

(define-dependency-namestring :source (env name &key in)
  (declare (ignore env))
  (enough-namestring
   (merge-pathnames (portable-pathname-from-string name)
                    (grain-pathname (registered-build in)))))

(define-dependency-namestring :path (env name)
  (declare (ignore env))
  name)

(define-dependency-namestring :manifest (env name)
  (object-namestring env (strcat name "--manifest") +lisp-pathname+))

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
                    (make-pathname :name (strcat (pathname-name path) "--temp")
                                   :defaults path))))
    (rename-target target tempname)))

(define-simple-dispatcher text-for-xcvb-driver-command #'text-for-xcvb-driver-command-atom)

(defun text-for-xcvb-driver-command-atom (env foo)
  (declare (ignore env foo))
  (error "FOO"))

(defun text-for-xcvb-driver-command (env clause)
  (text-for-xcvb-driver-command-dispatcher env clause))

(define-text-for-xcvb-driver-command :load-file (env dep)
  (format nil "(:load-file ~S)" (dependency-namestring env dep)))

(define-text-for-xcvb-driver-command :require (env name)
  (declare (ignore env))
  (format nil "(:cl-require ~(~S~))" name))

(define-text-for-xcvb-driver-command :load-asdf (env name &key parallel)
  (declare (ignore env))
  (format nil "(:load-asdf ~(~S~)~@[ :parallel t~])" name parallel))

(define-text-for-xcvb-driver-command :register-asdf-directory (env directory)
  (declare (ignore env))
  (format nil "(:register-asdf-directory ~S)" (namestring directory)))

(define-text-for-xcvb-driver-command :initialize-manifest (env manifest)
  (format nil "(:initialize-manifest ~S)" (dependency-namestring env manifest)))

(define-text-for-xcvb-driver-command :load-manifest (env name)
  (format nil "(:load-manifest ~S)" (dependency-namestring env name)))

(defun text-for-xcvb-driver-helper (env dependencies format &rest args)
  (with-output-to-string (stream)
    (format stream "(")
    (apply #'format stream format args)
    (dolist (dep dependencies)
      (write-string (text-for-xcvb-driver-command env dep) stream))
    (format stream ")")))

(define-text-for-xcvb-driver-command :compile-lisp (env name-options &rest dependencies)
  (destructuring-bind (name &key) name-options
    (text-for-xcvb-driver-helper
     env dependencies
     ":compile-lisp (~S ~S~@[ :cfasl ~S~])"
     (dependency-namestring env name)
     (tempname-target (dependency-namestring env `(:fasl ,name)))
     (when *use-cfasls* (tempname-target (dependency-namestring env `(:cfasl ,name)))))))

(define-text-for-xcvb-driver-command :create-image (env spec &rest dependencies)
  (destructuring-bind (&key image standalone package) spec
    (text-for-xcvb-driver-helper
     env dependencies
     ":create-image (~S~@[~* :standalone t~]~@[ :package ~S~])"
     (tempname-target (dependency-namestring env `(:image ,image)))
     standalone package)))

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
            (when *renamed-targets*
              (loop :for (target . tempname) :in *renamed-targets*
                    :collect (shell-tokens-to-Makefile (list "mv" tempname target)))))))

(define-Makefile-commands-for-computation :xcvb-driver-command (env keys &rest commands)
  (list
   (destructuring-bind (&key image load) keys
     (shell-tokens-to-Makefile
      (lisp-invocation-arglist
       :image-path (if image (dependency-namestring env image) *lisp-image-pathname*)
       :load (mapcar/ #'dependency-namestring env load)
       :eval (xcvb-driver-commands-to-shell-token env commands))))))

(define-Makefile-commands-for-computation :progn (env &rest commands)
  (loop :for command :in commands
    :append (Makefile-commands-for-computation env command)))

(define-Makefile-commands-for-computation :shell-command (env command)
  (declare (ignore env))
  (list (escape-string-for-Makefile command)))

(define-Makefile-commands-for-computation :exec-command (env &rest argv)
  (declare (ignore env))
  (list (shell-tokens-to-Makefile argv)))

(define-Makefile-commands-for-computation :make-manifest (env manifest &rest commands)
  (list (shell-tokens-to-Makefile
         (cmdize 'xcvb 'make-manifest
                 :output (dependency-namestring env manifest)
                 :spec (with-safe-io-syntax ()
                         (write-to-string (commands-to-manifest-spec env commands)
                                          :case :downcase))))))

(defun compile-file-directly-shell-token (env name &optional cfasl)
  (quit-form
   :code
   (format nil "(multiple-value-bind (output warningp failurep) ~
                  (let ((*default-pathname-defaults* ~
                         (truename *default-pathname-defaults*))) ~
                        (compile-file ~S ~
                         :output-file (merge-pathnames ~S) ~
                         ~@[:emit-cfasl (merge-pathnames ~S) ~]~
                         :verbose nil :print nil)) ~
                    (if (or (not output) warningp failurep) 1 0))"
           (dependency-namestring env name)
           (tempname-target (dependency-namestring env `(:fasl ,name)))
           (when cfasl
             (tempname-target (dependency-namestring env `(:cfasl ,name)))))))

(defun xcvb-driver-commands-to-shell-token (env commands)
  (cond
    ((eq :compile-file-directly (caar commands))
     (apply #'compile-file-directly-shell-token env (cdar commands)))
    (t
     (with-output-to-string (s)
       (write-string "(xcvb-driver:run " s)
       (dolist (c commands)
         (write-string (text-for-xcvb-driver-command env c) s))
       (write-string ")" s)))))

(defgeneric grain-pathname-text (env grain))

(defmethod grain-pathname-text (env (grain file-grain))
  (let ((pathname (dependency-namestring env (fullname grain))))
    (values (escape-shell-token-for-Makefile pathname) pathname)))

(defmethod grain-pathname-text (env (grain asdf-grain))
  (declare (ignore env grain))
  "")

(defmethod grain-pathname-text (env (grain require-grain))
  (declare (ignore env grain))
  "")

(defmethod grain-pathname-text (env (grain phony-grain))
  (declare (ignore env))
  (let ((n (normalize-name-for-makefile (princ-to-string (fullname grain)))))
    (pushnew n *makefile-phonies* :test 'equal)
    n))

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
    (let* ((image-pathname (dependency-namestring env (fullname output)))
           (pathname-text (escape-shell-token-for-Makefile image-pathname))))
    (with-output-to-string (s)
      (format s " $(shell [ -f ~A ] && " pathname-text)
      (shell-tokens-to-Makefile
       (lisp-invocation-arglist
        :image-path image-pathname
        :eval (format nil "(xcvb-driver::asdf-systems-up-to-date~{ ~S~})"
                      (mapcar #'asdf-grain-system-name asdf-grains)))
       s)
      (format s " || echo force)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make-Makefile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +make-makefile-option-spec+
 '((("build" #\b) :type string :optional nil :documentation "specify what system to build")
   (("setup" #\s) :type string :optional t :documentation "specify a Lisp setup file")
   (("xcvb-path" #\x) :type string :optional t :documentation "override your XCVB_PATH")
   (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
   (("object-directory" #\O) :type string :initial-value "obj" :documentation "specify object directory")
   (("lisp-implementation" #\i) :type string :initial-value "sbcl" :documentation "specify type of Lisp implementation")
   (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
   (("disable-cfasl" #\C) :type boolean :optional t :documentation "disable the CFASL feature")
   (("verbosity" #\v) :type integer :initial-value 5 :documentation "set verbosity")
   (("base-image" #\B) :type boolean :optional t :initial-value t :documentation "use a base image")
   (("master" #\m) :type boolean :optional t :initial-value t :documentation "enable XCVB-master")
   (("profiling" #\P) :type boolean :optional t :documentation "profiling")))

(defun make-makefile (&key
                      xcvb-path setup verbosity output-path
                      build lisp-implementation lisp-binary-path
                      disable-cfasl master object-directory base-image profiling)
  (with-maybe-profiling (profiling)
    (reset-variables)
    (when xcvb-path
      (set-search-path! xcvb-path))
    (when verbosity
      (setf *xcvb-verbosity* verbosity))
    (when output-path
      (setf *default-pathname-defaults*
            (ensure-absolute-pathname (pathname-directory-pathname output-path))))
    (when object-directory
      (setf *object-directory* ;; strip last "/"
            (but-last-char (enough-namestring (ensure-pathname-is-directory object-directory)))))
    (when lisp-implementation
      (setf *lisp-implementation-type*
            (find-symbol (string-upcase lisp-implementation) (find-package :keyword))))
    (when lisp-binary-path
      (setf *lisp-executable-pathname* lisp-binary-path))
    (extract-target-properties)
    (read-target-properties)
    (when disable-cfasl
      (setf *use-cfasls* nil))
    (setf *use-base-image* base-image)
    (search-search-path)
    (setf *use-master* master)
    (when master
      (ensure-tthsum-present)
      (append1f *lisp-setup-dependencies* '(:fasl "/xcvb/master")))
    (when setup
      (let ((module (resolve-absolute-module-name setup)))
        (unless module
          (error "Cannot find setup module ~A" setup))
        (append1f *lisp-setup-dependencies* `(:lisp ,(fullname module)))))
    (write-makefile (canonicalize-fullname build) :output-path output-path)))
