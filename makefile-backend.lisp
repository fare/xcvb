#+xcvb
(module
  (:author ("Francois-Rene Rideau" "Stas Boukarev")
   :maintainer "Francois-Rene Rideau"
   :depends-on ("static-backends" "string-escape" "computations")))

(in-package :xcvb)

(defvar +fasl-pathname+ (make-pathname :type "fasl"))
(defvar +cfasl-pathname+ (make-pathname :type "cfasl"))
(defvar +image-pathname+ (make-pathname :type "image"))

(defvar *makefile-target-directories* ())
(defvar *makefile-phonies* ())


(defun write-makefile (fullname &key output-path)
  "Write a Makefile to output-path with information about how to compile the specified BUILD."
  (let* ((build (registered-build fullname))
         (default-output-path (merge-pathnames "xcvb.mk" (grain-pathname build)))
         (output-path (if output-path (merge-pathnames output-path default-output-path) default-output-path))
         (makefile-path (ensure-absolute-pathname output-path))
         (makefile-dir (pathname-directory-pathname makefile-path))
         (*default-pathname-defaults* makefile-dir)
         (*makefile-target-directories* nil)
         (*makefile-phonies* nil))
    (graph-for-build-grain (make-instance 'static-traversal) build)
    (let ((body
           (with-output-to-string (s)
             (dolist (computation *computations*)
               (write-computation-to-makefile s computation)))))
      (with-open-file (out makefile-path
                           :direction :output
                           :if-exists :supersede)
        (write-makefile-prelude out)
        (princ body out)
        (write-makefile-conclusion out)))))


(defun write-makefile-prelude (&optional stream)
  (let ((directories (join-strings " " (mapcar #'escape-string-for-Makefile *makefile-target-directories*))))
    (format stream "~
### This file was automatically created by XCVB ~A
### DO NOT EDIT! Changes will be lost when XCVB overwrites this file.

XCVB_EOD :=
ifneq ($(wildcard ~A),~A)
  XCVB_EOD := xcvb-ensure-object-directories
endif~2%"
            *xcvb-version* directories directories)
    (flet ((export-directory (x)
             (format stream "~%~A ?= ~A~%export ~A~%~%"
                     x *lisp-implementation-directory* x)))
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

(defun dependency-namestring (fullname)
  ;; TODO: double check that a namestring is only used by one fullname,
  ;; using a table to record namestring => fullname mappings
  ;; maybe also have a table the other way to cache these computations?
  (dependency-namestring-dispatcher nil fullname))

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

(defun object-namestring (env name &optional merge)
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


;; Renaming of targets ensures reasonable atomicity whereas CL implementations
;; may create bad invalid stale output files when interrupted in the middle
;; (whether the process is killed in the midst of an unsuccessful debug attempt,
;; or the plug is simply pulled). This isn't done in the target Lisp side,
;; because CL implementations don't usually do that for you implicitly, and
;; while we could do it explicitly for :compile-lisp doing it for :create-image
;; would be a pain in SBCL where we would have to fork and wait for a subprocess
;; to SAVE-LISP-AND-DIE which would make the target much more complex than desired.

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
                    (make-pathname :name (strcat (pathname-name path) "-temp")
                                   :defaults path))))
    (rename-target target tempname)))

(define-simple-dispatcher text-for-xcvb-driver-command #'text-for-xcvb-driver-command-atom)

(defun text-for-xcvb-driver-command-atom (str foo)
  (declare (ignore str foo))
  (error "FOO"))

(defun text-for-xcvb-driver-command (str clause)
  (text-for-xcvb-driver-command-dispatcher str clause))

(define-text-for-xcvb-driver-command :load-file (str dep)
  (format str "(:load-file ~S)" (dependency-namestring dep))
  (values))

(define-text-for-xcvb-driver-command :load-asdf (str name)
  (format str "(:load-asdf ~S)" name)
  (values))

(defun text-for-xcvb-driver-helper (stream dependencies format &rest args)
  (format stream "(")
  (apply #'format stream format args)
  (dolist (dep dependencies)
    (text-for-xcvb-driver-command stream dep))
  (format stream ")")
  (values))

(define-text-for-xcvb-driver-command :compile-lisp (str name-options &rest dependencies)
  (destructuring-bind (name &key) name-options
    (text-for-xcvb-driver-helper
     str dependencies
     ":compile-lisp (~S ~S~@[ :cfasl ~S~])"
     (dependency-namestring name)
     (tempname-target (dependency-namestring `(:fasl ,name)))
     (when *use-cfasls* (tempname-target (dependency-namestring `(:cfasl ,name)))))))

(define-text-for-xcvb-driver-command :create-image (str spec &rest dependencies)
  (destructuring-bind (&key image standalone package) spec
    (text-for-xcvb-driver-helper
     str dependencies
     ":create-image (~S~@[~* :standalone t~]~@[ :package ~S~])"
     (tempname-target (dependency-namestring `(:image ,image)))
     standalone package)))

(define-simple-dispatcher Makefile-commands-for-computation #'Makefile-commands-for-computation-atom)

(defun Makefile-commands-for-computation-atom (str computation-command)
  (declare (ignore str))
  (error "Invalid computation ~S" computation-command))

(defun Makefile-commands-for-computation (str computation-command)
  ;; We rename secondary targets first, according to the theory that
  ;; in case of interruption, the primary target will be re-built which will
  ;; cause the secondary targets to be implicitly re-built before success.
  (let* ((*renamed-targets* nil)
         (commands (Makefile-commands-for-computation-dispatcher str computation-command)))
    (append commands
            (when *renamed-targets*
              (loop :for (target . tempname) :in *renamed-targets*
                    :collect (shell-tokens-to-Makefile (list "mv" tempname target)))))))

(define-Makefile-commands-for-computation :xcvb-driver-command (str keys &rest commands)
  (list
   (destructuring-bind (&key image load) keys
     (shell-tokens-to-Makefile
      (lisp-invocation-arglist
       :image-path (if image (dependency-namestring image) *lisp-image-pathname*)
       :load (mapcar #'dependency-namestring load)
       :eval (xcvb-driver-commands-to-shell-token commands))
      str))))

(define-Makefile-commands-for-computation :shell-command (str command)
  (list (escape-string-for-Makefile command str)))

(define-Makefile-commands-for-computation :exec-command (str &rest argv)
  (list (shell-tokens-to-Makefile argv str)))

(defun xcvb-driver-commands-to-shell-token (commands)
  (with-output-to-string (s)
    (format s "(xcvb-driver:run ")
    (dolist (c commands)
      (text-for-xcvb-driver-command s c))
    (format s ")")))

(defgeneric grain-pathname-text (grain))

(defmethod grain-pathname-text ((grain file-grain))
  (let ((pathname (dependency-namestring (fullname grain))))
    (values (escape-shell-token-for-Makefile pathname) pathname)))

(defmethod grain-pathname-text ((grain asdf-grain))
  nil)

(defmethod grain-pathname-text ((grain phony-grain))
  (let ((n (normalize-name-for-makefile (princ-to-string (fullname grain)))))
    (pushnew n *makefile-phonies* :test 'equal)
    n))

(defun write-computation-to-makefile (stream computation)
  (with-accessors ((command computation-command)
                   (inputs computation-inputs)
                   (outputs computation-outputs)) computation
    (let* ((first-output (first outputs))
           (target (grain-pathname-text first-output))
           (other-outputs (rest outputs)))
      (dolist (o other-outputs)
        (format stream "~&~A: ~A~%" (grain-pathname-text o) target))
      (format stream "~&~A:~{~@[ ~A~]~}~@[~A~] ${XCVB_EOD}~%"
              target
              (mapcar #'grain-pathname-text inputs)
              (asdf-dependency-text first-output inputs))
      (when command
        (dolist (c (Makefile-commands-for-computation nil command))
          (format stream "~C~A~%" #\Tab c)))
      (terpri stream))))

;;; This is only done for images, not for individual files.
;;; For finer granularity, we could possibly define for each ASDF system
;;; (and implementation) a variable
;;; ASDF_CL_PPCRE_UP_TO_DATE := $(shell ...)
;;; but that would require more work.
;;; Also, it doesn't make sense to try to beat ASDF at its own game:
;;; if you really want proper dependencies,
;;; you'll migrate from ASDF to XCVB anyway.
(defun asdf-dependency-text (output inputs)
  (with-nesting ()
    (when (image-grain-p output))
    (let ((asdf-grains (remove-if-not #'asdf-grain-p inputs))))
    (when asdf-grains)
    (let* ((image-pathname (dependency-namestring (fullname output)))
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

#|
;; This should be generalized and moved to some generic file
(defun group-grains-by-implementation (asdf-grains)
    (loop :while asdf-grains :collect
      (loop
        :with implementation = (asdf-grain-implementation (first asdf-grains))
        :for grain :in asdf-grains
        :when (equal implementation (asdf-grain-implementation grain))
        :collect grain :into in
        :else :collect grain :into out
        :finally (progn
                   (setf asdf-grains out)
                   (return in)))))
|#
