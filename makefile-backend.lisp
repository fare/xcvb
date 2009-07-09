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
    (case *lisp-implementation-type*
      (:sbcl
       (format stream "~%export SBCL_HOME=~A~%~%" *lisp-implementation-directory*))
      (:ccl
       (format stream "~%export CCL_DIRECTORY=~A~%~%" *lisp-implementation-directory*)))))


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

(define-text-for-xcvb-driver-command :compile-lisp (str name)
  (format str "(:compile-lisp ~S ~S~@[ :cfasl ~S~])"
          (dependency-namestring name)
          (dependency-namestring `(:fasl ,name))
          (when *use-cfasls*
            (dependency-namestring `(:cfasl ,name))))
  (values))

(define-text-for-xcvb-driver-command :create-image (str spec &rest dependencies)
  (destructuring-bind (&key image standalone package) spec
    (format str "(:create-image ~S"
            (append (list (dependency-namestring `(:image ,image)))
                    (when standalone '(:standalone t))
                    (when package `(:package ,package))))
    (dolist (dep dependencies)
      (text-for-xcvb-driver-command str dep))
    (format str ")"))
  (values))

(define-simple-dispatcher Makefile-command-for-computation #'Makefile-command-for-computation-atom)

(defun Makefile-command-for-computation-atom (str computation-command)
  (declare (ignore str))
  (error "Invalid command ~S" computation-command))

(defun Makefile-command-for-computation (str computation-command)
  (Makefile-command-for-computation-dispatcher str computation-command))

(define-Makefile-command-for-computation :xcvb-driver-command (str keys &rest commands)
  (destructuring-bind (&key image load) keys
    (shell-tokens-to-Makefile
     (lisp-invocation-arglist
      :image-path (if image (dependency-namestring image) *lisp-image-pathname*)
      :load (mapcar #'dependency-namestring load)
      :eval (xcvb-driver-commands-to-shell-token commands))
     str)))

(define-Makefile-command-for-computation :shell-command (str command)
  (escape-string-for-Makefile command str))

(define-Makefile-command-for-computation :exec-command (str &rest argv)
  (shell-tokens-to-Makefile argv str))

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
        (format stream "~C~A~%"
              #\Tab (Makefile-command-for-computation nil command)))
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
