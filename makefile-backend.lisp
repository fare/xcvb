(in-package :xcvb)

(defvar +fasl-pathname+ (make-pathname :type "fasl"))
(defvar +cfasl-pathname+ (make-pathname :type "cfasl"))
(defvar +image-pathname+ (make-pathname :type "image"))

(defvar *makefile-target-directories* ())


(defun write-makefile-prelude (&optional stream)
  (format stream "~
### This file was automatically created by XCVB ~A
### DO NOT EDIT! Changes will be lost when XCVB overwrites this file.~2%"
          *xcvb-version*))

;; TODO: clean
;; * a clean-xcvb target that removes the object directory
(defun write-makefile-conclusion (&optional stream)
  (format stream "
ensure-xcvb-object-directories:
	mkdir -p ~A

.PHONY: force ensure-xcvb-object-directories~2%"
          (shell-tokens-to-Makefile *makefile-target-directories*)))

(defun write-makefile (fullname
                       &key
                       output-path
                       (makefile-name "xcvb.mk"))
  "Write a Makefile to output-path with information about how to compile the specified BUILD."
  (let* ((build (registered-grain fullname))
         (output-path (or output-path (grain-pathname build)))
         (makefile-path (ensure-absolute-pathname (merge-pathnames makefile-name output-path)))
         (makefile-dir (pathname-directory-pathname makefile-path))
         (*default-pathname-defaults* makefile-dir)
         (*makefile-target-directories* nil))
    (graph-for-build (make-instance 'static-traversal) build)
    (with-open-file (out makefile-path
                         :direction :output
                         :if-exists :supersede)
      (write-makefile-prelude out)
      (dolist (computation *computations*)
        (write-computation-to-makefile out computation))
      (write-makefile-conclusion out))))


;;; dependency-pathname: extract Makefile-printable pathname from dependency spec

(define-simple-dispatcher dependency-pathname #'dependency-pathname-for-atom)

(defun dependency-pathname (clause)
  (dependency-pathname-dispatcher nil clause))

(defun dependency-pathname-for-atom (env name)
  (declare (ignore env))
  (enough-namestring (grain-pathname (registered-grain name))))

(define-dependency-pathname :lisp (env name)
  (dependency-pathname-for-atom env name))


(defun ensure-makefile-will-make-pathname (env namestring)
  (declare (ignore env))
  (unless (member namestring *makefile-target-directories* :test 'equal)
    (labels ((f (end)
               (let ((p (position #\/ namestring :from-end t :end end)))
                 (when p
                   (let ((dir (subseq namestring 0 p)))
                     (if end
                       (setf *makefile-target-directories*
                             (remove dir *makefile-target-directories* :test 'equal))
                       (pushnew dir *makefile-target-directories* :test 'equal)))
                   (f p)))))
      (f nil)))
  (values))

(defun object-namestring (env name merge)
  (let ((pathname
         (strcat "obj"
                 (portablish-namestring (merge-pathnames merge name)))))
    (ensure-makefile-will-make-pathname env pathname)
    pathname))

(define-dependency-pathname :fasl (env name)
  (object-namestring env name +fasl-pathname+))

(define-dependency-pathname :cfasl (env name)
  (object-namestring env name +cfasl-pathname+))

(define-dependency-pathname :image (env name)
  (object-namestring env name +image-pathname+))

(define-simple-dispatcher text-for-lisp-command #'text-for-lisp-command-atom)

(defun text-for-lisp-command-atom (str foo)
  (declare (ignore str foo))
  (error "FOO"))

(defun text-for-lisp-command (str clause)
  (text-for-lisp-command-dispatcher str clause))

(define-text-for-lisp-command :load-file (str dep)
  (format str "(:load-file ~S)" (dependency-pathname dep))
  (values))

(define-text-for-lisp-command :load-asdf (str name)
  (format str "(:load-asdf ~S)" name)
  (values))

(define-text-for-lisp-command :compile-lisp (str name)
  (format str "(:compile ~S ~S~@[ :cfasl ~S~])"
          (dependency-pathname name)
          (dependency-pathname `(:fasl ,name))
          (when *use-cfasls*
            (dependency-pathname `(:cfasl ,name))))
  (values))

(define-text-for-lisp-command :create-image (str spec &rest dependencies)
  (destructuring-bind (&key image standalone package) spec
    (format str "(:create-image ~S "
            (append (list (dependency-pathname `(:image ,image)))
                    (when standalone '(:standalone t))
                    (when package `(:package ,package))))
    (dolist (dep dependencies)
      (text-for-lisp-command str dep))
    (format str ")"))
  (values))

(defun Makefile-command-for-computation (str computation-command)
  (destructuring-bind (lisp (&key image) &rest commands) computation-command
    (unless (eq lisp :lisp)
      (error ":lisp required"))
    (shell-tokens-to-Makefile
     (lisp-invocation-arglist
      :image-path (when image (dependency-pathname image))
      :eval (lisp-commands-to-shell-token commands))
     str)))

(defun lisp-commands-to-shell-token (commands)
  (with-output-to-string (s)
    (format s "(xcvb-driver:run ")
    (dolist (c commands)
      (text-for-lisp-command s c))
    (format s ")")))

(defmethod grain-pathname-text ((grain file-grain))
  (let ((pathname (dependency-pathname (fullname grain))))
    (values (escape-shell-token-for-Makefile pathname) pathname)))

(defmethod grain-pathname-text ((grain asdf-grain))
  nil)

(defun write-computation-to-makefile (stream computation)
  (with-accessors ((command computation-command)
                   (inputs computation-inputs)
                   (outputs computation-outputs)) computation
    (let* ((first-output (first outputs))
           (target (grain-pathname-text first-output))
           (other-outputs (rest outputs)))
      (dolist (o other-outputs)
        (format stream "~&~A: ~A~%" (grain-pathname-text o) target))
      (format stream "~&~A:~{~@[ ~A~]~}~@[~A~] ensure-xcvb-object-directories~%"
              target
              (mapcar #'grain-pathname-text inputs)
              (asdf-dependency-text first-output inputs))
      (format stream "~C~A~2%"
              #\Tab (Makefile-command-for-computation nil command)))))

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
    (let* ((image-pathname (dependency-pathname (fullname output)))
           (pathname-text (escape-shell-token-for-Makefile image-pathname))))
    (with-output-to-string (s)
      (format s " $(shell { [ -f ~A ] && " pathname-text)
      (shell-tokens-to-Makefile
       (lisp-invocation-arglist
        :image-path image-pathname
        :eval "(xcvb-driver::asdf-systems-up-to-date-p)")
       s)
      (format s " } || echo force)"))))

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
