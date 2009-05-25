(in-package :xcvb)
;; that's what i have so far

(defun write-makefile-prelude (&optional stream)
  (format stream "~
### This file was automatically created by XCVB ~A
### DO NOT EDIT! Changes will be lost when xcvb overwrites this file.

"
          *xcvb-version*))

(defun write-makefile (fullname
                       &key
                       output-path
                       (makefile-name "xcvb.mk"))
  "Write a Makefile to output-path with information about how to compile the specified BUILD."
  (let* ((build (registered-grain fullname))
         (output-path (or output-path (grain-pathname build)))
         (makefile-path (ensure-absolute-pathname (merge-pathnames makefile-name output-path)))
         (makefile-dir (pathname-directory-pathname makefile-path))
         (*default-pathname-defaults* makefile-dir))
    (graph-for-build (make-instance 'static-traversal) build)
    (with-open-file (out makefile-path
                         :direction :output
                         :if-exists :supersede)
      (write-makefile-prelude out)
      (dolist (computation *computations*)
        (write-computation-to-makefile out computation)))))


;;; dependency-pathname: extract Makefile-printable pathname from dependency spec

(define-simple-dispatcher dependency-pathname #'dependency-pathname-for-atom)

(defun dependency-pathname (clause)
  (dependency-pathname-dispatcher nil clause))

(defun dependency-pathname-for-atom (env name)
  (declare (ignore env))
  (enough-namestring (grain-pathname (registered-grain name))))

(define-dependency-pathname :lisp (env name)
  (dependency-pathname-for-atom env name))

(defvar +fasl-pathname+ (make-pathname :type "fasl"))
(defvar +cfasl-pathname+ (make-pathname :type "cfasl"))
(defvar +image-pathname+ (make-pathname :type "image"))

(defun object-namestring (env name merge)
  (declare (ignore env))
  (strcat "obj"
          (portablish-namestring (merge-pathnames merge name))))

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
  (dependency-pathname (fullname grain)))

(defmethod grain-pathname-text ((grain asdf-grain))
  "") ;;; TODO: have a $(shell ... || echo force) for all asdf systems at once

(defun write-computation-to-makefile (stream computation)
  (with-accessors ((command computation-command)
                   (inputs computation-inputs)
                   (outputs computation-outputs)) computation
    (let ((first-output (grain-pathname-text (first outputs)))
          (other-outputs (rest outputs)))
      (dolist (o other-outputs)
        (format stream "~&~A: ~A~%" (grain-pathname-text o) first-output))
      (format stream "~&~A: ~{~A~^ ~}~@[~A~]~%"
              first-output
              (mapcar #'grain-pathname-text inputs)
              (asdf-dependency-text inputs))
      (format stream "~C~A~2%"
              #\Tab (Makefile-command-for-computation nil command)))))

;;; TODO: should only be done for images,
;;; testing the existence of the image, then rebuilding the image if needed.
;;; It doesn't make sense to try to beat ASDF at its own game:
;;; if you really want proper dependencies, you'll migrate from ASDF to XCVB
;;; anyway.
(defun asdf-dependency-text (inputs)
  (let ((asdf-grains (remove-if-not #'asdf-grain-p inputs)))
    (when asdf-grains
      (with-output-to-string (s)
        (dolist (grain-group (group-grains-by-implementation asdf-grains))
          (format s " $(shell xcvb asdf-up-to-date-p ~
			--implementation ~(~A~) --~{ ~A~} || echo force)"
                  (asdf-grain-implementation (first grain-group))
                  (mapcar #'asdf-grain-system-name grain-group)))))))

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
