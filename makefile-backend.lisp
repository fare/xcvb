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
  (let ((build (registered-grain fullname)))
    (unless output-path
      (setf output-path (grain-pathname build)))
    (graph-for-build (make-instance 'static-traversal) build)
    (with-open-file (out (merge-pathnames makefile-name output-path)
                         :direction :output
                         :if-exists :supersede)
      (write-makefile-prelude out)
      (dolist (computation *computations*)
        (write-computation-to-makefile out computation)))))

(define-simple-dispatcher dependency-pathname #'dependency-pathname-for-atom)

(defun dependency-pathname (clause)
  (dependency-pathname-dispatcher nil clause))

(defun dependency-pathname-for-atom (env name)
  (declare (ignore env))
  (grain-pathname (registered-grain name)))

(define-dependency-pathname :lisp (env name)
  (dependency-pathname-for-atom env name))

(defvar +fasl-pathname+ (make-pathname :type "fasl"))
(defvar +cfasl-pathname+ (make-pathname :type "cfasl"))

(define-dependency-pathname :fasl (env name)
  (merge-pathnames +fasl-pathname+ (dependency-pathname-for-atom env name)))

(define-dependency-pathname :cfasl (env name)
  (merge-pathnames +cfasl-pathname+ (dependency-pathname-for-atom env name)))

(define-dependency-pathname :image (env name)
  (declare (ignore env))
  (namestring
   (merge-pathnames (make-pathname :type "image")
                    (pathname-name name))))

(define-simple-dispatcher text-for-lisp-command #'text-for-lisp-command-atom)

(defun text-for-lisp-command-atom (str foo)
  (declare (ignore str foo))
  (error "FOO"))

(defun text-for-lisp-command (str clause)
  (text-for-lisp-command-dispatcher str clause))

(define-text-for-lisp-command :load (str name)
  (format str "(load \"~a\")" (dependency-pathname name)))

(define-text-for-lisp-command :compile-lisp (str name)
  (format str "(compile-file \"~a\" :fasl \"~a\"~@[ :cfasl \"~a\"~])"
          (dependency-pathname name)
          (dependency-pathname `(:fasl ,name))
          (when *use-cfasls*
            (dependency-pathname `(:cfasl ,name)))))

(define-text-for-lisp-command :save-image (str name)
  (format str "(xcvb-driver:dump-image ~S)" (dependency-pathname name)))

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
    (format s "(xcvb-driver:with-exit-on-error () ")
    (dolist (c commands)
      (text-for-lisp-command s c))
    (format s ")")))

(defun grain-pathname-text (grain)
  (dependency-pathname (fullname grain)))

(defun write-computation-to-makefile (stream computation)
  (with-accessors ((command computation-command)
                   (inputs computation-inputs)
                   (outputs computation-outputs)) computation
    (let ((first-output (grain-pathname-text (first outputs)))
          (other-outputs (rest outputs)))
      (dolist (o other-outputs)
        (format stream "~A: ~A" (grain-pathname-text o) first-output))
    (format stream "~A: ~{~a~^ ~}~%" first-output (mapcar 'grain-pathname-text inputs))
    (format stream "~c~a~2%" #\Tab (Makefile-command-for-computation nil command)))))
