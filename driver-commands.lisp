#+xcvb
(module (:depends-on ("utilities" "specials" "grain-interface")))

(in-package :xcvb)

(define-simple-dispatcher text-for-xcvb-driver-command #'text-for-xcvb-driver-command-atom)

(defun text-for-xcvb-driver-command-atom (env foo)
  (declare (ignore env foo))
  (error "FOO"))

(defun text-for-xcvb-driver-command (env clause)
  (text-for-xcvb-driver-command-dispatcher env clause))

(define-text-for-xcvb-driver-command :load-file (env dep)
  (format nil "(:load-file ~S)" (fullname-enough-namestring env dep)))

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
  (format nil "(:initialize-manifest ~S)"
          (pseudo-fullname-enough-namestring env manifest)))

(define-text-for-xcvb-driver-command :load-manifest (env manifest)
  (format nil "(:load-manifest ~S)"
          (pseudo-fullname-enough-namestring env manifest)))


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
     (fullname-enough-namestring env name)
     (tempname-target (fullname-enough-namestring env `(:fasl ,(second name))))
     (when *use-cfasls*
       (tempname-target (fullname-enough-namestring env `(:cfasl ,(second name))))))))

(define-text-for-xcvb-driver-command :create-image (env spec &rest dependencies)
  (destructuring-bind (&key image standalone package) spec
    (text-for-xcvb-driver-helper
     env dependencies
     ":create-image (~S~@[~* :standalone t~]~@[ :package ~S~])"
     (tempname-target (fullname-enough-namestring env `(:image ,image)))
     standalone package)))

(defun lisp-invocation-for (env keys eval)
  (destructuring-bind (&key image load) keys
    (shell-tokens-to-Makefile
     (lisp-invocation-arglist
      :image-path (if image (fullname-enough-namestring env image) *lisp-image-pathname*)
      :load (mapcar/ #'fullname-enough-namestring env load)
      :eval eval))))

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
           (fullname-enough-namestring env name)
           (tempname-target (fullname-enough-namestring env `(:fasl ,(second name))))
           (when cfasl
             (tempname-target (fullname-enough-namestring env `(:cfasl ,(second name))))))))

(defun xcvb-driver-commands-to-shell-token (env commands)
  (with-output-to-string (s)
    (write-string "(xcvb-driver:run " s)
    (dolist (c commands)
      (write-string (text-for-xcvb-driver-command env c) s))
    (write-string ")" s)))

(defgeneric grain-pathname-text (env grain))

(defmethod grain-pathname-text (env (grain file-grain))
  (let ((pathname (enough-namestring (vp-namestring env (grain-vp grain)))))
    (values (escape-shell-token-for-Makefile pathname) pathname)))

(defmethod grain-pathname-text (env (grain asdf-grain))
  (declare (ignore env grain))
  "")

(defmethod grain-pathname-text (env (grain require-grain))
  (declare (ignore env grain))
  "")

