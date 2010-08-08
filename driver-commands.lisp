#+xcvb
(module (:depends-on ("utilities" "specials" "grain-interface")))

(in-package :xcvb)

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
  (destructuring-bind (name &key &aux (basename (second name))) name-options
    (text-for-xcvb-driver-helper
     env dependencies
     ":compile-lisp (~S ~S~@[ :cfasl ~S~])"
     (fullname-enough-namestring env name)
     (tempname-target (fullname-enough-namestring env `(:fasl ,basename)))
     (when *use-cfasls*
       (tempname-target (fullname-enough-namestring env `(:cfasl ,basename)))))))

(define-text-for-xcvb-driver-command :create-image (env spec &rest dependencies)
  (destructuring-bind (&key image standalone package) spec
    (text-for-xcvb-driver-helper
     env dependencies
     ":create-image (~S~@[~* :standalone t~]~@[ :package ~S~])"
     (tempname-target (fullname-enough-namestring env `(:image ,image)))
     standalone package)))

(defun lisp-invocation-for (env keys eval)
  (shell-tokens-to-Makefile
   (lisp-invocation-arglist-for env keys eval)))

(defun lisp-invocation-arglist-for (env keys eval)
  (destructuring-bind (&key image load) keys
    (lisp-invocation-arglist
     :image-path (if image (fullname-enough-namestring env image) *lisp-image-pathname*)
     :load (mapcar/ #'fullname-enough-namestring env load)
     :eval (if (or *target-added-features* *target-suppressed-features*)
               (format nil "(progn~
  ~{~#[~;(pushnew ~S *features*)~:;(dolist(x'(~@{~S~^ ~}))(pushnew x *features*))~]~}~
  ~@[(setf *feature*(remove~{~#[~; ~S~:;-if(lambda(x)(member x'(~@{~S~^ ~})))~]~} *features*))~]~
  ~A)"
                       *target-added-features* *target-suppressed-features* eval)
               eval))))

(defun compile-file-directly-shell-token (env name &key cfasl)
  (quit-form
   :code
   (format nil "(multiple-value-bind (output warningp failurep) ~
                    (let ((*default-pathname-defaults* ~
                           (truename *default-pathname-defaults*))) ~
                      (compile-file ~S :verbose nil :print nil ~
                       :output-file (merge-pathnames ~S) ~
                       ~[~;:emit-cfasl (merge-pathnames ~S)~;~
                       :system-p t) (c::build-fasl ~
                         (merge-pathnames ~S) :lisp-files (list ~3:*(merge-pathnames ~S))~]))~
                  (if (or (not output) warningp failurep) 1 0))"
           (fullname-enough-namestring env name)
           (tempname-target (fullname-enough-namestring env `(:fasl ,(second name))))
           (if cfasl
             (ecase *lisp-implementation-type*
               (:sbcl 1)
               (:ecl 2))
             0)
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

