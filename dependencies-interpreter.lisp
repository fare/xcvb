#+xcvb (module (:depends-on ("normalize-dependency" "traversal")))

(in-package :xcvb)

;;; LOAD-COMMAND-FOR

(define-simple-dispatcher load-command-for #'load-command-for-atom :generic)

(defun load-command-for (env spec)
  (load-command-for-dispatcher env spec))

(defun load-command-for* (env specs)
  (dolist (spec specs)
    (load-command-for env spec)))

(defun load-command-for-atom (env spec)
  (declare (ignore env))
  (error "Invalid dependency ~S" spec))

(define-load-command-for :lisp (env name)
  (simple-load-command-for
   env `(:load-file ,name) name))
(define-load-command-for :fasl (env name)
  (simple-load-command-for
   env `(:load-file (:fasl ,name)) `(:fasl ,name)))
(define-load-command-for :cfasl (env name)
  (simple-load-command-for
   env `(:load-file (:cfasl ,name)) `(:cfasl ,name)))
(define-load-command-for :asdf (env name)
  (simple-load-command-for env `(:load-asdf ,name) `(:asdf ,name)))
(define-load-command-for :require (env name)
  (simple-load-command-for env `(:require ,name) `(:require ,name)))

(defun simple-load-command-for (env command fullname)
  (call-with-dependency-grain
   env fullname
   (lambda (grain)
     (with-dependency-loading (env grain)
       (load-commands-for-build-dependencies env grain)
       (load-commands-for-load-dependencies env grain)
       (issue-load-command env command)))))

(define-load-command-for :source (env name &key in)
  ;; Suffices to know data file exists.  No need to issue load command.
  (call-with-dependency-grain
   env `(:source ,name :in ,in)
   (lambda (grain)
     (with-dependency-loading (env grain)
       (values)))))

(defun call-with-dependency-grain (environment dep fun)
  (let* ((grain (graph-for environment dep)))
    (with-dependency (:type type) dep
      (unless (typep grain type)
        (error "Expected a grain of type ~S for ~S, instead got ~S"
               type dep grain))
      (funcall fun grain))))

(define-load-command-for :build (env name)
  (let ((build (registered-build name)))
    (handle-lisp-dependencies build)
    (with-dependency-loading (env build)
      (load-commands-for-build-dependencies env build)
      (load-commands-for-load-dependencies env build))))

(define-load-command-for :compile-build (env name)
  (let ((build (registered-build name)))
    (handle-lisp-dependencies build)
    (with-dependency-loading (env build)
      (load-commands-for-build-dependencies env build)
      (load-commands-for-compile-dependencies env build))))

(defun load-commands-for-load-dependencies (env grain)
  (load-command-for* env (load-dependencies grain)))

(defun load-commands-for-compile-dependencies (env grain)
  (load-command-for* env (compile-dependencies grain)))

(defun load-commands-for-cload-dependencies (env grain)
  (load-command-for* env (cload-dependencies grain)))

(defun load-commands-for-build-dependencies (env grain)
  (load-command-for* env (build-dependencies grain)))

(define-load-command-for :when (env expression &rest dependencies)
  (when (evaluate-condition env expression)
    (load-command-for* env dependencies)))

(define-load-command-for :cond (env &rest cond-expressions)
  (loop :for cond-expression :in cond-expressions
        :when (evaluate-condition env (car cond-expression))
        :do (return (load-command-for* env (cdr cond-expression)))))

(define-simple-dispatcher evaluate-condition #'evaluate-condition-atom)

(defun evaluate-condition (env expression)
  (evaluate-condition-dispatcher env expression))

(defun evaluate-condition-atom (env atom)
  (declare (ignore env))
  (if (typep atom 'boolean)
    atom
    (error "Invalid condition ~S" atom)))

(define-evaluate-condition :featurep (env feature-expression)
  (evaluate-featurep env feature-expression))

(defun evaluate-featurep (env feature-expression)
  (evaluate-featurep-dispatcher env feature-expression))

(define-simple-dispatcher evaluate-featurep #'evaluate-featurep-atom)

(defun evaluate-featurep-atom (env atom)
  (declare (ignore env))
  (unless (keywordp atom)
    (error "Invalid feature ~S" atom))
  (member atom (target-system-features)))

(define-evaluate-featurep :and (env &rest feature-expressions)
  (loop :for feature-expression :in feature-expressions
        :always (evaluate-featurep env feature-expression)))

(define-evaluate-featurep :or (env &rest feature-expressions)
  (loop :for feature-expression :in feature-expressions
        :thereis (evaluate-featurep env feature-expression)))

(define-evaluate-featurep :not (env feature-expression)
  (not (evaluate-featurep env feature-expression)))
