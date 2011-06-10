#+xcvb (module (:depends-on ("normalize-dependency" "traversal")))

(in-package :xcvb)

;;; BUILD-COMMAND-FOR
(defgeneric build-command-for (env name))
(defgeneric build-command-for-lisp (env name))
(defgeneric build-command-for-fasl (env name))
(defgeneric build-command-for-cfasl (env name))
(defgeneric build-command-for-lisp-object (env name)) ;; XXX or should have link-commands instead ?
(defgeneric build-command-for-asdf (env name))
(defgeneric build-command-for-require (env name))
(defgeneric build-command-for-static-library (env name))
(defgeneric build-command-for-dynamic-library (env name))
(defgeneric build-command-for-source (env name &key in))
(defgeneric build-command-for-build (env name))
(defgeneric build-command-for-compile-build (env name))
(defgeneric build-command-for-build-named (env name))
(defgeneric build-command-for-when (env expression &rest dependencies))
(defgeneric build-command-for-cond (env &rest cond-expressions))
(defgeneric simple-build-command-for-grain (env command grain))

(define-simple-dispatcher build-command-for #'build-command-for-atom :generic t)

(defmethod build-command-for (env spec)
  (build-command-for-dispatcher env (tweak-dependency env spec)))

(defun build-command-for* (env specs)
  (dolist (spec specs)
    (build-command-for env spec)))

(defun build-command-for-atom (env spec)
  (declare (ignore env))
  (error "Cannot produce a build command for the invalid dependency ~S" spec))

(define-build-command-for :lisp (env name)
  (simple-build-command-for
   env `(:load-file ,name) `(:lisp ,name)))
(define-build-command-for :fasl (env name)
  (simple-build-command-for
   env `(:load-file (:fasl ,name)) `(:fasl ,name)))
(define-build-command-for :cfasl (env name)
  (simple-build-command-for
   env `(:load-file (:cfasl ,name)) `(:cfasl ,name)))
(define-build-command-for :lisp-object (env name)
  (simple-build-command-for
   env `(:load-file (:lisp-object ,name)) `(:lisp-object ,name)))
(define-build-command-for :dynamic-library (env name)
  (simple-build-command-for
   env `(:load-file `(:dynamic-library ,name)) `(:dynamic-library ,name)))
(define-build-command-for :static-library (env name)
  (simple-build-command-for
   env `(:load-file (:static-library ,name)) `(:static-library ,name)))
(define-build-command-for :asdf (env name)
  (build-command-for env `(:build "/asdf"))
  (simple-build-command-for env `(:load-asdf ,name) `(:asdf ,name)))
(define-build-command-for :require (env name)
  (simple-build-command-for env `(:require ,name) `(:require ,name)))

(defun simple-build-command-for (env command fullname)
  (call-with-dependency-grain
   env fullname
   (lambda (grain) (simple-build-command-for-grain env command grain))))

(defmethod simple-build-command-for-grain (env command grain)
  (with-dependency-loading (env grain)
    (build-commands-for-build-dependencies env grain)
    (build-commands-for-load-dependencies env grain)
    (issue-build-command env command)))

(define-build-command-for :source (env name &key in)
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

(define-build-command-for :build (env name)
  (let ((build (registered-build name :ensure-build t)))
    (finalize-grain build)
    (if (target-ecl-p)
	(build-command-for env `(:dynamic-library ,name))
	(with-dependency-loading (env build)
	  (build-commands-for-load-dependencies env build)))))

(define-build-command-for :compile-build (env name)
  (let ((build (registered-build name :ensure-build t)))
    (finalize-grain build)
    (with-dependency-loading (env build)
      (build-commands-for-build-dependencies env build)
      (build-commands-for-compile-dependencies env build))))

(defun build-commands-for-load-dependencies (env grain)
  (build-command-for* env (load-dependencies grain)))

(defun build-commands-for-compile-dependencies (env grain)
  (build-command-for* env (compile-dependencies grain)))

(defun build-commands-for-cload-dependencies (env grain)
  (build-command-for* env (cload-dependencies grain)))

(defun build-commands-for-build-dependencies (env grain)
  (build-command-for* env (build-dependencies grain)))

(define-build-command-for :when (env expression &rest dependencies)
  (when (evaluate-condition env expression)
    (build-command-for* env dependencies)))

(define-build-command-for :cond (env &rest cond-expressions)
  (loop :for cond-expression :in cond-expressions
        :when (evaluate-condition env (car cond-expression))
        :do (return (build-command-for* env (cdr cond-expression)))))

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
  (target-feature-p atom))

(define-evaluate-featurep :and (env &rest feature-expressions)
  (loop :for feature-expression :in feature-expressions
        :always (evaluate-featurep env feature-expression)))

(define-evaluate-featurep :or (env &rest feature-expressions)
  (loop :for feature-expression :in feature-expressions
        :thereis (evaluate-featurep env feature-expression)))

(define-evaluate-featurep :not (env feature-expression)
  (not (evaluate-featurep env feature-expression)))
