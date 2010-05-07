#+xcvb
(module
  (:depends-on ("makefile-backend" "static-traversal" "computations" "driver-commands"
                                   "grain-implementation" "asdf-backend")))

(in-package :xcvb)

(defclass cffi-grovel-specification-grain (lisp-module-grain)
  ((fullname :initarg :fullname)
   (cc-flags :initarg cc-flags :initform nil :reader cc-flags-of)))

(defclass object-grain (file-grain)
  ((parent :initarg :parent)
   (fullname :initarg :fullname)))

(defclass cffi-grovel-processed-grain (lisp-file-grain)
  ())

(define-handle-extension-form :cffi-grovel (build name &rest keys &key
                                            cc-flags depends-on build-depends-on
                                            compile-depends-on cload-depends-on load-depends-on)
  (let* ((bname (fullname build))
         (prefix (strcat bname "/"))
         (suffix (cond
                   ((eql #\/ (first-char name))
                    (or (string-strip-prefix prefix name)
                        (error ":cffi-grovel module name ~A not under build name ~A"
                               name bname)))
                   (t
                    name)))
         (fname (strcat prefix suffix))
         (grain
          (apply #'make-instance 'cffi-grovel-specification-grain
                 :fullname `(:cffi-grovel ,fname)
                 :parent build
                 :vp (make-vp :src bname "/" suffix "." "lisp")
                 keys)))
    (setf (registered-grain `(:cffi-grovel ,fname)) grain)
    (with-slots (fullname
                 parent cc-flags
                 build-depends-on compile-depends-on
                 cload-depends-on load-depends-on) grain
      (let* ((lisp
              (apply 'make-instance
               'cffi-grovel-processed-grain
               :fullname `(:lisp ,fname)
               :vp (make-vp :obj bname "/" suffix "." "grovel-tmp" "." "lisp")
               :parent build
               :computation nil
               :extension-forms nil
               :depends-on (append depends-on '((:build "/cffi")))
               keys))
             (c
              (make-instance
               'object-grain
               :fullname `(:cffi-grovel-c ,fname)
               :vp (make-vp :obj bname "/" suffix "." "c")
               :parent build))
             (exe
              (make-instance
               'object-grain
               :fullname `(:cffi-grovel-exe ,fname)
               :vp (make-vp :obj bname "/" suffix "." "exe")
               :parent build))
             (targets (list lisp c exe))
             (build-depends-on
              (cond
                ((slot-boundp grain 'build-depends-on) build-depends-on)
                ((slot-boundp build 'build-depends-on) (slot-value build 'build-depends-on))
                (t nil)))
             (generator
              (make-instance
               'lisp-generator
               :build build
               :targets targets
               :dependencies
               `(,@(normalize-dependencies build build-depends-on :build-depends-on)
                   (:build "/cffi/grovel")
                   ,@(normalize-dependencies build compile-depends-on :compile-depends-on)
                   ,@(normalize-dependencies build depends-on :depends-on)
                   (:process-cffi-grovel-file ,fname ,@(when cc-flags `(:cc-flags ,cc-flags)))))))
    (loop :for target :in targets :do
      (setf (registered-grain (fullname target)) target)
      (setf (grain-generator target) generator)))))
  (values))

(define-build-command-for :process-cffi-grovel-file (env name &key cc-flags)
  (issue-build-command env
    `(:process-cffi-grovel-file ,name ,@(when cc-flags `(:cc-flags ,cc-flags)))))

(defmethod compute-fullname ((grain cffi-grovel-specification-grain))
  nil)

(defmethod asdf-spec (env (grain cffi-grovel-specification-grain))
  `(:cffi-grovel
    ,(asdf-dependency-grovel::strip-extension
      (enough-namestring (grain-namestring env grain))
      "lisp")))

(define-text-for-xcvb-driver-command :process-cffi-grovel-file (env name &key cc-flags)
  (let ((spec-path (fullname-enough-namestring env `(:cffi-grovel ,name)))
        (c-path (fullname-enough-namestring env `(:cffi-grovel-c ,name)))
        (exe-path (fullname-enough-namestring env `(:cffi-grovel-exe ,name)))
        (lisp-path (fullname-enough-namestring env `(:lisp ,name))))
    (format nil "(:process-cffi-grovel-file ~S ~S ~S ~S~@[ :cc-flags '~S~])"
            spec-path c-path exe-path lisp-path cc-flags)))
  
(defmethod validate-fullname ((grain cffi-grovel-specification-grain))
  (ensure-valid-fullname (fullname grain) :cffi-grovel))
(defmethod validate-fullname ((grain cffi-grovel-processed-grain))
  (ensure-valid-fullname (fullname grain) :lisp))
(define-normalize-dependency :cffi-grovel (grain name)
  (normalize-dependency-lisp* :cffi-grovel grain name))

(dolist (spec
          '((:cffi-grovel . cffi-grovel-specification-grain)
            (:cffi-grovel-exe . object-grain)
            (:cffi-grovel-c . object-grain)))
  (pushnew spec +dependency-type+))

(define-linkable-dependency :cffi-grovel (x) `(:fasl ,x))
(define-compiled-dependency :cffi-grovel (x) (list (compile-time-fasl-type) x))

;; export a cffi-grovel:grovel-file component (?)

;;;; dependencies-interpreter

#|
(make-computation
 env
 :inputs ...
  `(:xcvb-driver-command
    (:call :cffi-grovel :process-grovel-file
           ,(grain-pathname cffi-grovel-lisp-module-grain)
           ,(grain-pathname cffi-grovel-results-lisp-module-grain))))
|#
;;;# ASDF component: WRAPPER-FILE
#|
(defclass wrapper-file (asdf:cl-source-file cc-flags-mixin)
  ((soname :initform nil :initarg :soname :accessor soname-of))
  (:documentation
   "This ASDF component defines COMPILE-OP and LOAD-SOURCE-OP
operations that take care of calling PROCESS-WRAPPER-FILE in
order to generate a foreign library and matching CFFI bindings
that are subsequently compiled and/or loaded."))

(defun %perform-process-wrapper-file (op c)
  (let ((fasl-file (ensure-pathname (car (asdf:output-files op c)))))
    (values (process-wrapper-file (asdf:component-pathname c)
                                  fasl-file
                                  (or (soname-of c)
                                      (asdf:component-name c)))
            fasl-file)))

(defmethod asdf:perform ((op asdf:compile-op) (c wrapper-file))
  (multiple-value-bind (generated-source-file fasl-file)
      (%perform-process-wrapper-file op c)
    (compile-file generated-source-file
                  :output-file fasl-file
                  #+ecl :system-p #+ecl t)))

(defmethod asdf:perform ((op asdf:load-source-op) (c wrapper-file))
  (load (%perform-process-wrapper-file op c)))
|#
