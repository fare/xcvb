#+xcvb
(module
  (:depends-on
   ("makefile-backend" "static-traversal" "computations" "target-lisp-commands"
    "grain-implementation" "asdf-backend" "dependencies-interpreter")))

(in-package :xcvb)

(defclass cffi-grovel-specification-grain (lisp-module-grain)
  ((fullname :initarg :fullname)
   (cc-flags :initarg :cc-flags :initform nil)))

(defclass object-grain (file-grain)
  ((parent :initarg :parent)
   (fullname :initarg :fullname)))

(define-handle-extension-form :cffi-grovel (build name &rest keys &key
                                            cc-flags depends-on build-depends-on
                                            compile-depends-on cload-depends-on load-depends-on)
  (declare (ignorable cc-flags depends-on build-depends-on
                      compile-depends-on cload-depends-on load-depends-on))
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
               'lisp-file-grain
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

(defgeneric build-command-for-process-cffi-grovel-file (env name &key cc-flags))
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
  (let ((spec-path (effective-namestring env `(:cffi-grovel ,name)))
        (c-path (effective-namestring env `(:cffi-grovel-c ,name)))
        (exe-path (effective-namestring env `(:cffi-grovel-exe ,name)))
        (lisp-path (effective-namestring env `(:lisp ,name))))
    (format nil "(:process-cffi-grovel-file ~S ~S ~S ~S~@[ :cc-flags '~S~])"
            spec-path c-path exe-path lisp-path cc-flags)))

(defmethod validate-fullname ((grain cffi-grovel-specification-grain))
  (ensure-valid-fullname (fullname grain) :type :cffi-grovel))
(define-normalize-dependency :cffi-grovel (grain name)
  (normalize-dependency-lisp* :cffi-grovel grain name))

(dolist (spec
          '((:cffi-grovel . cffi-grovel-specification-grain)
            (:cffi-grovel-exe . object-grain)
            (:cffi-grovel-c . object-grain)
            (:cffi-wrapper . cffi-wrapper-specification-grain)
            (:cffi-wrapper-so . object-grain)
            (:cffi-wrapper-c . object-grain)))
  (pushnew spec +dependency-type+))

(define-linkable-dependency :cffi-grovel (x) `(:fasl ,x))
(define-compiled-dependency :cffi-grovel (x) (list (compile-time-fasl-type) x))


;; From cffi
(defparameter *cffi-feature-suffix-map*
  '((:windows . ".dll")
    (:darwin . ".dylib")
    (:unix . ".so")
    (t . ".so"))
  "Mapping of OS feature keywords to shared library suffixes.")
(defun default-library-suffix ()
  "Return a string to use as default library suffix based on the
operating system.  This is used to implement the :DEFAULT option.
This will need to be extended as we test on more OSes."
  (or (cdr (assoc-if #'target-feature-p *cffi-feature-suffix-map*))
      (error "Unable to determine the default library suffix on this OS.")))


(defclass cffi-wrapper-specification-grain (lisp-module-grain)
  ((fullname :initarg :fullname)
   (cc-flags :initarg :cc-flags :initform nil)
   (soname :initarg :soname :initform nil)))

(define-handle-extension-form :cffi-wrapper (build name &rest keys &key
                                             cc-flags depends-on build-depends-on
                                             compile-depends-on cload-depends-on load-depends-on)
  (declare (ignorable cc-flags depends-on build-depends-on
                      compile-depends-on cload-depends-on load-depends-on))
  (let* ((bname (fullname build))
         (prefix (strcat bname "/"))
         (suffix (cond
                   ((eql #\/ (first-char name))
                    (or (string-strip-prefix prefix name)
                        (error ":cffi-wrapper module name ~A not under build name ~A"
                               name bname)))
                   (t
                    name)))
         (fname (strcat prefix suffix))
         (grain
          (apply #'make-instance 'cffi-wrapper-specification-grain
                 :fullname `(:cffi-wrapper ,fname)
                 :parent build
                 :vp (make-vp :src bname "/" suffix "." "lisp")
                 keys)))
    (setf (registered-grain `(:cffi-wrapper ,fname)) grain)
    (with-slots (fullname
                 parent cc-flags
                 build-depends-on compile-depends-on
                 cload-depends-on load-depends-on) grain
      (let* ((lisp
              (apply 'make-instance
               'lisp-file-grain
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
               :fullname `(:cffi-wrapper-c ,fname)
               :vp (make-vp :obj bname "/" suffix "." "c")
               :parent build))
             (so
              (make-instance
               'object-grain
               :fullname `(:cffi-wrapper-so ,fname)
               :vp (make-vp :obj bname "/" suffix "."
                            (subseq (default-library-suffix) 1))
               :parent build))
             (targets (list lisp c so))
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
                   (:process-cffi-wrapper-file ,fname ,@(when cc-flags `(:cc-flags ,cc-flags)))))))
    (loop :for target :in targets :do
      (setf (registered-grain (fullname target)) target)
      (setf (grain-generator target) generator)))))
  (values))

(defgeneric build-command-for-process-cffi-wrapper-file (env name &key cc-flags))
(define-build-command-for :process-cffi-wrapper-file (env name &key cc-flags)
  (issue-build-command env
    `(:process-cffi-wrapper-file ,name ,@(when cc-flags `(:cc-flags ,cc-flags)))))

(defmethod compute-fullname ((grain cffi-wrapper-specification-grain))
  nil)

(defmethod asdf-spec (env (grain cffi-wrapper-specification-grain))
  `(:cffi-wrapper
    ,(asdf-dependency-grovel::strip-extension
      (enough-namestring (grain-namestring env grain))
      "lisp")))

(define-text-for-xcvb-driver-command :process-cffi-wrapper-file (env name &key cc-flags)
  (let ((spec-path (effective-namestring env `(:cffi-wrapper ,name)))
        (c-path (effective-namestring env `(:cffi-wrapper-c ,name)))
        (so-path (effective-namestring env `(:cffi-wrapper-so ,name)))
        (lisp-path (effective-namestring env `(:lisp ,name))))
    (format nil "(:process-cffi-wrapper-file ~S ~S ~S ~S~@[ :cc-flags '~S~])"
            spec-path c-path so-path lisp-path cc-flags)))

(defmethod validate-fullname ((grain cffi-wrapper-specification-grain))
  (ensure-valid-fullname (fullname grain) :type :cffi-wrapper))
(define-normalize-dependency :cffi-wrapper (grain name)
  (normalize-dependency-lisp* :cffi-wrapper grain name))

(define-linkable-dependency :cffi-wrapper (x) `(:fasl ,x))
(define-compiled-dependency :cffi-wrapper (x) (list (compile-time-fasl-type) x))
