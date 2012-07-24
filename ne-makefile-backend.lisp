#+xcvb
(module
  (:depends-on ("makefile-backend" "asdf-backend" "simplifying-traversal")))

(in-package :xcvb)

(defclass nem-traversal (asdf-traversal makefile-traversal)
  ())

(defmethod build-in-target-p ((env nem-traversal) build)
  (declare (ignorable env build))
  t)

(defmethod issue-dependency ((env nem-traversal) (grain lisp-file-grain))
  (unless (member (second (fullname grain)) '("/xcvb/driver" "/asdf/asdf" "/poiu/poiu")
                  :test 'equal)
    (call-next-method))
  (values))

(defun make-nem-stage (env asdf-name build &key previous parallel)
  (nest
   (let ((*computations* nil)
         (*asdf-system-dependencies* nil)
         (*require-dependencies* nil)))
   (progn (graph-for-build-module-grain env build))
   (let ((inputs (loop :for computation :in (reverse *computations*)
                   :for i = (first (computation-inputs computation))
                   :when (and i (not (grain-computation i)))
                   :collect i))
         (asd-vp (make-vp :obj "/" asdf-name "." "asd"))))
   (progn (do-write-asd-file env :output-path (vp-namestring env asd-vp)
                             :asdf-name asdf-name))
   (let* ((image-name `(:image ,(strcat "/" asdf-name)))
          (image (setf (registered-grain image-name)
                       (make-grain 'image-grain :fullname image-name)))
          (previous-spec (if previous
                             `(:image (:image ,(strcat "/" previous)))
                             (progn
                               (setf inputs
                                     (append (mapcar #'registered-grain *lisp-setup-dependencies*)
                                             inputs))
                               `(:load ,*lisp-setup-dependencies*))))
          (computation
           (make-computation ()
             :outputs (list image)
             :inputs inputs
             :command
             `(:xcvb-driver-command ,previous-spec
                (:create-image
                (:image ,image-name)
                (:register-asdf-directory ,*object-cache*)
                ,@(when parallel
                    `((:register-asdf-directory
                       ,(pathname-directory-pathname
                         (grain-pathname (registered-build "/poiu" :ensure-build t))))))
                (:initialize-asdf)
                ,@(when parallel '((:load-asdf :poiu)))
                (:load-asdf ,(coerce-asdf-system-name asdf-name)
                            ,@(when parallel `(:parallel t)))))))
          (*computations* (list computation)))
     (computations-to-Makefile env))))

(defun write-non-enforcing-makefile (build-names &key output-path asdf-name parallel)
  "Write a Makefile to output-path with information about how to compile the specified BUILD
in a fast way that doesn't enforce dependencies."
  (let* ((*print-pretty* nil); otherwise SBCL will slow us down a lot.
         (*use-cfasls* nil) ;; We use ASDF that doesn't know about them
         (builds ;; TODO: somehow use handle-target instead
          (mapcar (lambda (n) (registered-build n :ensure-build t)) build-names))
         (last-build (first (last builds)))
         (asdf-names (loop :for (build . rest) :on build-names :for i :from 1
                       :collect (if rest
                                    (format nil "~A-stage~D-~A" asdf-name i build)
                                    asdf-name)))
         (default-output-path (subpathname (grain-pathname last-build) "xcvb-ne.mk"))
         (output-path (merge-pathnames* output-path default-output-path))
         (makefile-path (ensure-pathname-absolute output-path))
         (makefile-dir (pathname-directory-pathname makefile-path))
         (*default-pathname-defaults* makefile-dir)
         (*makefile-target-directories* (make-hash-table :test 'equal))
         (*makefile-target-directories-to-mkdir* nil)
         (*makefile-phonies* nil)
         (lisp-env-var (lisp-environment-variable-name :prefix nil))
         (*lisp-executable-pathname* ;; magic escape!
          (list :makefile "${" lisp-env-var "}"))
         (smt (make-instance 'static-makefile-traversal))
         (env (make-instance 'nem-traversal))
         (static-rules
          (prog2
              (issue-image-named smt nil)
              (computations-to-Makefile smt)
            (setf *computations* nil)))
         (build-rules
          (loop
            ;; :for build-name :in build-names
            :for build :in builds
            :for previous-asdf = nil :then asdf-name
            :for asdf-name :in asdf-names
            :collect (make-nem-stage env asdf-name build
                                     :previous previous-asdf
                                     :parallel parallel))))
    (ensure-directories-exist makefile-path)
    (with-open-file (out makefile-path
			 :direction :output
			 :if-exists :supersede)
      (write-makefile-prelude :stream out :lisp-env-var lisp-env-var)
      (dolist (body (reverse (cons static-rules build-rules)))
	(princ body out))
      (write-makefile-conclusion out))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; non-enforcing makefile ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command non-enforcing-makefile
    (("non-enforcing-makefile" "nemk" "nm")
     (&rest keys &key)
     `(,@+multi-build-option-spec+
       ,@+setup-option-spec+
       ,@+base-image-option-spec+
       ,@+source-registry-option-spec+
       (("name" #\n) :type string :optional t :initial-value "xcvb-tmp" :documentation "ASDF name for the target")
       (("output-path" #\o) :type string :initial-value "xcvb-ne.mk" :documentation "specify output path")
       ,@+xcvb-program-option-spec+
       ,@+workspace-option-spec+
       ,@+lisp-implementation-option-spec+
       (("parallel" #\P) :type boolean :optional t :initial-value nil :documentation "compile in parallel with POIU")
       ;; ,@+profiling-option-spec+
       ,@+verbosity-option-spec+)
     "Create some Makefile for a non-enforcing build"
     "Create some Makefile for a non-enforcing build,
that will use ASDF or POIU to create one or a series of images each containing
the previous image, a build and its dependencies."
     (build name output-path parallel))
  (apply 'handle-global-options #|:disable-cfasl (not force-cfasl)|# keys)
  (with-maybe-profiling ()
    (write-non-enforcing-makefile
     (mapcar #'canonicalize-fullname build)
     :asdf-name name
     :output-path output-path
     :parallel parallel)))
