#+xcvb
(module
  (:depends-on ("makefile-backend" "asdf-backend" "simplifying-traversal")))

(in-package :xcvb)

(defclass nem-traversal (simplifying-traversal makefile-traversal)
  ())

(define-build-command-for :build ((env nem-traversal) name)
  (unless (equal name "/asdf")
    (call-next-method)))

(defmethod issue-dependency ((env nem-traversal) (grain lisp-grain))
  (let* ((build (build-grain-for grain))
         (name (fullname build)))
    (setf (gethash name *target-builds*) t))
  (call-next-method))

(defun make-nem-stage (env asdf-name build-name build &key previous parallel)
  (let* ((*computations* nil)
         (*asdf-system-dependencies* nil)
         (*require-dependencies* nil)
         (_g (graph-for-build-grain env build))
         (inputs (loop :for computation :in (reverse *computations*)
                   :collect (first (computation-inputs computation))))
         (asdfile (object-namestring env (strcat "/" asdf-name ".asd")))
         (_w (do-write-asd-file :output-path asdfile :build build-name :asdf-name asdf-name))
         (image-name `(:image ,(strcat "/" asdf-name)))
         (image (make-grain 'image-grain :fullname image-name))
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
               ,image-name
               (:register-asdf-directory ,(merge-pathnames (strcat *object-directory* "/")))
               ,@(when parallel
                       `((:register-asdf-directory
                          ,(pathname-directory-pathname (grain-pathname (registered-build "/poiu"))))
                         (:load-asdf :poiu)))
               (:load-asdf ,(coerce-asdf-system-name asdf-name)
                           ,@(when parallel `(:parallel t)))))))
         (*computations* (list computation)))
    (declare (ignore _w _g))
    (computations-to-Makefile env)))

(defun write-non-enforcing-makefile (build-names &key output-path asdf-name parallel)
  "Write a Makefile to output-path with information about how to compile the specified BUILD
in a fast way that doesn't enforce dependencies."
  (let* ((*print-pretty* nil); otherwise SBCL will slow us down a lot.
         (*use-cfasls* nil) ;; We use ASDF that doesn't know about them
         (builds (mapcar #'registered-build build-names))
         (last-build (first (last builds)))
         (asdf-names (loop :for (build . rest) :on build-names :for i :from 1
                       :collect (if rest (format nil "~A-stage~D-~A" asdf-name i build) asdf-name)))
         (default-output-path (merge-pathnames "xcvb-ne.mk" (grain-pathname last-build)))
         (output-path (merge-pathnames output-path default-output-path))
         (makefile-path (ensure-absolute-pathname output-path))
         (makefile-dir (pathname-directory-pathname makefile-path))
         (*default-pathname-defaults* makefile-dir)
         (*makefile-target-directories* nil)
         (*makefile-phonies* nil)
         (env (make-instance 'nem-traversal))
         (static-rules
          (prog2
              (issue-image-named env nil)
              (computations-to-Makefile env)
            (setf *computations* nil)))
         (build-rules
          (loop
            :for build-name :in build-names
            :for build :in builds
            :for previous-asdf = nil :then asdf-name
            :for asdf-name :in asdf-names
            :collect (make-nem-stage env asdf-name build-name build
                                     :previous previous-asdf
                                     :parallel parallel))))
      (with-open-file (out makefile-path
                           :direction :output
                           :if-exists :supersede)
        (write-makefile-prelude out)
        (dolist (body (cons static-rules build-rules))
          (princ body out))
        (write-makefile-conclusion out))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; non-enforcing makefile ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +non-enforcing-makefile-option-spec+
 '((("build" #\b) :type string :optional nil :list t :documentation "specify a (series of) system(s) to build")
   (("base-image" #\B) :type boolean :optional t :initial-value nil :documentation "use a base image")
   (("name" #\n) :type string :optional t :initial-value "xcvb-tmp" :documentation "ASDF name for the target")
   (("setup" #\s) :type string :optional t :documentation "specify a Lisp setup file")
   (("xcvb-path" #\x) :type string :optional t :documentation "override your XCVB_PATH")
   (("output-path" #\o) :type string :initial-value "xcvb-ne.mk" :documentation "specify output path")
   (("object-directory" #\O) :type string :initial-value "obj-ne" :documentation "specify object directory")
   (("lisp-implementation" #\i) :type string :initial-value "sbcl" :documentation "specify type of Lisp implementation")
   (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
   (("verbosity" #\v) :type integer :initial-value 5 :documentation "set verbosity")
   (("parallel" #\P) :type boolean :optional t :initial-value nil :documentation "compile in parallel with POIU")
   ;;(("debugging" #\D) :type boolean :optional t :initial-value nil :documentation "debug")
;  (("force-cfasl" #\C) :type boolean :optional t :initial-value nil :documentation "force use of CFASL")
;  (("profiling" #\P) :type boolean :optional t :documentation "profiling")
   ))

(defun non-enforcing-makefile (&key
                               build base-image setup xcvb-path name
                               output-path object-directory
                               lisp-implementation lisp-binary-path
                               verbosity parallel #|force-cfasl profiling|#)
  (reset-variables)
  (when xcvb-path
    (set-search-path! xcvb-path))
  (when verbosity
    (setf *xcvb-verbosity* verbosity))
  (when output-path
    (setf *default-pathname-defaults*
          (ensure-absolute-pathname (pathname-directory-pathname output-path))))
  (when object-directory
    (setf *object-directory* ;; strip last "/"
          (but-last-char (enough-namestring (ensure-pathname-is-directory object-directory)))))
  (when lisp-implementation
    (setf *lisp-implementation-type*
          (find-symbol (string-upcase lisp-implementation) (find-package :keyword))))
  (when lisp-binary-path
    (setf *lisp-executable-pathname* lisp-binary-path))
  (extract-target-properties)
  (read-target-properties)
  ;;(setf *use-cfasls* force-cfasl)
  (setf *use-base-image* base-image)
  (setf *lisp-setup-dependencies*
        (append +xcvb-setup-dependencies+
                (when setup `((:lisp ,setup)))))
  (search-search-path)
  (write-non-enforcing-makefile
   (mapcar #'canonicalize-fullname build)
   :asdf-name name
   :output-path output-path
   :parallel parallel))
