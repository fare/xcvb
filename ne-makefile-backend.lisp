#+xcvb
(module
  (:depends-on ("makefile-backend" "asdf-backend" "simplifying-traversal")))

(in-package :xcvb)

(defclass nem-traversal (simplifying-traversal)
  ())

(define-load-command-for :build ((env nem-traversal) name)
  (unless (equal name "/asdf")
    (call-next-method)))

(defmethod issue-dependency ((env nem-traversal) (grain lisp-grain))
  (let* ((build (build-grain-for grain))
         (name (fullname build)))
    (setf (gethash name *target-builds*) t))
  (call-next-method))

(defun make-nem-stage (asdf-name build-name build &key previous parallel)
  (let* ((*computations* nil)
         (*asdf-system-dependencies* nil)
         (*require-dependencies* nil)
         (_g (graph-for-build-grain (make-instance 'nem-traversal) build))
         (inputs (loop :for computation :in (reverse *computations*)
                   :collect (first (computation-inputs computation))))
         (asdfile (object-namestring nil (strcat "/" asdf-name ".asd")))
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
    (computations-to-Makefile)))

(defun write-non-enforcing-makefile (build-names &key output-path asdf-name parallel)
  "Write a Makefile to output-path with information about how to compile the specified BUILD
in a fast way that doesn't enforce dependencies."
  (let* ((*print-pretty* nil); otherwise SBCL will slow us down a lot.
         (*use-cfasls* nil) ;; We use ASDF that doesn't know about them
         (builds (mapcar #'registered-build build-names))
         (last-build (first (last builds)))
         (asdf-names (loop :for (build . rest) :on build-names :for i :from 1
                       :collect (if rest (format nil "~A-stage~D" asdf-name i) asdf-name)))
         (default-output-path (merge-pathnames "xcvb-ne.mk" (grain-pathname last-build)))
         (output-path (merge-pathnames output-path default-output-path))
         (makefile-path (ensure-absolute-pathname output-path))
         (makefile-dir (pathname-directory-pathname makefile-path))
         (*default-pathname-defaults* makefile-dir)
         (*makefile-target-directories* nil)
         (*makefile-phonies* nil)
         (static-rules
          (prog2
              (issue-image-named (make-instance 'static-traversal) nil)
              (computations-to-Makefile)
            (setf *computations* nil)))
         (build-rules
          (loop
            :for build-name :in build-names
            :for build :in builds
            :for previous-asdf = nil :then asdf-name
            :for asdf-name :in asdf-names
            :for first = t :then nil
            :collect (make-nem-stage asdf-name build-name build
                                     :previous previous-asdf
                                     :parallel parallel))))
      (with-open-file (out makefile-path
                           :direction :output
                           :if-exists :supersede)
        (write-makefile-prelude out)
        (dolist (body (cons static-rules build-rules))
          (princ body out))
        (write-makefile-conclusion out))))
