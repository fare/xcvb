#+xcvb
(module (:depends-on ("pkgdcl")))

(in-package :xcvb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; slave builder ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +slave-builder-option-spec+
 '((("build" #\b) :type string :optional nil :documentation "specify a (series of) system(s) to build")
   (("setup" #\s) :type string :optional t :documentation "specify a Lisp setup file")
   (("xcvb-path" #\x) :type string :optional t :documentation "override your XCVB_PATH")
   (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
   (("object-directory" #\O) :type string :initial-value "obj" :documentation "specify object directory")
   (("lisp-implementation" #\i) :type string :initial-value "sbcl" :documentation "specify type of Lisp implementation")
   (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
   (("disable-cfasl" #\C) :type boolean :optional t :initial-value nil :documentation "disable use of CFASL")
   (("base-image" #\B) :type boolean :optional t :initial-value nil :documentation "use a base image")
   (("verbosity" #\v) :type integer :initial-value 5 :documentation "set verbosity")
   (("profiling" #\P) :type boolean :optional t :documentation "profiling")
   ))

(defun slave-builder (arguments &key
                       build setup xcvb-path
                       output-path object-directory
                       lisp-implementation lisp-binary-path
                       disable-cfasl base-image verbosity profiling)
  (xcvb-driver::debugging)
  (multiple-value-bind (makefile-path makefile-dir)
      (make-makefile
       arguments :master t
       :build build :setup setup
       :xcvb-path xcvb-path :output-path output-path
       :object-directory object-directory
       :lisp-implementation lisp-implementation :lisp-binary-path lisp-binary-path
       :disable-cfasl disable-cfasl :base-image base-image :verbosity verbosity :profiling profiling)
    (let ((*standard-output* *error-output*)
          #|#+sbcl (sb-alien::*default-c-string-external-format* :iso-8859-1)|#)
      (run-program/process-output-stream
       "make" (list "-C" (namestring makefile-dir) "-f" (namestring makefile-path))
       (lambda (stream) (copy-stream-to-stream-line-by-line stream *standard-output*))))
    (let* ((image-grain (graph-for (make-instance 'static-traversal)
                                   `(:image ,(canonicalize-fullname build))))
           (included (image-included image-grain)))
      (with-safe-io-syntax ()
        (write `(:xcvb () ;;; TODO: do something about non-file dependencies
                       ,(manifest-form
                         (loop :for grain :in included
                           :for fullname = (fullname grain)
                           :when (typecase grain
                                   ((or lisp-grain fasl-grain cfasl-grain)
                                    t)
                                   (image-grain
                                    nil)
                                   (t
                                    (warn "Not including grain ~S" grain)
                                    nil))
                           :collect
                           `(:fullname fullname
                             :pathname (merge-pathnames (dependency-namestring fullname)
                                                        makefile-dir)
                             ,@(let ((source-grain (grain-source grain)))
                                 (when source-grain
                                   `((:source-pathname ,(grain-pathname source-grain)))))))))
               :readably t :pretty t :case :downcase)
        (terpri)))))
