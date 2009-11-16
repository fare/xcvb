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

(defun slave-builder (&key
                      build setup xcvb-path
                      output-path object-directory
                      lisp-implementation lisp-binary-path
                      disable-cfasl base-image verbosity profiling)
  (multiple-value-bind (makefile-path makefile-dir)
      ;; Note that make-makefile calls handle-common-options for us.
      (make-makefile
       :master t
       :build build :setup setup
       :xcvb-path xcvb-path :output-path output-path
       :object-directory object-directory
       :lisp-implementation lisp-implementation :lisp-binary-path lisp-binary-path
       :disable-cfasl disable-cfasl :base-image base-image
       :verbosity verbosity :profiling profiling)
    (let* ((*default-pathname-defaults* makefile-dir)
           (make-command
            (list "make"
                  "-C" (namestring makefile-dir)
                  "-f" (namestring makefile-path))))
      (log-format 6 "~&Building with ~S~%" make-command)
      (let ((*standard-output* *error-output*))
        (run-program/process-output-stream
         make-command
         (lambda (stream) (copy-stream-to-stream-line-by-line stream *standard-output*))))
      (let* ((env (make-instance 'static-makefile-traversal))
             (issued
              (progn
                (build-command-for env (handle-target build))
                (reverse (traversed-build-commands-r env))))
             (manifest-spec (commands-to-manifest-spec env issued))
             (manifest-form (manifest-form manifest-spec)))
        (with-safe-io-syntax ()
          (write-string +xcvb-slave-greeting+)
          (write manifest-form :readably nil :pretty t :case :downcase)
          (write-string +xcvb-slave-farewell+)))))
  (values))
