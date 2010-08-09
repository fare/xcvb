#+xcvb (module (:depends-on ("main")))

(in-package :xcvb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; slave builder ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +slave-builder-option-spec+
  `((("build" #\b) :type string :optional nil :documentation "specify a (series of) system(s) to build")
    ,@+setup-option-spec+
    ,@+source-registry-option-spec+
    (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
    (("object-directory" #\O) :type string :initial-value "obj" :documentation "specify object directory")
    ,@+lisp-implementation-option-spec+
    ,@+cfasl-option-spec+
    (("base-image" #\B) :type boolean :optional t :initial-value nil :documentation "use a base image")
    ,@+verbosity-option-spec+
    ,@+profiling-option-spec+))

(defun slave-builder (&key
                      build setup source-registry
                      output-path object-directory
                      lisp-implementation lisp-binary-path
                      define-feature undefine-feature
                      disable-cfasl base-image verbosity profiling debugging)
  (multiple-value-bind (makefile-path makefile-dir)
      ;; Note that make-makefile calls handle-common-options for us.
      (make-makefile
       :master t
       :build build :setup setup
       :source-registry source-registry :output-path output-path
       :object-directory object-directory
       :lisp-implementation lisp-implementation :lisp-binary-path lisp-binary-path
       :define-feature define-feature :undefine-feature undefine-feature
       :disable-cfasl disable-cfasl :base-image base-image
       :verbosity verbosity :profiling profiling :debugging debugging)
    (let* ((make-command
            (list "make"
                  "-C" (namestring makefile-dir)
                  "-f" (namestring makefile-path)))
           (*standard-output* *error-output*))
      (log-format 6 "Building with ~S" make-command)
      (run-program/process-output-stream
       make-command
       (lambda (stream) (copy-stream-to-stream-line-by-line stream *standard-output*))))
    (let* ((*default-pathname-defaults* makefile-dir)
           (env (make-instance 'static-makefile-traversal))
           (issued
            (progn
              (build-command-for env (handle-target build))
              (reverse (traversed-build-commands-r env))))
           (manifest-spec (commands-to-manifest-spec env issued))
           (manifest-form (manifest-form manifest-spec)))
      (with-safe-io-syntax ()
        (write-string +xcvb-slave-greeting+)
        (write manifest-form :readably nil :pretty t :case :downcase)
        (write-string +xcvb-slave-farewell+))))
  (values))
