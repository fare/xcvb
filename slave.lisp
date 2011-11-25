#+xcvb (module (:depends-on ("commands")))

(in-package :xcvb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; slave builder ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command slave-builder
    (("slave-builder")
     (&rest keys &key)
     `(,@+build-option-spec+
       ,@+setup-option-spec+
       ,@+base-image-option-spec+
       ,@+source-registry-option-spec+
       (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
       ,@+xcvb-program-option-spec+
       ,@+install-option-spec+
       ,@+workspace-option-spec+
       ,@+lisp-implementation-option-spec+
       ,@+cfasl-option-spec+
       ,@+verbosity-option-spec+
       ,@+profiling-option-spec+)
     "Build some project as a slave to the XCVB master (for internal use)"
     "Build some project as a slave to the XCVB master (for internal use)"
     (build))
  (multiple-value-bind (code makefile-dir)
      ;; Note that make-makefile calls handle-common-options for us.
      (apply 'make-build :master t :retry nil :exit nil keys)
    (unless (zerop code)
      (exit code))
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
