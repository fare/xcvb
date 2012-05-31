#+xcvb (module (:depends-on ("helpers" "specials")))

(in-package #:xcvb-test)

(declaim (optimize (debug 3) (safety 3)))

(defsuite* (test-run-program-backend
            :in xcvb-test
            :documentation "Test the run-program-backend (simple-build)"))

(defun test-simple-build (&key (build (first +example-builds+))
                          (implementation *lisp-implementation-type*))
  (check-type build string)
  (let* ((*package* (find-package :xcvb))
	 (workspace (subpathname *temporary-directory* "xcvb-test/"))
	 (cache (subpathname workspace "cache/"))
	 (object-cache (subpathname workspace "obj/"))
	 (lisp-key (find-symbol* (string-upcase implementation) :keyword)))
    (when (lisp-present-p lisp-key)
      (rm-rfv workspace)
      (cmd 'simple-build
	   :build build
	   :lisp-implementation (string-downcase implementation)
	   :cache cache :object-cache object-cache :workspace workspace
	   :verbosity 10))))

(macrolet ((defs ()
             (let ((defined (make-hash-table :test 'equal)))
               (labels ((call-with-definition (key thunk)
                          (unless (gethash key defined)
                            (setf (gethash key defined) t)
                            (funcall thunk)))
                        (defsb (&key (build (first +example-builds+))
                                     (implementation *lisp-implementation-type*))
                          (setf implementation (string-downcase implementation))
			  (call-with-definition
			   `(simple-build ,implementation ,build)
                           (lambda ()
                             `(deftest ,(intern (format nil "~:@(test/simple-build/~A~A~)"
                                                        implementation build)) ()
                                (test-simple-build :build ,build
                                                   :implementation ,implementation))))))
                 `(progn
                    ,@(loop :for i :in +simple-target-lisps+ :collect
                        (defsb :implementation i))
                    ,@(loop :for b :in (append1 +example-builds+ "/xcvb") :collect
                        (defsb :build b)))))))
  (defs))
