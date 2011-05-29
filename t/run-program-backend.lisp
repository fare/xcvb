#+xcvb (module (:depends-on ("helpers" "specials")))

(in-package #:xcvb-unit-tests)

(defsuite* (test-run-program-backend
            :in test-xcvb
            :documentation "Test the run-program-backend (simple-build)"))

(defun test-simple-build (&key (build (first +example-builds+))
                          (implementation *lisp-implementation-type*))
  (check-type build string)
  (cmd 'simple-build :build build
       :lisp-implementation (string-downcase implementation)
       :verbosity 10))

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
