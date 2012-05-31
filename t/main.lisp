#+xcvb (module (:depends-on ("package")))

(in-package #:xcvb-test)

(define-command run-driver-tests
    (("driver-tests" "dt")
     (args) ()
     "Run driver unit tests"
     "Run a bunch of tests for xcvb driver on specified implementations,
or all supported implementations if none are specified")
  (loop :for lisp :in (or (mapcar #'upkeywordp args) +xcvb-lisps+)
    :when (lisp-present-p lisp) :do
    (xcvb::lisp-invocation-arglist
     :implementation-type lisp
     :eval (format nil "(and#.(load ~S)#.(asdf:load-system :xcvb-driver-test)#.(xcvb-driver:with-coded-exit () (xcvb-driver-test:xcvb-driver-test)))" (find-asdf)))))

(define-command run-unit-tests
    (("unit-tests" "ut")
     (args) ()
     "Run unit tests"
     "Run a bunch of tests for xcvb as built into the current xcvb-test image")
  (declare (ignore args))
  (xcvb-driver-test)
  (let ((failures
         (mapcan (lambda (x)
                   (coerce (hu.dwim.stefil::failure-descriptions-of (funcall x)) 'list))
                 '(xcvb-test))))
    (when failures
      (error "failures in XCVB unit-tests:~%~{~S~%~}" failures))))

(map () 'xcvb::register-command '(xcvb::load-command xcvb::eval-command xcvb::repl-command))

(define-command program-help
    (("help" "-?" "--help" "-h")
     (args) ()
     "Output some help message"
     "With no additional arguments, the 'help' command provides general help on
using XCVB-test.  Using 'xcvb-test help COMMAND' where COMMAND is
the name of an XCVB-test command gives specific help on that command.")
  (xcvb::program-help args :name "xcvb-test"))

(defun main (&rest arguments)
  (with-safe-io-syntax (:package :xcvb-test)
    (let ((*program* "xcvb-test"))
      (xcvb::main* arguments))))
