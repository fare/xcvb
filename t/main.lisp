#+xcvb (module (:depends-on ("package")))

(in-package #:xcvb-test)

(define-command run-unit-tests
    (("unit-tests" "ut")
     (args) ()
     "Run unit tests"
     "Run a bunch of tests for xcvb as built into the current xcvb-test image")
  (declare (ignore args))
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

(defun unit-tests () nil)

(defun main (&rest arguments)
  (with-safe-io-syntax (:package :xcvb-test)
    (let ((*program* "xcvb-test"))
      (apply 'xcvb::main arguments))))

(defun entry-point ()
  (apply 'main uiop/image:*command-line-arguments*))
