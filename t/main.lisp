#+xcvb (module (:depends-on ("sub-xcvb")))

(in-package #:xcvb-unit-tests)

(defparameter +xcvb-test-commands+
  '((("help" "-?" "--help" "-h")
     program-help ()
     "Output some help message"
     "With no additional arguments, the 'help' command provides general help on
using XCVB-test.  Using 'xcvb-test help COMMAND' where COMMAND is
the name of an XCVB-test command gives specific help on that command.")
    (("unit-tests" "ut")
     unit-tests +unit-tests-option-spec+
     "Run unit tests"
     "Run a bunch of tests for xcvb as built into the current xcvb-test image")
    (("test-xcvb-dir" "tx")
     test-xcvb-dir +test-xcvb-dir-option-spec+
     "Test a XCVB source directory"
     "Compile a XCVB checkout and run tests on it")
    (("test-release-dir" "tr")
     test-release-dir +test-release-dir-option-spec+
     "Test a XCVB release directory"
     "Compile a XCVB release directory and run tests on it")))

(defparameter +unit-tests-option-spec+
  '())

(defparameter +test-xcvb-dir-option-spec+
  '())

(defparameter +test-release-dir-option-spec+
  '())

(defun run-unit-tests ()
  (test-xcvb))


(defun program-help (args)
  (xcvb::program-help args :commands +xcvb-test-commands+ :name "xcvb-test"))

(defun main (&rest arguments)
  (xcvb::main* :name "xcvb-test" :commands +xcvb-test-commands+ :arguments arguments))
