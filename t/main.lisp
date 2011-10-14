#+xcvb (module (:depends-on ("sub-xcvb")))

(in-package #:xcvb-test)

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
    (("validate-xcvb-dir" "vx")
     validate-xcvb-dir +validate-xcvb-dir-option-spec+
     "Test a XCVB source directory"
     "Compile a XCVB checkout and run tests on it")
    (("validate-xcvb-dir-all-lisps" "vx")
     validate-xcvb-dir-all-lisps +validate-xcvb-dir-all-lisps-option-spec+
     "Test a XCVB source directory"
     "Compile a XCVB checkout and run tests on it")
    (("validate-release-dir" "vr")
     validate-release-dir +validate-release-dir-option-spec+
     "Test a XCVB release directory"
     "Compile a XCVB release directory and run tests on it")
    (("validate-release-dir-all-lisps" "vral")
     validate-release-dir-all-lisps +validate-release-dir-all-lisps-option-spec+
     "Test a XCVB release directory"
     "Compile a XCVB release directory and run tests on it")))

(defparameter +unit-tests-option-spec+
  '())

(defparameter +base-xcvb-dir-option-spec+
  `((("build-dir") :type string :optional t :documentation "where to build stuff")
    (("object-dir") :type string :optional t :documentation "where to store object files")
    (("cl-launch-flags") :type string :optional t :documentation "cl-launch flags")
    ,@xcvb::+source-registry-option-spec+))

(defparameter +bootstrap-option-spec+
  `((("xcvb" #\S) :type string :optional t :documentation "name of bootstrap XCVB binary")
    (("install-bin") :type string :optional t :documentation "where to install binaries")
    (("install-image") :type string :optional t :documentation "where to install images")
    ;;(("install-lisp") :type string :optional t :documentation "where to install source")
    ;;(("install-systems") :type string :optional t :documentation "where to install systems")
    (("parallelize") :type string :optional t :documentation "shall we parallelize?")))

(defparameter +lisp-implementation-option-spec+
  `((("implementation-type" #\S) :type string :optional t
     :documentation "the Lisp implementation type")))

(defparameter +validate-xcvb-dir-option-spec+
  `((("xcvb-dir" #\S) :type string :optional t :documentation "the XCVB directory")
    ,@+base-xcvb-dir-option-spec+
    ,@+bootstrap-option-spec+
    ,@+lisp-implementation-option-spec+))

(defparameter +validate-xcvb-dir-all-lisps-option-spec+
  `((("xcvb-dir" #\S) :type string :optional t :documentation "the XCVB directory")
    ,@+base-xcvb-dir-option-spec+
    ,@+bootstrap-option-spec+))

(defparameter +base-release-dir-option-spec+
  `((("release-dir" #\S) :type string :optional nil
     :documentation "the XCVB directory")
    ,@+base-xcvb-dir-option-spec+))

(defparameter +validate-release-dir-option-spec+
  `(,@+base-release-dir-option-spec+
    ,@+bootstrap-option-spec+
    ,@+lisp-implementation-option-spec+))

(defparameter +validate-release-dir-all-lisps-option-spec+
  `(,@+base-release-dir-option-spec+
    ,@+bootstrap-option-spec+))

(defun run-unit-tests ()
  (let ((failures 
         (mapcan (lambda (x)
                   (coerce (hu.dwim.stefil::failure-descriptions-of (funcall x)) 'list))
                 (list #'xcvb-driver-test #'xcvb-test))))
    (when failures
      (error "failures in XCVB unit-tests:~%~{~S~%~}" failures))))

(defun program-help (args)
  (xcvb::program-help args :commands +xcvb-test-commands+ :name "xcvb-test"))

(defun main (&rest arguments)
  (xcvb::main* :name "xcvb-test" :commands +xcvb-test-commands+ :arguments arguments))
