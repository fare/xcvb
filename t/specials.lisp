#+xcvb (module (:depends-on ("package")))

(in-package #:xcvb-unit-tests)

;; These are the only supported so far -- please add support for more!
(defparameter +xcvb-lisps+ '(:clisp :ccl :sbcl)
  "Lisp implementations that can compile XCVB itself")

;; These are missing for the following reason:
;; :abcl :xcl are missing create-image
;; :cmucl is not on my system, but should work - hopefully :scl is good approximation
;; :gcl (gclcvs from debian) seems to be buggy and not able to compile driver.lisp
;; :lispworks to be tested
;; :allegro to be tested
;; :allegromodern to be tested (?)
(defparameter +simple-target-lisps+ '(:clisp :ccl :sbcl :scl)
  "Lisp implementations that can be targetted by the simple backend,
spawning a new process for each build step.")

(defparameter +farmer-target-lisps+ '(:clisp :ccl :sbcl)
  "Lisp implementations that can be targetted by the farmer backend,
forking processes to share state between build steps.")

(defparameter +example-builds+
  '("/xcvb/example-1" "/xcvb/example-2" "/xcvb/hello")
  "example builds for XCVB")
