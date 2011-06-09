#+xcvb (module (:depends-on ("package")))

(in-package #:xcvb-unit-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)

;; These are the only supported so far -- please add support for more!
(defparameter +xcvb-lisps+ '(:clisp :ccl :sbcl)
  "Lisp implementations that can compile XCVB itself")

;; These are missing for the following reason:
;; :abcl :xcl are missing create-image
;; :gcl (gclcvs from debian) has compiler bugs and is not even able to compile driver.lisp
;; :lispworks and :allegro work, but
;; being proprietary are not part of the regular testing regime
(defparameter +simple-target-lisps+ '(:clisp :ccl :sbcl :scl :ecl :cmucl)
  "Lisp implementations that can be targetted by the simple backend,
spawning a new process for each build step.")

(defparameter +farmer-target-lisps+ '(:clisp :ccl :sbcl)
  "Lisp implementations that can be targetted by the farmer backend,
forking processes to share state between build steps.")

(defparameter +example-builds+
  '("/xcvb/example-1" "/xcvb/example-2" "/xcvb/hello")
  "example builds for XCVB")

(defparameter +xcvb-repo+
  '(("/xcvb" :systems (:xcvb)
     :upstream "git://common-lisp.net/projects/xcvb/xcvb.git")))

(defparameter +xcvb-dependencies+
  '(("/asdf" :systems (:asdf)
     :upstream "git://common-lisp.net/projects/asdf/asdf.git")
    ("/alexandria" :systems (:alexandria)
     :repo "git://common-lisp.net/projects/xcvb/alexandria.git")
    ("/asdf-dependency-grovel" :systems (:asdf-dependency-grovel)
     :repo "git://common-lisp.net/projects/xcvb/asdf-dependency-grovel.git")
    ("/babel" :systems (:babel)
     :repo "git://common-lisp.net/projects/xcvb/babel.git")
    ("/binascii" :systems (:binascii)
     :upstream "git://github.com/froydnj/binascii.git")
    ("/bordeaux-threads" :systems (:bordeaux-threads)
     :repo "git://common-lisp.net/projects/xcvb/bordeaux-threads.git")
    ("/cffi" :systems (:cffi)
     :repo "git://common-lisp.net/projects/xcvb/cffi.git")
    ("/cl-launch" :systems (:cl-launch)
     :repo "git://common-lisp.net/projects/xcvb/cl-launch.git")
    ("/command-line-arguments" :systems (:command-line-arguments)
     :upstream "git://common-lisp.net/projects/qitab/command-line-arguments.git")
    ("/fare-utils" :systems (:fare-utils)
     :upstream "git://common-lisp.net/users/frideau/fare-utils.git")
    ("/fare-matcher" :systems (:fare-matcher :fare-quasiquote-readtable)
     :upstream "git://common-lisp.net/users/frideau/fare-matcher.git")
    ("/iolib" :systems (:iolib)
     :repo "git://common-lisp.net/projects/xcvb/iolib.git")
    ("/ironclad" :systems (:ironclad)
     :upstream "git://github.com/froydnj/ironclad.git")
    ("/rucksack" :systems (:rucksack)
     :repo "git://common-lisp.net/projects/xcvb/rucksack.git")
    ("/poiu" :systems (:poiu)
     :upstream "git://common-lisp.net/projects/qitab/poiu.git")
    ("/quux-iolib" :systems (:quux-iolib)
     :upstream "git://common-lisp.net/projects/xcvb/quux-iolib.git")
    ("/closer-mop" :systems (:closer-mop)
     :repo (:darcs "http://common-lisp.net/project/xcvb/darcs/closer-mop"))
    ("/named-readtables" :systems (:named-readtables)
     :repo (:darcs "http://common-lisp.net/project/editor-hints/darcs/named-readtables"))
    ("/trivial-features" :systems (:trivial-features)
     :repo (:darcs "http://common-lisp.net/project/xcvb/darcs/trivial-features"))
    ("/trivial-garbage" :systems (:trivial-garbage)
     :repo (:darcs "http://common-lisp.net/project/xcvb/darcs/trivial-garbage"))))

); eval-when
