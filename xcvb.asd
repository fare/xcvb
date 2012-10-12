;;; -*- mode: lisp -*-

(in-package :asdf)
#-asdf2 (error "XCVB requires ASDF 2")

(let ((old-ver (asdf-version)))
  (load-system :asdf)
  (let ((min "2.21")
	(ver (asdf-version)))
    (unless (or (version-satisfies old-ver "2.014.8") ; first version to do magic upgrade
		(equal ver old-ver))
      (error "You must upgrade ASDF to your latest *before* you load XCVB~%~
		If you're trying to load XCVB at a REPL, try again, it should work."))
    (unless (and ver (version-satisfies ver min))
      (error "XCVB requires ASDF ~D or later, you only have ~D" min ver))))

(when (plusp (length (getenv "XCVB_FARMER")))
  (pushnew :xcvb-farmer *features*))

(pushnew :xcvb-using-asdf *features*)

#+sbcl
(map () 'require
     '(;; Actually used by XCVB
       :sb-grovel :sb-posix :sb-sprof
       ;; Used by SLIME
       :sb-cltl2 :sb-introspect :sb-bsd-sockets))

(proclaim '(optimize (speed 2) (safety 3) (debug 3) (compilation-speed 0)))

(defsystem :xcvb
    :author ("Francois-Rene Rideau" "Spencer Brody" "Joyce Chen")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "XCVB"
    :long-description "an eXtensible Component Verifier and Builder for Lisp.
XCVB provides a scalable system to build large software in Lisp, featuring
deterministic separate compilation and enforced locally-declared dependencies."
    :defsystem-depends-on (:asdf :xcvb-driver :xcvb-bootstrap
                                 :asdf-condition-control :asdf-encodings)
    :depends-on (:asdf :xcvb-driver :xcvb-utils :lambda-reader
                 :fare-mop :fare-memoization
                 :command-line-arguments
                 :asdf-dependency-grovel
                 :fare-matcher :fare-quasiquote-readtable
                 :ironclad :binascii :babel
		 :inferior-shell
                 :lisp-interface-library
		 #+clozure :single-threaded-ccl
                 #+xcvb-farmer :quux-iolib
                 #|#-clisp :rucksack|#)
    :components
    ((:file "version")
     (:file "pkgdcl" :depends-on ("version"))
     (:file "conditions" :depends-on ("pkgdcl"))
     (:file "specials" :depends-on ("pkgdcl"))
     (:file "macros" :depends-on ("pkgdcl"))
     (:file "profiling" :depends-on ("pkgdcl"))
     (:file "digest" :depends-on ("pkgdcl"))
     (:file "utilities" :depends-on ("macros"))
     (:file "logging" :depends-on ("specials"))
     (:file "commands" :depends-on ("specials" "macros"))
     (:file "string-escape" :depends-on ("utilities"))
     (:file "virtual-pathnames" :depends-on ("specials" "utilities"))
     (:file "grain-interface" :depends-on ("utilities" "conditions"))
     (:file "grain-sets" :depends-on ("grain-interface"))
     (:file "grain-registry" :depends-on ("grain-interface" "specials"))
     (:file "source-registry" :depends-on ("grain-registry" "commands"))
     (:file "computations" :depends-on ("grain-interface" "grain-registry"))
     (:file "manifest" :depends-on ("macros" "virtual-pathnames" "commands"))
     (:file "extract-target-properties" :depends-on ("specials" "string-escape" "grain-interface"))
     (:file "grain-implementation" :depends-on ("grain-registry" "extract-target-properties"))
     (:file "names" :depends-on ("grain-registry" "grain-interface"))
     (:file "normalize-dependency" :depends-on ("names" "grain-interface"))
     (:file "traversal" :depends-on ("names" "computations"))
     (:file "change-detection" :depends-on ("traversal"))
     (:file "dependencies-interpreter" :depends-on ("normalize-dependency" "traversal"))
     (:file "pathname-mappings" :depends-on ("specials" "grain-interface"))
     (:file "static-traversal" :depends-on ("grain-sets" "dependencies-interpreter"))
     (:file "external-commands" :depends-on ("specials" "utilities" "grain-interface"))
     (:file "target-lisp-commands" :depends-on ("specials" "utilities" "grain-interface" "external-commands"))
     (:file "run-program-backend" :depends-on ("profiling" "static-traversal" "target-lisp-commands"
					       "computations" "commands" "virtual-pathnames"))
     (:file "makefile-backend"
            :depends-on ("profiling" "static-traversal" "target-lisp-commands" "computations"
                         "extract-target-properties" "commands" "virtual-pathnames" "specials"))
     (:file "blaze-backend"
            :depends-on ("profiling" "static-traversal" "target-lisp-commands" "computations"
                         "extract-target-properties" "commands" "virtual-pathnames" "specials"))
     (:file "simplifying-traversal" :depends-on ("traversal" "dependencies-interpreter"))
     (:file "list-files" :depends-on ("simplifying-traversal" "commands"))
     (:file "asdf-backend" :depends-on ("simplifying-traversal" "logging" "commands"))
     (:file "ne-makefile-backend" :depends-on ("commands" "makefile-backend"
                                               "asdf-backend" "simplifying-traversal"))
     (:file "asdf-converter" :depends-on ("commands" "grain-interface" "source-registry"))
     (:file "self-upgrade" :depends-on ("commands"))
     (:file "slave" :depends-on ("commands"))
     #+xcvb-farmer
     (:file "farmer"
            :depends-on ("profiling" "commands" "target-lisp-commands" "external-commands"
                                     "grain-interface" "dependencies-interpreter"))
     (:file "cffi-grovel-support" :depends-on
            ("makefile-backend" "static-traversal" "computations" "target-lisp-commands"
                                "grain-implementation" "asdf-backend" "dependencies-interpreter"))
     (:file "main" :depends-on ("commands"))))

(defmethod perform ((op test-op) (c (eql (find-system :xcvb))))
  (asdf:load-system :xcvb-test)
  (xcvb-driver:call :xcvb-test :unit-tests)
  (xcvb-driver:call :xcvb-test :validate-xcvb-dir-all-lisps))
