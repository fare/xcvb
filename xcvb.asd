;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008-2009 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original authors: Spencer Brody, Francois-Rene Rideau            ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+sbcl (require :sb-posix)
#+sbcl (require :sb-sprof)

(proclaim '(optimize (speed 2) (safety 3) (debug 3) (compilation-speed 0)))

(asdf:defsystem :xcvb
    :author ("Francois-Rene Rideau" "Spencer Brody" "Joyce Chen")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "XCVB"
    :long-description "an eXtensible Component Verifier and Builder for Lisp.
XCVB provides a scalable system to build large software in Lisp, featuring
deterministic separate compilation and enforced locally-declared dependencies."
    :depends-on (:cl-launch
                 :xcvb-driver :xcvb-master :command-line-arguments
                 :asdf-dependency-grovel :closer-mop
                 #|:iolib|#)
    :components
    ((:file "pkgdcl")
     (:file "conditions" :depends-on ("pkgdcl"))
     (:file "specials" :depends-on ("pkgdcl"))
     (:file "macros" :depends-on ("pkgdcl"))
     (:file "profiling" :depends-on ("pkgdcl"))
     (:file "utilities" :depends-on ("macros"))
     (:file "manifest" :depends-on ("macros"))
     (:file "logging" :depends-on ("specials"))
     (:file "grain-interface" :depends-on ("utilities" "conditions"))
     (:file "portablish-pathnames" :depends-on ("utilities"))
     (:file "registry" :depends-on ("portablish-pathnames" "grain-interface" "specials"))
     (:file "computations" :depends-on ("grain-interface" "registry" "specials"))
     (:file "lisp-invocation" :depends-on ("specials"))
     (:file "string-escape" :depends-on ("utilities"))
     (:file "extract-target-properties" :depends-on ("string-escape" "lisp-invocation"))
     (:file "grain-implementation" :depends-on ("registry" "extract-target-properties"))
     (:file "names" :depends-on ("registry" "grain-implementation" "specials"))
     (:file "search-path" :depends-on ("registry" "specials" "portablish-pathnames"))
     (:file "normalize-dependency" :depends-on ("names" "specials" "grain-interface"))
     (:file "traversal" :depends-on ("names" "specials" "computations"))
     (:file "dependencies-interpreter" :depends-on ("normalize-dependency" "traversal"))
     (:file "static-traversal" :depends-on
            ("specials" "grain-implementation" "dependencies-interpreter" "logging"))
     (:file "makefile-backend" :depends-on ("profiling" "static-traversal" "string-escape" "computations"))
     (:file "simplifying-traversal" :depends-on ("traversal" "dependencies-interpreter"))
     (:file "asdf-backend" :depends-on ("simplifying-traversal" "logging"))
     (:file "ne-makefile-backend" :depends-on ("specials" "makefile-backend"
                                               "asdf-backend" "simplifying-traversal"))
     (:file "asdf-converter" :depends-on ("grain-implementation"))
     (:file "slave" :depends-on ("pkgdcl"))
     (:file "farmer" :depends-on ("specials" "grain-implementation" "dependencies-interpreter"))
     (:file "main" :depends-on ("static-traversal" "search-path" "computations"))
     (:file "version" :depends-on ("specials"))))
