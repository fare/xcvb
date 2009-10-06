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
     (:file "digest" :depends-on ("macros"))
     (:file "logging" :depends-on ("specials"))
     (:file "grains" :depends-on ("utilities" "conditions"))
     (:file "portablish-pathnames" :depends-on ("utilities"))
     (:file "registry" :depends-on ("portablish-pathnames" "grains"))
     (:file "computations" :depends-on ("grains" "registry"))
     (:file "lisp-invocation" :depends-on ("specials"))
     (:file "string-escape" :depends-on ("utilities"))
     (:file "extract-target-properties" :depends-on ("string-escape" "lisp-invocation"))
     (:file "lisp-grain" :depends-on ("registry" "extract-target-properties"))
     (:file "names" :depends-on ("registry" "lisp-grain" "specials"))
     (:file "search-path" :depends-on ("registry" "specials" "portablish-pathnames"))
     (:file "normalize-dependency" :depends-on ("names" "specials" "grains"))
     (:file "traversal" :depends-on ("names" "specials" "computations"))
     (:file "dependencies-interpreter" :depends-on ("normalize-dependency" "traversal"))
     (:file "static-backends" :depends-on
            ("specials" "lisp-grain" "dependencies-interpreter" "logging"))
     (:file "makefile-backend" :depends-on ("profiling" "static-backends" "string-escape" "computations"))
     (:file "simplifying-traversal" :depends-on ("traversal" "dependencies-interpreter"))
     (:file "asdf-backend" :depends-on ("simplifying-traversal" "logging"))
     (:file "ne-makefile-backend" :depends-on ("specials" "makefile-backend"
                                               "asdf-backend" "simplifying-traversal"))
     (:file "asdf-converter" :depends-on ("lisp-grain"))
     (:file "slave" :depends-on ("pkgdcl"))
     (:file "main" :depends-on ("static-backends" "search-path" "computations"))
     (:file "version" :depends-on ("specials"))))
