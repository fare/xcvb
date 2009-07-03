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

(asdf:defsystem :xcvb
    :author ("Francois-Rene Rideau" "Spencer Brody")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "XCVB"
    :long-description "an eXtensible Component Verifier and Builder for Lisp.
XCVB provides a scalable system to build large software in Lisp, featuring
deterministic separate compilation and enforced locally-declared dependencies."
    :depends-on (:cl-launch :asdf-dependency-grovel :closer-mop
                            :command-line-arguments #|:iolib|#)
    :components
    ((:file "driver")
     (:file "pkgdcl" :depends-on ("driver"))
     (:file "macros" :depends-on ("pkgdcl"))
     (:file "utilities" :depends-on ("macros"))
     (:file "specials" :depends-on ("utilities"))
     (:file "logging" :depends-on ("specials"))
     (:file "grains" :depends-on ("utilities"))
     (:file "computations" :depends-on ("grains"))
     (:file "portablish-pathnames" :depends-on ("utilities"))
     (:file "registry" :depends-on ("portablish-pathnames" "grains"))
     (:file "lisp-invocation" :depends-on ("specials"))
     (:file "extract-target-properties" :depends-on ("specials" "lisp-invocation"))
     (:file "lisp-grain" :depends-on ("registry" "extract-target-properties"))
     (:file "names" :depends-on ("registry" "lisp-grain" "specials"))
     (:file "search-path" :depends-on ("registry" "specials" "portablish-pathnames"))
     (:file "dependencies-interpreter" :depends-on ("names" "specials" "computations"))
     (:file "static-backends" :depends-on
            ("specials" "lisp-grain" "dependencies-interpreter" "logging"))
     (:file "string-escape" :depends-on ("utilities"))
     (:file "makefile-backend" :depends-on ("static-backends" "string-escape" "computations"))

     ;;; These files are not meaningful anymore, need to be re-written in the new framework:
     ;;(:file "asd-generator" :depends-on (...)

     ;;; These files may need some work before we officially release
     (:file "asdf-converter" :depends-on ("lisp-grain"))
     (:file "main" :depends-on ("static-backends" "search-path" "computations"))))
