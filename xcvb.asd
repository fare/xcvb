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
    :author ("Spencer Brody" "Francois-Rene Rideau")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "XCVB"
    :long-description "an eXtensible Component Verifier and Builder for Lisp.
XCVB provides a scalable system to build large software in Lisp, featuring
deterministic separate compilation and enforced locally-declared dependencies."
    :depends-on (:cl-launch :asdf-dependency-grovel :closer-mop #|:iolib|#)
    :components
    ((:file "driver")
     (:file "asdf-extensions" :depends-on ("driver"))
     (:file "memoization")
     (:file "pkgdcl" :depends-on ("driver" "memoization"))
     (:file "macros" :depends-on ("pkgdcl"))
     (:file "utilities" :depends-on ("macros" "memoization"))
     (:file "specials" :depends-on ("utilities"))
     (:file "grains" :depends-on ("utilities"))
     (:file "lisp-invocation" :depends-on ("specials"))
     (:file "computations" :depends-on ("grains" "lisp-invocation"))
     (:file "portablish-pathnames" :depends-on ("utilities"))
     (:file "names" :depends-on ("portablish-pathnames" "grains"))
     (:file "registry" :depends-on ("names"))
     (:file "search-path" :depends-on ("registry" "specials"))
     (:file "lisp-grain" :depends-on ("registry"))
     (:file "string-escape" :depends-on ("utilities"))
     (:file "asdf-dependencies" :depends-on ("pkgdcl"))
     (:file "static-backends" :depends-on ("asdf-dependencies" "string-escape" "lisp-grain"))

     ;;; These files need to be re-written:
     (:file "extensions" :depends-on ("pkgdcl"))
     (:file "xcvb" :depends-on ("lisp-grain" "extensions"))
     (:file "traverse" :depends-on ("xcvb" "macros"))
     (:file "makefile-generator" :depends-on
	    ("xcvb" "traverse" "lisp-invocation" "string-escape"))
     (:file "asd-generator" :depends-on ("xcvb" "traverse"))
     (:file "asdf-converter" :depends-on ("xcvb"))
     (:file "main" :depends-on ("xcvb" "asdf-converter" "search-path"))))
