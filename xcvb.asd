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
    :depends-on (:cl-launch :asdf-dependency-grovel #|:iolib|#)
    :components
    ((:file "driver")
     (:file "asdf-extensions" :depends-on ("driver"))
     (:file "pkgdcl" :depends-on ("driver"))
     (:file "macros" :depends-on ("pkgdcl"))
     (:file "specials" :depends-on ("pkgdcl"))
     (:file "utilities" :depends-on ("pkgdcl" "macros" "specials"))
     (:file "lisp-invocation" :depends-on ("pkgdcl" "specials"))
     (:file "xcvb" :depends-on ("pkgdcl" "utilities" "extensions" "specials"))
     (:file "search-path" :depends-on ("xcvb"))
     (:file "traverse" :depends-on ("pkgdcl" "xcvb" "macros"))
     (:file "extensions" :depends-on ("pkgdcl"))
     (:file "makefile-generator" :depends-on
	    ("pkgdcl" "specials" "utilities"
	     "xcvb" "traverse" "lisp-invocation"))
     (:file "asd-generator" :depends-on ("pkgdcl" "xcvb" "traverse"))
     (:file "asdf-converter" :depends-on ("pkgdcl" "xcvb" "utilities"))
     (:file "main" :depends-on ("xcvb" "asdf-converter" "specials" "search-path"))))
