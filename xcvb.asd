;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under the MIT license.                   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Spencer Brody                                   ;;;
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
    :components
    ((:file "driver")
     (:file "pkgdcl" :depends-on ("driver"))
     (:file "compiler-options" :depends-on ("pkgdcl"))
     (:file "macros" :depends-on ("pkgdcl"))
     (:file "utilities" :depends-on ("pkgdcl" "compiler-options"))
     (:file "xcvb" :depends-on ("pkgdcl" "utilities" "extensions"))
     (:file "traverse" :depends-on ("pkgdcl" "xcvb" "macros"))
     (:file "extensions" :depends-on ("pkgdcl"))
     (:file "makefile-generator" :depends-on
	    ("pkgdcl" "compiler-options" "utilities"
	     "xcvb" "traverse"))
     (:file "asd-generator" :depends-on ("pkgdcl" "xcvb" "traverse")))
