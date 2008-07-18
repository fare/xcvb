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
    :author "Spencer Brody"
    :maintainer '("Spencer Brody" "Francois-Rene Rideau")
    :licence "MIT"
    :description      "XCVB"
    :long-description "XCVB: an eXtensible Component Verifier and Builder for Lisp
XCVB provides a scalable system to build large software in Lisp,
featuring deterministic separate compilation and enforced locally-declared dependencies."
    ;;:depends-on (:alexandria)
    :components
    ((:file "pkgdcl")
     (:file "extensions" :depends-on ("pkgdcl"))
     (:file "xcvb-compiler-options" :depends-on ("pkgdcl"))
     (:file "utilities" :depends-on ("pkgdcl" "xcvb-compiler-options"))
     (:file "xcvb" :depends-on ("pkgdcl" "utilities" "extensions"))
     (:file "asdf-extensions" :depends-on ("utilities"))
     (:file "makefile-generator" :depends-on ("pkgdcl" "xcvb-compiler-options" "utilities" "xcvb" "asdf-extensions"))
     (:file "asd-generator" :depends-on ("pkgdcl" "xcvb"))
     (:file "asdf-converter" :depends-on ("pkgdcl" "xcvb"))))
     

(cl:pushnew :xcvb *features*)
