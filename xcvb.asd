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
     (:file "xcvb" :depends-on ("pkgdcl"))
     (:file "xcvb-compiler-options" :depends-on ("pkgdcl"))
     (:file "asdf-extensions")
     (:file "makefile-generator" :depends-on ("pkgdcl" "xcvb" "xcvb-compiler-options" "asdf-extensions"))
     (:file "asd-generator" :depends-on ("pkgdcl" "xcvb"))
     (:file "asdf-converter" :depends-on ("pkgdcl" "xcvb"))))
     

(cl:pushnew :xcvb *features*)
