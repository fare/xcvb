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
    :long-description "XCVB: an eXtensible Component Verifier and Builder for 
Lisp.  XCVB provides a scalable system to build large software in Lisp,
featuring deterministic separate compilation and enforced locally-declared 
dependencies."
    :components
    ((:file "pkgdcl")
     (:file "extensions" :depends-on ("pkgdcl"))
     (:file "compiler-options" :depends-on ("pkgdcl"))
     (:file "utilities" :depends-on ("pkgdcl" "compiler-options"))
     (:file "xcvb" :depends-on ("pkgdcl" "utilities" "extensions"))
     (:file "traverse" :depends-on ("pkgdcl" "xcvb"))
     (:file "asdf-extensions" :depends-on ("utilities"))
     (:file "makefile-generator" :depends-on ("pkgdcl" 
                                              "compiler-options"
                                              "utilities"
                                              "xcvb"
                                              "traverse"
                                              "asdf-extensions"))
     (:file "asd-generator" :depends-on ("pkgdcl" "xcvb" "traverse"))
     (:file "asdf-converter" :depends-on ("pkgdcl" "xcvb"))))
     

(cl:pushnew :xcvb *features*)
