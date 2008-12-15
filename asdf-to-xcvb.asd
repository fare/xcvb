;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under the MIT license.                   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Francois-Rene Rideau                            ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(asdf:defsystem :asdf-to-xcvb
    :author ("Spencer Brody" "Francois-Rene Rideau")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "a tool to migrate systems from ASDF to XCVB"
    :long-description "A tool to migrate existing Lisp software
from the legacy ASDF defsystem to the new XCVB builder."
    :depends-on (:asdf-dependency-grovel :xcvb)
    :components
    ((:file "pkgdcl")
     (:file "extensions" :depends-on ("pkgdcl"))
     (:file "compiler-options" :depends-on ("pkgdcl"))
     (:file "utilities" :depends-on ("pkgdcl" "compiler-options"))
     (:file "xcvb" :depends-on ("pkgdcl" "utilities" "extensions"))
     (:file "traverse" :depends-on ("pkgdcl" "xcvb"))
     (:file "asdf-extensions" :depends-on ("utilities"))
     (:file "makefile-generator" :depends-on
	    ("pkgdcl" "compiler-options" "utilities"
	     "xcvb" "traverse" "asdf-extensions"))
     (:file "asd-generator" :depends-on ("pkgdcl" "xcvb" "traverse"))
     (:file "asdf-converter" :depends-on ("pkgdcl" "xcvb"))))

(cl:pushnew :xcvb *features*)
