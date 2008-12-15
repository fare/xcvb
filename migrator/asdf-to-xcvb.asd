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
     (:file "asdf-converter" :depends-on ("pkgdcl"))))
