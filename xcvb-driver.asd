;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2009-2011 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original authors: Spencer Brody, Francois-Rene Rideau            ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem :xcvb-driver
    :author ("Francois-Rene Rideau" "Spencer Brody")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "XCVB Driver"
    :long-description
    "a minimal driver to be loaded in target system
    when building software with XCVB"
    :depends-on () ; The very idea of it is that it should be standalone, without dependencies
    :components
    ((:file "driver")))
