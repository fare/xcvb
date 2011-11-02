;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008-2011 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original authors: Spencer Brody, Francois-Rene Rideau            ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :asdf)

(proclaim '(optimize (speed 2) (safety 3) (debug 3) (compilation-speed 0)))

(defsystem :xcvb-bootstrap
    :author ("Francois-Rene Rideau" "Peter Keller")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "Bootstrapping an XCVB binary from ASDF"
    :long-description "This system allows you to create an XCVB binary
using an automatically detected supported host implementation and
install it in a known location, from an arbitrary current Lisp implementation."
    :depends-on (:xcvb-driver)
    :components
    ((:file "lisp-invocation")
     (:file "bootstrap" :depends-on ("lisp-invocation"))))
