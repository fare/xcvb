;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2009 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Francois-Rene Rideau                            ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defparameter cl-user::*xcvb-test-pathname* cl:*load-truename*
  "Path to the XCVB test directory")

(asdf:defsystem :xcvb-test
    :author "Francois-Rene Rideau"
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "Tests for XCVB"
    :long-description ""
    :depends-on (:xcvb)
    :components
    ((:module "test"
              :components
              ((:file "utilities")))))
