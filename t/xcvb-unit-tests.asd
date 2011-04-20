;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2009-2011 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original author: Peter Keller & Francois-Rene Rideau             ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:xcvb-unit-tests-asd
  (:use :cl :asdf))
(in-package #:xcvb-unit-tests-asd)

(asdf:defsystem :xcvb-unit-tests
  :author "Peter Keller & Francois-Rene Rideau"
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "Tests for XCVB"
  ;;:long-description ""
  :depends-on (#:xcvb #:hu.dwim.stefil)
  :components
  ((:file "package")
   (:file "master" :depends-on ("package"))))
