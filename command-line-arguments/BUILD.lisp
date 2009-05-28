;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008-2009 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original authors: Francois-Rene Rideau, Dan Weinreb              ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+xcvb
(module
 (:fullname "itasoftware.com/quux/command-line-arguments"
  :nicknames ("command-line-arguments")
  :supersedes-asdf ("command-line-arguments")
  :author ("Francois-Rene Rideau" "Dan Weinreb")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "small library to deal with command-line arguments"
  :long-description "A library to abstract away the parsing of Unix-style command-line arguments"
  :build-requires ((:asdf "cl-launch"))
  :depends-on ("command-line-arguments")
  :build-image nil))
