;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008-2010 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original authors: Spencer Brody, Francois-Rene Rideau            ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+sbcl
(progn
  ;;; Actually used by XCVB
  (require :sb-posix)
  (require :sb-sprof)
  ;;; Used by SLIME
  (require :sb-grovel)
  (require :sb-cltl2)
  (require :sb-introspect)
  (require :sb-bsd-sockets))

(proclaim '(optimize (speed 2) (safety 3) (debug 3) (compilation-speed 0)))

(asdf:defsystem :xcvb
    :author ("Francois-Rene Rideau" "Spencer Brody" "Joyce Chen")
    :maintainer "Francois-Rene Rideau"
    :licence "MIT"
    :description "XCVB"
    :long-description "an eXtensible Component Verifier and Builder for Lisp.
XCVB provides a scalable system to build large software in Lisp, featuring
deterministic separate compilation and enforced locally-declared dependencies."
    :depends-on (:asdf
                 :xcvb-driver :xcvb-master
                 :fare-utils :command-line-arguments
                 :asdf-dependency-grovel :closer-mop
                 #|:iolib|#)
    :components
    ((:file "pkgdcl")
     (:file "conditions" :depends-on ("pkgdcl"))
     (:file "specials" :depends-on ("pkgdcl"))
     (:file "macros" :depends-on ("pkgdcl"))
     (:file "profiling" :depends-on ("pkgdcl"))
     (:file "utilities" :depends-on ("macros"))
     (:file "logging" :depends-on ("specials"))
     (:file "grain-interface" :depends-on ("utilities" "conditions")) ;;; FIX THIS FILE AND BELOW
     (:file "registry" :depends-on ("grain-interface" "specials"))
     (:file "search-path" :depends-on ("registry"))
     (:file "computations" :depends-on ("grain-interface" "registry" "specials"))
     (:file "lisp-invocation" :depends-on ("specials"))
     (:file "string-escape" :depends-on ("utilities"))
     (:file "manifest" :depends-on ("macros"))
     (:file "extract-target-properties" :depends-on ("string-escape" "lisp-invocation"))
     (:file "grain-implementation" :depends-on ("registry" "extract-target-properties"))
     (:file "names" :depends-on ("registry" "grain-interface" "specials"))
     (:file "virtual-pathnames" :depends-on ("specials" "utilities"))
     (:file "normalize-dependency" :depends-on ("names" "specials" "grain-interface"))
     (:file "traversal" :depends-on ("names" "specials" "computations"))
     (:file "dependencies-interpreter" :depends-on ("normalize-dependency" "traversal"))
     (:file "static-traversal" :depends-on
            ("specials" "grain-interface" "dependencies-interpreter" "logging"))
     (:file "driver-commands" :depends-on ("specials" "utilities" "grain-interface"))
     (:file "makefile-backend" :depends-on ("profiling" "static-traversal" "driver-commands"
					    "computations"))
     (:file "simplifying-traversal" :depends-on ("traversal" "dependencies-interpreter"))
     (:file "list-files" :depends-on ("simplifying-traversal"))
     (:file "asdf-backend" :depends-on ("simplifying-traversal" "logging"))
     (:file "ne-makefile-backend" :depends-on ("specials" "makefile-backend"
                                               "asdf-backend" "simplifying-traversal"))
     (:file "asdf-converter" :depends-on ("specials" "grain-interface"))
     (:file "slave" :depends-on ("pkgdcl"))
     (:file "farmer" :depends-on ("profiling" "specials"
                                  "grain-interface" "dependencies-interpreter"))
     (:file "main" :depends-on ("static-traversal" "search-path" "computations"))
     (:file "cffi-grovel-support" :depends-on
            ("makefile-backend" "static-traversal" "computations" "driver-commands"))
     (:file "version" :depends-on ("specials"))))
