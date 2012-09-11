;;; -*- Mode: Lisp ; Syntax: ANSI-Common-Lisp -*-
;;; XCVB-Utils, a stable interface to utilities from XCVB.
;;;
;;; Free Software available under an MIT-style license.
;;; Copyright (c) 2012 - 2012, Francois-Rene Rideau
;;;

(defsystem :xcvb-utils
  :licence "MIT"
  :description "Utilities from XCVB"
  :long-description "Utilities from XCVB-Driver, repackaged"
  :depends-on (:xcvb-driver :alexandria :fare-utils :asdf-utils)
  :components
  ((:file "xcvb-utils")))
