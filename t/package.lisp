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

#+xcvb
(module
 (:description "Unit test package for XCVB"))

(in-package #:cl)

(defpackage #:xcvb-unit-tests
  (:use :closer-common-lisp
        :xcvb-driver :command-line-arguments :xcvb-master
        :fare-matcher :fare-utils :interface :pure
        #+xcvb-farmer :quux-iolib #+xcvb-farmer :iolib.os
        :hu.dwim.stefil)

  (:import-from :xcvb
                #:run-program/process-output-stream
                #:slurp-stream-lines
                #:slurp-stream-string)
  (:export
   #:run-program/*))

(in-package #:xcvb-unit-tests)

;; We add a newline to the end of a string and return it. We do it in
;; this specific manner so that under unix and windows format will
;; choose the correct type of newline delimiters
(defun nl (str)
  (format nil "~A~%" str))

;; Sometimes we only want forms to be present when we've been compiled
;; on a unix machine of any kind
(defmacro using-unix (&body body)
  (or #+os-unix `(progn ,@body)))

;; Sometimes we only want forms to be present when we've been compiled
;; on a windows machine of any kind
(defmacro using-windows (&body body)
  (or #+os-windows `(progn ,@body)))

