(in-package :xcvb)

(defvar *lisp-implementation-type* :sbcl
  "Type of Lisp implementation for the target system")

(defvar *lisp-executable-pathname* nil
  "Path to the Lisp implementation to use for the target system")

(defvar *lisp-image-pathname* nil
  "What path to a Lisp image do we need invoke the target Lisp with?")

(defvar *lisp-options* nil
  "What options do we need invoke the target Lisp with?")

(defvar *use-cfasls* (eq *lisp-implementation-type* :sbcl)
  "Should we assume the target Lisp supports CFASL?")
