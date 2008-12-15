(in-package :xcvb)

(defvar *lisp-implementation* :sbcl
  "Type of Lisp implementation for the target system")

(defvar *lisp-executable-pathname* nil
  "Path to the Lisp implementation to use for the target system")

;(defvar *lisp-executable-pathname* "/usr/local/sbcl/1.0.21.2.ita.1cfasl/x86_64-linux/sbcl")
;(defvar *lisp-executable-pathname* "/usr/local/ccl/working-0711-11351/lx86cl64")

(defvar *lisp-image-pathname* nil
  "What path to a Lisp image do we need invoke the target Lisp with?")

(defvar *lisp-options* nil
  "What options do we need invoke the target Lisp with?")

(defvar *use-cfasls* (eq *lisp-implementation* :sbcl)
  "Should we assume the target Lisp supports CFASL?")

