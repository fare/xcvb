#+xcvb (module (:build-depends-on ()))

;; This module is intended to be used as the --setup argument to XCVB,
;; on Lisp implementations that provide a recent enough ASDF.

(in-package :cl-user)

(require :asdf)

(unless (or #+asdf2 (asdf:version-satisfies (asdf:asdf-version) "1.703"))
  (cerror "~
Your implementation provides an old ASDF - continue at your own risk.~%~
Next time you may have better luck depending directly on /asdf/asdf"))
