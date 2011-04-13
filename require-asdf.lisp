#+xcvb (module (:build-depends-on ()))

;; This module is intended to be used as the --setup argument to XCVB,
;; on Lisp implementations that provide a recent enough ASDF.

(in-package :cl-user)

(require :asdf)
(unless (or #+asdf2 (asdf:version-satisfies (asdf:asdf-version) "2.000"))
  (error "XCVB requires ASDF2"))

;; Make sure you have the latest ASDF *before* you load any other system
(asdf:load-system :asdf)
