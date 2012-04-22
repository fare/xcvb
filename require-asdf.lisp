#+xcvb (module (:build-depends-on ()))

;; This module is intended to be used as the --setup argument to XCVB,
;; on Lisp implementations that provide a recent enough ASDF.

(in-package :cl-user)

;; We assume that your implementation comes with ASDF.
;; If not, complain to your vendor.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (unless (or #+asdf2 (asdf:version-satisfies (asdf:asdf-version) "2.000"))
    (error "XCVB requires ASDF2"))

  ;; Make sure you have the latest ASDF *before* you load any other system.
  ;; This matters when the implementation provides an ASDF earlier than 2.015.
  (handler-bind ((style-warning #'muffle-warning))
    (asdf:load-system :asdf))

  (unless (asdf:version-satisfies (asdf:asdf-version) "2.019")
    (error "XCVB requires ASDF 2.019 or later")))
