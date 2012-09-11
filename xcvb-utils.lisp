;;;;; XCVB-Utils. Mix and match utilities from xcvb, asdf, alexandria, fare-utils

#+xcvb
(module
 (:build-depends-on ((:asdf "alexandria")
                     (:asdf "fare-utils")
                     (:asdf "asdf-utils")
                     "/xcvb/driver")))

(in-package :fare-utils)

(define-package-mix :xcvb-utils
  (:alexandria :xcvb-driver :asdf-utils :fare-utils))
