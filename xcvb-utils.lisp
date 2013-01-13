;;;;; XCVB-Utils. Mix and match utilities from xcvb, asdf, alexandria, fare-utils

#+xcvb
(module
 (:build-depends-on ((:asdf "alexandria")
                     (:asdf "fare-utils")
                     (:asdf "asdf-utils")
                     "/xcvb/driver")))

(asdf/package:define-package :xcvb-utils
  (:mix :asdf/driver :fare-utils :alexandria)
  (:reexport :asdf/driver :fare-utils :alexandria))
