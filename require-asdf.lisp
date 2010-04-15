#+xcvb (module (:build-depends-on ()))

(in-package :cl-user)

(require :asdf)

(unless (or #+asdf2 (asdf:version-satisfies (asdf:asdf-version) "1.678"))
  (cerror "~
Your implementation provides an old ASDF - continue at your own risk.~%~
Next time you may have better luck depending directly on /asdf/asdf"))
