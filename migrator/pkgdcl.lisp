#+xcvb
(xcvb:module
  :description "package for ASDF to XCVB migrator")

(cl:defpackage :asdf-to-xcvb
  (:use :cl :xcvb :asdf)
  (:export
     #:asdf-systems-are-up-to-date-p
     #:convert-asdf-system-to-xcvb))
