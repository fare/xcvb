#+xcvb (module (:depends-on ("lib-a/package" "lib-b/package")))

(cl:defpackage :xcvb-example-2
  (:use :common-lisp :lib-a :lib-b))
