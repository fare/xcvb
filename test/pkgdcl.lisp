#+xcvb
(module
 (:description "package for XCVB-test"))

(cl:defpackage :xcvb-test
  (:use :cl :xcvb-driver :closer-mop)
  (:import-from :xcvb-driver #:quit)
  (:import-from :cl-launch #:DBG)
  (:import-from :xcvb . #.(loop :for s :being :the :present-symbols :of :xcvb :collect s)))
