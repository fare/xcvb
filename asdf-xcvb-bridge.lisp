;;; XCVB bridge: call XCVB from ASDF

(in-package :cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (intern (string '#:use-build.xcvb) :asdf))

(defpackage :xcvb-bridge
  (:nicknames :xcvb-bridge)
  (:use :cl :asdf :xcvb-master)
  (:import-from :asdf #:use-build.xcvb))

(in-package :xcvb-bridge)

(defclass use-build.xcvb (system)
  ((build-name :initarg :build :reader system-xcvb-build-name :initform nil)))

(defun build-and-load-system (u)
  (build-and-load (or (system-xcvb-build-name u)
                      (component-name u))))

(defmethod perform ((op compile-op) (u use-build.xcvb))
  (declare (ignorable operation c))
  (build-and-load-system u))

(defmethod perform ((op load-op) (u use-build.xcvb))
  (declare (ignorable operation c))
  (build-and-load-system u))
