;;; XCVB bridge: call XCVB from ASDF

(in-package :cl)

(defpackage :xcvb-bridge
  (:nicknames :xcvbb)
  (:use :cl :asdf :xcvb-master)
  (:export #:build))

(in-package :xcvb-bridge)

(defclass build (system)
  ((name :initarg :build :reader build-name :initform nil)))

(defgeneric build-and-load-system (b)
  (:method ((b build))
    (build-and-load (or (build-name b) (component-name b)))))

(defmethod perform ((op compile-op) (b build))
  (declare (ignorable op))
  (build-and-load-system b))

(defmethod perform ((op load-op) (b build))
  (declare (ignorable op))
  (build-and-load-system b))
