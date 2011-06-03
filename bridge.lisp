;;; XCVB bridge: call XCVB from ASDF

(in-package :asdf)

(defclass xcvb-build (system)
  ((build-name :initarg :build :reader %build-name :initform nil)))

(defgeneric build-name (b)
  (:method ((b xcvb-build))
    (or (%build-name b) (string-downcase (component-name b)))))

(defgeneric build-and-load-system (b)
  (:method ((b xcvb-build))
    (xcvb-driver:build-and-load (build-name b))))

(defmethod perform ((op compile-op) (b build))
  (declare (ignorable op))
  (build-and-load-system b))

(defmethod perform ((op load-op) (b build))
  (declare (ignorable op))
  (build-and-load-system b))
