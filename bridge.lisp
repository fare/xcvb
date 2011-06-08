;;; XCVB bridge: call XCVB from ASDF

(in-package :asdf)

(defclass xcvb-build (system)
  ((build-name :initarg :build :reader %build-name :initform nil)))

(defgeneric build-name (b)
  (:method ((b xcvb-build))
    (or (%build-name b) (string-downcase (component-name b)))))

(defmethod perform ((op compile-op) (b xcvb-build))
  (declare (ignorable op))
  (xcvb-driver:build-in-slave (build-name b)))

(defmethod perform ((op load-op) (b xcvb-build))
  (declare (ignorable op))
  (xcvb-driver:build-and-load (build-name b)))

(defmethod traverse ((op compile-op) (b xcvb-build))
  (list (cons op b)))

(defmethod traverse ((op load-op) (b xcvb-build))
  (list (cons op b)))
