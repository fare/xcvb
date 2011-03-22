;;; XCVB bridge: call XCVB from ASDF

(in-package :cl)

(defpackage :xcvb-bridge
  (:nicknames :xcvbb)
  (:use :cl :asdf :xcvb-master)
  (:export #:build))

(in-package :xcvb-bridge)

(defclass build (system)
  ((name :initarg :build :reader %build-name :initform nil)))

(defgeneric build-name (b)
  (:method ((b build))
    (or (%build-name b) (component-name b))))

(defun object-directory ()
  (if (absolute-pathname-p *object-directory*)
      *object-directory*
      (asdf::resolve-location (list *user-cache* "xcvb-obj"))))

(defgeneric build-and-load-system (b)
  (:method ((b build))
    (build-and-load (build-name b) :object-directory (object-directory))))

(defmethod perform ((op compile-op) (b build))
  (declare (ignorable op))
  (build-and-load-system b))

(defmethod perform ((op load-op) (b build))
  (declare (ignorable op))
  (build-and-load-system b))
