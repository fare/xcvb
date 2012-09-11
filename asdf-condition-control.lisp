#+xcvb (module (:build-depends-on ("/asdf" "/xcvb/driver")))

(in-package :asdf)

(defmethod perform :around ((o compile-op) (c cl-source-file))
  (declare (ignorable o c))
  (xcvb-driver:with-controlled-compiler-conditions ()
    (xcvb-driver:proclaim-optimization-settings)
    (call-next-method)))

(defmethod perform :around ((o load-op) (c cl-source-file))
  (declare (ignorable o c))
  (xcvb-driver:with-controlled-loader-conditions ()
    (xcvb-driver:proclaim-optimization-settings)
    (call-next-method)))

(defmethod perform :around ((o load-source-op) (c cl-source-file))
  (declare (ignorable o c))
  (xcvb-driver:with-controlled-loader-conditions ()
    (xcvb-driver:proclaim-optimization-settings)
    (call-next-method)))

;;; Most users of ASDF will want debugging ON.
;;; They can disable it if they want.
(setf xcvb-driver:*debugging* t)
