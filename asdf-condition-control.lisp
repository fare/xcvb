#+xcvb (module (:build-depends-on ("/asdf" "/xcvb/driver")))

(in-package :asdf)

(defmethod perform :around ((o compile-op) (c cl-source-file))
  (declare (ignorable o c))
  (xcvb-driver:with-controlled-compiler-conditions ()
    (call-next-method)))

(defmethod perform :around ((o load-op) (c cl-source-file))
  (declare (ignorable o c))
  (xcvb-driver:with-controlled-loader-conditions ()
    (call-next-method)))

(defmethod perform :around ((o load-source-op) (c cl-source-file))
  (declare (ignorable o c))
  (xcvb-driver:with-controlled-loader-conditions ()
    (call-next-method)))
