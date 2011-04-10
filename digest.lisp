#+xcvb (module (:depends-on ("macros" "virtual-pathnames")))

(in-package :xcvb)

(defgeneric digest (x))

(defmethod digest ((x string))
  (digest (babel:string-to-octets x :encoding :utf8)))

(defmethod digest ((x vector))
  (unless (typep x '(simple-array (unsigned-byte 8) (*)))
    (error "Can only digest ub8 vectors and strings"))
  (tthsum x))

(defmethod digest ((x cons))
  (digest (write-to-string x)))
