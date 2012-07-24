#+xcvb (module (:depends-on ("grain-interface")))

(in-package :xcvb)

(defvar *ordered-grains*
  (make-array '(0) :adjustable t :fill-pointer 0)
  "Array mapping numbers to grains, so we can associate them and
have compact FMIM set representations. Each grain should conversely have a number
mapping back to this array...")

(defmethod grain-ordinal :before ((grain grain))
  (unless (slot-boundp grain 'ordinal)
    (setf (grain-ordinal grain)
          (vector-push-extend grain *ordered-grains*))))

(defun check-grain-ordinal (ordinal)
  (check-type ordinal fixnum)
  (assert (< ordinal (fill-pointer *ordered-grains*))))

(defun ordinal-grain (ordinal)
  (check-grain-ordinal ordinal)
  (aref *ordered-grains* ordinal))

(define-interface <grain-map>
    (<encoded-key-map>
     map-simple-empty map-simple-decons map-simple-update-key
     map-simple-map/2 map-simple-join/list map-size-from-fold-left
     map-for-each-from-fold-left map-divide/list-from-divide)
  ())

(defmethod base-interface ((i <grain-map>))
  <fmim>)
(defmethod encode-key ((i <grain-map>) grain)
  (grain-ordinal grain))
(defmethod decode-key ((i <grain-map>) ordinal)
  (ordinal-grain ordinal))

(defparameter <gm> (make-instance '<grain-map>))
