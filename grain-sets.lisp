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
     <map-empty-is-nil>
     <map-decons-from-first-key-value-drop>
     <map-divide/list-from-divide>
     <map-for-each-from-fold-left>
     <map-join/list-from-join>
     <map-map/2-from-fold-left-lookup-insert-drop>
     <map-size-from-fold-left>
     <map-update-key-from-lookup-insert-drop>)
  ()
  (:singleton)
  (:method base-interface () <fmim>)
  (:method encode-key (grain)
    (grain-ordinal grain))
  (:method decode-key (ordinal)
    (ordinal-grain ordinal)))

(defparameter <gm> <grain-map>)
