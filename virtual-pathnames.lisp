;;;;; Mapping virtual pathnames to actual pathnames
#+xcvb (module (:depends-on ("specials")))

(in-package :xcvb)


;;;;; Define a little-language for pathnames.


;;;;; Should we register pathnames to make sure there is no aliasing? meh...
#|
(defparameter *virtual-pathnames*
  (make-hash-table :test 'equal)
  "A registry of visited virtual pathnames,
mapping actual pathnames back to virtual pathnames.")

(defun registered-pathname (p)
  (gethash p *virtual-pathnames*))

(defun (setf registered-pathname) (vp p)
  (setf (gethash p *virtual-pathnames*) vp))
|#
