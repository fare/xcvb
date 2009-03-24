(in-package :xcvb)

(defun coerce-asdf-system-name (name)
  "This function take the name of an asdf-system, and
converts it to a string representation that can universally be used to refer to that system.
Modeled after the asdf function coerce-name"
  (string-downcase
   (typecase name
     (asdf:component (asdf:component-name name))
     (symbol (symbol-name name))
     (string name)
     (t (simply-error 'syntax-error "~@<invalid asdf system designator ~A~@:>" name)))))
