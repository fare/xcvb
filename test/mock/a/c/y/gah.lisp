#+xcvb (module ())

(defun optional-and-rest (x &optional y &rest r)
  (list x y r))
