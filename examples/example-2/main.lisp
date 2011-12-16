#+xcvb
(module
  (:depends-on
    ("lib-a/func-1" "lib-a/func-2" "lib-a/func-3"
     "lib-b/func-1" "lib-b/func-2" "lib-b/func-3"
     "package")))

(in-package :xcvb-example-2)

(defun main ()
  (format t "Example 2 main.~%")
  (lib-a/func-1)
  (lib-a/func-2)
  (lib-a/func-3)
  (lib-b/func-1)
  (lib-b/func-2)
  (lib-b/func-3))
