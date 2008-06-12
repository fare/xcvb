(in-package :cl-user)

(defun reload-xcvb ()
  (pushnew #p"/home/sbrody/xcvb/" asdf:*central-registry*)
  (pushnew #p"/ita/devel/qres/lisp/libs/alexandria/" asdf:*central-registry*)
  (asdf:operate 'asdf:load-op :xcvb-test))

(reload-xcvb)
(xcvb:run-tests)

(asdf:operate 'asdf:load-op :xcvb-test)
