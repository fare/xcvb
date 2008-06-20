(in-package :xcvb)

(defun converter-test1 ()
  (add-module-to-file '(xcvb:module :name "test1") (pathname "/home/sbrody/xcvb/test/test-module-replace.lisp"))
  (add-module-to-file '(xcvb:module :name "test2") (pathname "/home/sbrody/xcvb/test/test-module-add.lisp")))

(defun run-converter-tests()
  (converter-test1))


;(run-converter-tests)