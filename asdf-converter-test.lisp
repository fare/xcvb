(in-package :xcvb)

(defun converter-test1 ()
  (add-module-to-file '(xcvb:module :name "test1") (pathname "/home/sbrody/xcvb/test/test-module-replace.lisp"))
  (add-module-to-file '(xcvb:module :name "test2") (pathname "/home/sbrody/xcvb/test/test-module-add.lisp"))
  (add-module-to-file '(xcvb:module :name "test3") (pathname "/home/sbrody/xcvb/test/test-module-create.lisp")))


(defun converter-test2 ()
  (convert-asdf-system-to-xcvb :quux))

(defun run-converter-tests()
  (converter-test2))


;(run-converter-tests)