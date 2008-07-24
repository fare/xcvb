(in-package :xcvb)



(defun converter-test1 ()
  (convert-asdf-system-to-xcvb :quux))

(defun run-converter-tests()
  (converter-test1))


;(run-converter-tests)