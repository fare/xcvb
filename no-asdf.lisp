#+xcvb (module ())
(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel)
  (let ((package (find-package :asdf)))
    #+sbcl (setf sb-ext:*module-provider-functions*
                 (delete-if (lambda (x) (and (symbolp x) (eq package (symbol-package x))))
                            sb-ext:*module-provider-functions*))
    (when package (delete-package package))))
