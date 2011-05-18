#+xcvb (module (:depends-on ("package")))

(in-package #:xcvb-unit-tests)

(defun all-xcvb-vars ()
  (remove-duplicates
   (loop :for pkg-name :in '(:xcvb :xcvb-driver)
     :for pkg = (find-package pkg-name) :append
     (loop :for sym :being :the :present-symbols :of pkg
       :when (and (boundp sym) (not (constantp sym)))
       :collect sym))))

(defun call-with-local-xcvb-vars (thunk)
  (let* ((vars (all-xcvb-vars))
         (vals (mapcar 'symbol-value vars)))
    (progv vars vals (funcall thunk))))

(defmacro with-local-xcvb-vars (() &body body)
  `(call-with-local-xcvb-vars (lambda () ,@body)))
