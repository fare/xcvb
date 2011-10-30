#+xcvb
(module (:depends-on
         ("pkgdcl"
          (:when (:featurep :sbcl)
            (:require :sb-sprof)))))

(in-package :xcvb)

#+sbcl
(defun call-with-maybe-profiling (maybe thunk)
  (if maybe
    (sb-sprof:with-profiling (:max-samples 10000 :report :graph :loop nil)
      (funcall thunk))
    (funcall thunk)))

#-sbcl
(defun call-with-maybe-profiling (maybe thunk)
  (declare (ignore maybe))
  (funcall thunk))

(defmacro with-maybe-profiling ((&optional (maybe *profiling*)) &body body)
  `(call-with-maybe-profiling ,maybe (lambda () ,@body)))
