;;; -*- Lisp -*-
(in-package :cl-user)

#-cl-launch
(load (make-pathname :name "launcher" :type "lisp" :defaults *load-truename*))

(asdf:defsystem :cl-launch
  :components ((:file "launcher")))
