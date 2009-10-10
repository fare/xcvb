#+xcvb
(module
 (:depends-on
  ("macros"
   (:when (:featurep :sbcl) (:require :sb-posix)))))

(in-package :xcvb)

(defun mkfifo (pathname mode)
  #+sbcl (sb-posix:mkfifo pathname mode)
  #+clozure (ccl::with-filename-cstrs ((p pathname))(#_mkfifo p mode))
  #+clisp (LINUX:mkfifo pathname mode)
  #-(or sbcl clozure clisp) (error "mkfifo not implemented for your Lisp"))
