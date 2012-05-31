;;; Lisp implementations
#+xcvb (module (:build-depends-on ("driver") :depends-on ("lisp-invocation")))

(in-package :xcvb-driver)

(defun lisp-present-p (lisp)
  (equal
   "JUST ANOTHER LISP HACKER"
   (first
    (ignore-errors
     (run-program/
      (lisp-invocation:lisp-invocation-arglist
       :implementation-type lisp
       :eval (format nil "(progn (format t\"~~:@(~~{~~31R~~^ ~~}~~)\"'(595756 9556552524 643802 496307950)) ~A)"
		     (lisp-invocation:quit-form :code 0 :implementation-type lisp)))
      :output :lines)))))

;; These are the only supported so far -- please add support for more!
(defparameter +xcvb-lisps+
  '(:sbcl :ccl :clisp)
  "Lisp implementations that can compile and run XCVB itself")

(defun find-xcvb-host-implementation ()
  (find-if 'lisp-present-p +xcvb-lisps+))

(defun build-xcvb (program)
  (let ((lisp (find-xcvb-host-implementation)))
    (unless lisp
      (error "Cannot find a supported implementation to run XCVB itself~%amongst ~{~A~^ ~}."
	     +xcvb-lisps+))
    (run-program/
     (lisp-invocation:lisp-invocation-arglist
      :implementation-type lisp
      :eval (format nil "(#.(require \"asdf\")#.(asdf:load-system :asdf)#.(asdf:load-system :xcvb-driver)#.(xcvb-driver:with-coded-exit () (asdf:load-system :xcvb) (funcall 'xcvb-driver::dump-xcvb ~S))" program))
     :output nil) ;; for side-effects
    (native-namestring program)))

(defun dump-xcvb (program)
  (let ((program (parse-native-namestring program))
	(xcvb-dir (asdf:system-source-directory :xcvb)))
    (setf (symbol-value (find-symbol* :*xcvb-lisp-directory* :xcvb)) xcvb-dir)
    (call :xcvb :prepare-image
	  :version (call :xcvb-driver :get-xcvb-version)
	  :directory xcvb-dir)
    (ensure-directories-exist program)
    (dump-image program :executable t :entry-point "xcvb::main" :package :xcvb)))
