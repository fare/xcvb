(in-package :xcvb)



#|(defun print-module (filename)
  (let ((in (open filename)))
    (format t "~s~%" (read in))
    (close in)))|#
#|(defun print-module (filename)
  (with-open-file (in filename)
    (let ((m (read in)))
      (close in)
      (format t "~s~%" m))))|#


(defun print-file (filename)
  (let ((in (open filename)))
    (loop for line = (read-line in nil)
          while line do (format t "~s~%" line))
    (close in)))



(defun get-module-from-file (filename)
  (with-open-file (in filename)
    (let ((form (read in)))
      (close in)
      (destructuring-bind (module-decl &rest rest) form
        (declare (ignore rest))
        (unless (eql module-decl 'xcvb:module)
          (error "Missing module declaration")))
      form)))

#|(defun print-module (filename)
  (format t "~s~%" (get-module-from-file filename)))|#


(defun parse-module (module)
  "do stuff"
  (destructuring-bind (module-decl &key name origin licence nickname description long-description compile-depends-on load-depends-on build-depends-on) module
    (declare (ignore module-decl))
    (make-instance 'concrete-module :name name :origin origin :licence licence :nickname nickname :description description :long-description long-description :compile-depends-on compile-depends-on :load-depends-on load-depends-on :build-depends-on build-depends-on)))

(defun print-module (module)
  "Prints out a module object"
  (format t "MODULE: ~%~Tname: ~s ~%~Torigin: ~s ~%~Tlicence: ~s ~%~Tnickname: ~s ~%~Tdescription: ~s ~%~Tlong-description: ~s ~%~Tcompile-depends-on: ~s ~%~Tload-depends-on: ~s ~%~Tbuild-depends-on: ~s~%" (name module) (origin module) (licence module) (nickname module) (description module) (long-description module) (compile-depends-on module) (load-depends-on module) (build-depends-on module)))


(defun test ()
  (print-module (parse-module (get-module-from-file "/home/sbrody/xcvb/test/ORIGIN.lisp"))))


;(print-module "/home/sbrody/xcvb/test/ORIGIN.lisp")


(test)

