(in-package :xcvb)

(defgeneric write-graph-to-file (filestream node tab)
  (:documentation "Writes a representation of the dependency graph to a file"))

(defmethod write-graph-to-file (filestream (node dependency-graph-node) tab)
  (format filestream "~a~a~%" (generate-tab tab) (fullname node)))

(defmethod write-graph-to-file (filestream (node dependency-graph-node-with-dependencies) tab) 
  (if (dependencies node)
    (progn
      (format filestream "~a~a::dependencies:~%" (generate-tab tab) (fullname node))
      (mapcar (lambda (x) (write-graph-to-file filestream x (+ tab 1))) (dependencies node)))
    (format filestream "~a~a~%" (generate-tab tab) (fullname node))))

(defmethod write-graph-to-file (filestream (node image-dump-node) tab)
  (format filestream "~a~a~%" (generate-tab tab) (target node))
  (write-graph-to-file filestream (lisp-image node) (+ tab 1)))


(defun generate-tab (tab)
  (let ((string ""))
    (dotimes (x tab string)
      (setf string (concatenate 'string string " ")))))


(defun test1 ()
  (with-open-file (out "/home/sbrody/xcvb/test/dependency-graph-output.txt" :direction :output :if-exists :supersede)
    (write-graph-to-file out (build-dump-image-graph "/home/sbrody/xcvb/test/IMAGE.img" "/home/sbrody/xcvb/test/BUILD.lisp") 0)))

(defun test2 ()
  (with-open-file (out "/home/sbrody/xcvb/test/Makefile.xcvb" :direction :output :if-exists :supersede)
    (write-makefile out (build-dump-image-graph "/home/sbrody/xcvb/test/IMAGE.img" "/home/sbrody/xcvb/test/BUILD.lisp") (make-hash-table :test #'equal))))

(defun test3 ()
  (with-open-file (out "/home/sbrody/xcvb/test/ASDFILE.asd" :direction :output :if-exists :supersede)
    (write-asdf-file out (build-asdf-graph "/home/sbrody/xcvb/test/BUILD.lisp") (make-hash-table :test #'equal) nil)))

(defun run-tests ()
  (test1)
  (test2)
  (test3)
  (print-modules))

(defun print-module (module)
  "Prints out a module object"
  (format t "MODULE: ~%~Tname: ~s ~%~Tfullname: ~s ~%~Torigin: ~s ~%~Tlicence: ~s ~%~Tnickname: ~s ~%~Tdescription: ~s ~%~Tlong-description: ~s ~%~Tcompile-depends-on: ~s ~%~Tload-depends-on: ~s ~%~Tbuild-depends-on: ~s ~%~Tfilepath: ~s ~%" (name module) (fullname module) (origin module) (licence module) (nickname module) (description module) (long-description module) (compile-depends-on module) (load-depends-on module) (build-depends-on module) (filepath module)))

(defun print-modules ()
  (loop for module being the hash-values in *module-map* using (hash-key key)
        do (format t "KEY: ~a~%" key) 
           (print-module module)))

;;(print-modules)




(defgeneric print-graph (node tab)
  (:documentation "Prints the dependency graph - for testing purposes only"))

(defmethod print-graph ((node dependency-graph-node) tab)
  (if (dependencies node)
    (progn
      (format t "~a~a::dependencies:~%" (generate-tab tab) (fullname node))
      (format t "~a" (mapcar (lambda (x) (print-graph x (+ tab 1))) (dependencies node))))
    (format t "~a~a~%" (generate-tab tab) (fullname node))))





;(run-tests)
