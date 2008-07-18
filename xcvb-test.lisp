(in-package :xcvb)

(defgeneric write-graph-to-file (filestream node tab)
  (:documentation "Writes a representation of the dependency graph to a file"))

(defmethod write-graph-to-file (filestream (node dependency-graph-node) tab)
  (format filestream "~a~a~%" (generate-tab tab) (fullname node)))

(defmethod write-graph-to-file (filestream (node dependency-graph-node-with-dependencies) tab)
  (when (compile-dependencies node)
    (format filestream "~a~a::compile-dependencies:~%" (generate-tab tab) (fullname node))
    (mapcar (lambda (x) (write-graph-to-file filestream x (+ tab 1))) (compile-dependencies node)))
  (when (load-dependencies node)
    (format filestream "~a~a::load-dependencies:~%" (generate-tab tab) (fullname node))
    (mapcar (lambda (x) (write-graph-to-file filestream x (+ tab 1))) (load-dependencies node)))
  (unless (or (load-dependencies node) (compile-dependencies node))
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
    (write-graph-to-file out (create-dump-image-graph "/home/sbrody/xcvb/test/IMAGE.img" "/home/sbrody/xcvb/test/BUILD.lisp") 0)))

(defun test2 ()
  (write-makefile "/home/sbrody/xcvb/test/BUILD.lisp" "/home/sbrody/xcvb/test/Makefile.xcvb" :image-dump))

(defun test3 ()
  (write-asd-file "/home/sbrody/xcvb/test/BUILD.lisp" "/home/sbrody/xcvb/test/ASDFILE.asd"))

(defun test4 ()
  (format t "~%~{~a~%~}" (mapcar #'fullname (traverse (create-dump-image-graph "/home/sbrody/xcvb/test/IMAGE.img" "/home/sbrody/xcvb/test/BUILD.lisp") :all))))

(defun run-tests ()
  ;(test1)
  (test2)
  ;(test3)
  (print-modules))

(defun compile-quux ()
  (format T "Writing quux Makefile...")
  (write-makefile "/ita/devel/qres/lisp/quux/BUILD.lisp" "/ita/devel/qres/lisp/quux/Makefile.xcvb" :image-dump)
  ;(format T "done.~%Writing quux asd file...")
  ;(with-open-file (out "/home/sbrody/xcvb/test/quux/quux.asd" :direction :output :if-exists :supersede)
  ;  (write-asdf-file out (build-dependency-graph "/ita/devel/qres/lisp/quux/BUILD.lisp" :build-for-asdf T) (make-hash-table :test #'equal)))
  ;(format T "done~%")
  )

(defun print-module (module)
  "Prints out a module object"
  (format t "MODULE: ~%~Tname: ~s ~%~Tfullname: ~s ~%~Tlicence: ~s ~%~Tnickname: ~s ~%~Tdescription: ~s ~%~Tlong-description: ~s ~%~Tcompile-depends-on: ~s ~%~Tload-depends-on: ~s ~%~Tfilepath: ~s ~%" (name module) (fullname module) (licence module) (nickname module) (description module) (long-description module) (compile-depends-on module) (load-depends-on module) (filepath module)))

(defun print-modules ()
  (loop for module being the hash-values in *module-map* using (hash-key key)
        do (format t "KEY: ~a~%" key) 
           (print-module module)))
