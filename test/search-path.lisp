(in-package :xcvb)

(defparameter *test-dir*
  (pathname-directory-pathname cl-user::*xcvb-test-pathname*))

(defparameter *mock-directory*
  (subpathname *test-dir* "test/mock/"))

(defparameter *mock-d-directory*
  (subpathname *test-dir* "test/mock/d/"))


(reset-variables)

(setf *search-path* (list *mock-d-directory* *mock-directory* *test-dir*))

(search-search-path)

#+debug (format t "SP: ~W~%" *search-path*)
(finalize-search-path)

#+debug (format t "SP: ~W~%" *search-path*)
(setf *grains* (make-hash-table :test 'equal))
(search-search-path)

#+debug (format t "~W~%" (hash-table->alist *grains*))

(assert (typep (registered-grain "/xcvb/test/b") 'build-registry-conflict))
(assert (equal (fullname (grain-parent (registered-grain "/xcvb/test/c/x"))) "/xcvb/test/c"))
(assert (typep (registered-grain "/xcvb/test/d") 'build-grain))
