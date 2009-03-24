(in-package :xcvb-test)

(defparameter *test-dir*
  (pathname-directory-pathname cl-user::*xcvb-test-pathname*))

(defparameter *mock-directory*
  (subpathname *test-dir* "test/mock/"))

(defparameter *mock-d-directory*
  (subpathname *test-dir* "test/mock/d/"))

(defun hash-table->alist (table)
  (loop for key being the hash-keys of table using (hash-value value)
	collect (cons key value)))

(let ((*break-on-signals* 'error))
  (setf *search-path* (list *mock-d-directory* *mock-directory*))
  #+debug (format t "SP: ~W~%" *search-path*)
  (finalize-search-path)
  #+debug (format t "SP: ~W~%" *search-path*)
  (search-search-path)
  #+debug (format t "~W~%" (hash-table->alist *grains*))
  (assert (typep (registered-grain "b") 'build-registry-conflict))
  (assert (equal (fullname (grain-parent (registered-grain "c/x"))) "c"))
  (assert (typep (registered-grain "d") 'build-grain)))
