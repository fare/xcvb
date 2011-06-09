#+xcvb
(module
 (:build-depends-on ("driver")
  :depends-on ("/asdf")))

;;; Versioning

(in-package :xcvb-driver)

(defun get-xcvb-directory ()
  (with-standard-io-syntax
    (let ((*print-readably* nil))
      (asdf:system-source-directory :xcvb))))

(defun get-xcvb-version ()
  (first
   (run-program/read-output-lines
    (format nil "cd ~A ; git describe --tags" (get-xcvb-directory)))))
