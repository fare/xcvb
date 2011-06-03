#+xcvb (module (:depends-on ("pkgdcl")))

;;; Versioning

(in-package :xcvb)

(defun get-xcvb-directory ()
  (with-standard-io-syntax
    (let ((*print-readably* nil))
      (asdf:system-source-directory :xcvb))))

(defun get-xcvb-version ()
  (first
   (run-program/read-output-lines
    (format nil "cd ~A ; git describe --tags" (get-xcvb-directory)))))
