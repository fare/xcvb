#+xcvb
(module
 (:build-depends-on ("driver")
  :depends-on ("/asdf")))

;;; Versioning

(in-package :xcvb-driver)

(defun get-xcvb-directory ()
  (asdf:system-source-directory :xcvb))

(defun get-xcvb-version ()
  (first
   (run-program/
    (format nil "cd ~A ; git describe --tags --dirty=+" (get-xcvb-directory))
    :output :lines)))
