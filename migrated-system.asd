;;; -*- Lisp -*-

(in-package :xcvb)

(asdf:defsystem :migrated-system .
 #.(with-open-file (s (cl-launch:apply-output-pathname-translations
		       (merge-pathnames *components-path*)))
     (cdar (read s))))
