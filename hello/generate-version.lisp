#+xcvb (module (:depends-on ("specials")))

(in-package :xcvb-hello)

(defparameter *xcvb-version-path*
  (asdf:system-relative-pathname :xcvb "version.lisp"))
(defparameter *version-path*
  (asdf-utilities:merge-pathnames*
   (make-pathname :directory '(:relative "hello"))
   *xcvb-version-path*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :xcvb)
    (make-package :xcvb :use '(:cl))))

(load *xcvb-version-path*)
(with-open-file (s *version-path* :direction :output
                   :if-exists :rename-and-delete :if-does-not-exist :create)
  (format s "(in-package :xcvb-hello)~%(setf *version* ~S)~%"
          (symbol-value (find-symbol (string :*xcvb-version*) :xcvb))))
