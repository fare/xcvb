#+xcvb
(module
  (:depends-on ("specials" "grain-interface")))

(in-package :xcvb)

(defun grain-pathname-mapping (grain)
  (list (fullname grain) :pathname ,(grain-pathname grain)))

(defun print-pathname-mappings (stream grains)
  (with-safe-io-syntax ()
    (let ((*print-readably* t))
      (format stream "~
 (defparameter *pathname-mappings* ~
   (macrolet ((f () (let ((m (make-hash-table :test 'equal))) ~
     (loop :for (x . y) :in '~S :do (setf (gethash x m) y)) m))) ~
     (f)))~%"
              (mapcar 'grain-pathname-mapping grains)))))

(defun pathname-mappings-lisp-pathname ()
  (subpathname *workspace* "pathname-mappings.lisp"))

(defun pathname-mappings-fasl-pathname ()
  (make-pathname :type *fasl-type* :defaults (pathname-mappings-lisp-pathname)))

(defun make-pathname-mappings-lisp ()
  (with-open-file (s (pathname-mappings-lisp-pathname) :direction :output
                     :if-exists :rename-and-delete :if-does-not-exist :create)
    (print-pathname-mappings s (list-grains))))
