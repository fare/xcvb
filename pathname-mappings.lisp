#+xcvb
(module
  (:depends-on ("specials" "grain-interface")))

(in-package :xcvb)

(defun grain-pathname-mapping (env grain)
  (list (fullname grain) :pathname (grain-namestring env grain)))

(defun print-pathname-mappings (env stream grains)
  (with-safe-io-syntax ()
    (let ((*print-readably* t)
          (*print-pretty* t)
          (*print-case* :downcase))
      (format stream "(in-package :xcvb-driver)~%~
 (defparameter *pathname-mappings*~%  ~
   (macrolet ((f () (let ((m (make-hash-table :test 'equal)))~%    ~
     (loop :for (x . y) :in~%       '~S~%      ~
       :do (setf (gethash x m) y)) m)))~%    ~
     (f)))~%"
              (mapcar/ 'grain-pathname-mapping env
                       (remove-if-not 'file-grain-p grains))))))

(defun pathname-mappings-lisp-pathname ()
  (subpathname *workspace* "pathname-mappings.lisp"))

(defun pathname-mappings-fasl-pathname ()
  (make-pathname :type *fasl-type* :defaults (pathname-mappings-lisp-pathname)))

(defun make-pathname-mappings-lisp (env)
  (with-open-file (s (pathname-mappings-lisp-pathname) :direction :output
                     :if-exists :rename-and-delete :if-does-not-exist :create)
    (print-pathname-mappings env s (list-computation-grains))))
