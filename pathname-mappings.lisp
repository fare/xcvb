#+xcvb
(module
  (:depends-on ("specials" "grain-interface")))

;;; TODO: have magic grains, an override method for digesting,
;;; returning a magic NIL, meaning "this grain is not subject to digest"?
;;; Or keep treating magically? Or have some way of segregating
;;; "dependencies at our level of abstraction" vs
;;; "dependencies below our level of abstraction".
;;; Reminds me of the dependencies that do or do not "force"
;;; a module to be recompiled in ASDF.

(in-package :xcvb)

(defun grain-pathname-mapping (env grain)
  (list (fullname grain) :pathname (grain-namestring env grain)))

(defun print-pathname-mappings (env stream grains)
  (with-safe-io-syntax ()
    (let ((*print-readably* nil)
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

