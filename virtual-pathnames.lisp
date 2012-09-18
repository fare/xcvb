;;;;; Mapping virtual pathnames to actual pathnames
#+xcvb (module (:depends-on ("specials" "utilities")))

(in-package :xcvb)

;;;;; Virtual pathname object.

(define-interface xcvb-interface (<hashable>) ())

(defclass virtual-pathname ()
  ((hash :initarg :hash :reader vp-hash)
   (root :initarg :root :reader vp-root)
   (subpath :initarg :subpath :reader vp-subpath)
   (resolved-namestring :accessor vp-resolved-namestring)))

(defmethod hash ((i xcvb-interface) (vp virtual-pathname))
  (vp-hash vp))
(defmethod == ((i xcvb-interface) (vp1 virtual-pathname) (vp2 virtual-pathname))
  (and (equal (vp-root vp1) (vp-root vp2))
       (equal (vp-subpath vp1) (vp-subpath vp2))))

(defun make-vp (root &rest subpath)
  (let ((hash (sxhash (cons root subpath))))
    (make-instance 'virtual-pathname
                   :hash hash :root root :subpath subpath)))

(defmethod print-object ((x virtual-pathname) stream)
  (if *print-readably*
      (format stream "#.~S" `(make-vp '(,(vp-root x) ,@(vp-subpath x))))
      (format stream "#<VP ~S>" `(,(vp-root x) ,@(vp-subpath x)))))

(defun grain-namestring (env grain)
  (vp-namestring env (grain-vp grain)))
(defun fullname-namestring (env fullname)
  (grain-namestring env (or (registered-grain fullname)
                            (resolve-absolute-module-name fullname)
                            (error "Can't resolve ~S" fullname))))

(defgeneric effective-namestring (env fullname))
(defmethod effective-namestring (env fullname)
  (fullname-namestring env fullname))

(defun fullname-enough-namestring (env fullname)
  (enough-namestring (fullname-namestring env fullname)))
(defun pseudo-fullname-namestring (env pseudo-fullname) ;; FIXME! - make manifests into their own grain!
  (vp-namestring env (default-vp-for-fullname env pseudo-fullname)))

(defgeneric pseudo-effective-namestring (env fullname))
(defmethod pseudo-effective-namestring (env fullname)
  (pseudo-fullname-namestring env fullname))

(defun pseudo-fullname-enough-namestring (env pseudo-fullname) ;; FIXME!
  (enough-namestring (pseudo-fullname-namestring env pseudo-fullname)))

(defun vp-pathname (env vp)
  (pathname (vp-namestring env vp)))

(defgeneric vp-namestring (env vp))
(defmethod vp-namestring (env vp)
  (declare (ignorable env))
  (if (slot-boundp vp 'resolved-namestring)
      (vp-resolved-namestring vp)
      (with-slots (root subpath) vp
        (ecase root
          (:src
           (let* ((bname (first subpath))
                  (sub (rest subpath))
                  (build (registered-build bname :ensure-build t)))
             (apply 'strcat
                    (but-last-char
                     (namestring
                      (pathname-directory-pathname (grain-pathname build))))
                    sub)))
	  ;; TODO: define a zone :install for end-products like executables?
          (:obj
           (apply 'strcat *object-cache-namestring* subpath))))))


;;;;; Should we register pathnames to make sure there is no aliasing? meh...
#|
(defparameter *virtual-pathnames*
  (make-hash-table :test 'equal)
  "A registry of visited virtual pathnames,
mapping actual pathnames back to virtual pathnames.")

(defun registered-pathname (p)
  (gethash p *virtual-pathnames*))

(defun (setf registered-pathname) (vp p)
  (setf (gethash p *virtual-pathnames*) vp))
|#
