;;;;; Mapping virtual pathnames to actual pathnames
#+xcvb (module (:depends-on ("specials" "utilities")))

(in-package :xcvb)

;;;;; Virtual pathname object.

(defclass xcvb-interface (eq:<hashable>) ())

(defclass virtual-pathname ()
  ((hash :initarg :hash :reader vp-hash)
   (root :initarg :root :reader vp-root)
   (subpath :initarg :subpath :reader vp-subpath)
   (resolved-namestring :accessor vp-resolved-namestring)))

(defmethod eq:hash ((i xcvb-interface) (vp virtual-pathname))
  (vp-hash vp))
(defmethod eq:== ((i xcvb-interface) (vp1 virtual-pathname) (vp2 virtual-pathname))
  (and (equal (vp-root vp1) (vp-root vp2))
       (equal (vp-subpath vp1) (vp-subpath vp2))))

(defun make-vp (root &rest subpath)
  (let ((hash (sxhash (cons root subpath))))
    (make-instance 'virtual-pathname
                   :hash hash :root root :subpath subpath)))

(defmethod print-object ((x virtual-pathname) stream)
  (if *print-readably*
      (format stream "#.~S" `(make-vp '(,(vp-root x) ,@(vp-subpath x))))
      (format stream "#<VP: ~S>" `(,(vp-root x) ,@(vp-subpath x)))))

(defun grain-namestring (env grain)
  (vp-namestring env (grain-vp grain)))
(defun fullname-namestring (env fullname)
  (grain-namestring env (or (registered-grain fullname)
                            (resolve-absolute-module-name fullname)
                            (error "Can't resolve ~S" fullname))))
(defun fullname-enough-namestring (env fullname)
  (enough-namestring (fullname-namestring env fullname)))
(defun pseudo-fullname-namestring (env pseudo-fullname) ;; FIXME!
  (vp-namestring env (apply 'vp-for-type-name pseudo-fullname)))
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
                     (namestring (pathname-directory-pathname (grain-pathname build)))) sub)))
          (:obj
           (apply 'strcat *object-directory* subpath))))))

#|
;;;;; Define a little-language for virtual pathnames.

(defvar +lisp-pathname+ (make-pathname :type "lisp"))
(defvar +fasl-pathname+ (make-pathname :type "fasl"))
(defvar +cfasl-pathname+ (make-pathname :type "cfasl"))
(defvar +image-pathname+ (make-pathname :type "image"))

;;; dependency-namestring: extract Makefile-printable pathname from dependency spec
(define-simple-dispatcher dependency-namestring #'dependency-namestring-for-atom)

(defun dependency-pathname (env fullname)
  (ensure-absolute-pathname (dependency-namestring env fullname)))

(defun dependency-namestring (env fullname)
  ;; TODO: double pcheck that a namestring is only used by one fullname,
  ;; using a table to record namestring => fullname mappings
  ;; maybe also have a table the other way to cache these computations?
  (dependency-namestring-dispatcher env fullname))

(defun dependency-namestring-for-atom (env name)
  (declare (ignore env))
  (enough-namestring (grain-pathname (resolve-absolute-module-name name))))

(define-dependency-namestring :lisp (env name)
  (dependency-namestring-for-atom env name))

(define-dependency-namestring :file (env name)
  (declare (ignore env))
  name)

(define-dependency-namestring :fasl (env name)
  (object-namestring env name +fasl-pathname+))

(define-dependency-namestring :cfasl (env name)
  (object-namestring env name +cfasl-pathname+))

(define-dependency-namestring :image (env name)
  (object-namestring env name +image-pathname+))

(define-dependency-namestring :object (env name)
  (object-namestring env name))

(define-dependency-namestring :source (env name &key in)
  (declare (ignore env))
  (enough-namestring
   (merge-pathnames (portable-pathname-from-string name)
                    (grain-pathname (registered-build in)))))

(define-dependency-namestring :path (env name)
  (declare (ignore env))
  name)

(define-dependency-namestring :manifest (env name)
  (object-namestring env (strcat name "__manifest") +lisp-pathname+))



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



#|
;;;;; Define a little-language for virtual pathnames.

(defun normalize-vp (env vp)
  (normalize-vp-dispatcher env vp))

(define-simple-dispatcher normalize-vp #'normalize-vp-atom :generic)

(defmethod normalize-vp-atom (env vp)
  (declare (ignore env))
  (error "Invalid virtual pathname ~S" vp))

(define-normalize-vp :obj (env &rest names)
  (declare (ignore env))
  ;;(portable-pathname-from-string ns :allow-absolute nil)
  `(:obj ,@names))

(define-normalize-vp :path (env name)
  (declare (ignore env))
  name)

(define-normalize-vp :manifest (env &rest names)
  (normalize-vp env `(:obj ,@names "__manifest.lisp")))


;;;;; Define a little-language for virtual pathnames.

(defun normalize-vp (env vp)
  (normalize-vp-dispatcher env vp))

(define-simple-dispatcher normalize-vp #'normalize-vp-atom :generic)

(defmethod normalize-vp-atom (env vp)
  (declare (ignore env))
  (error "Invalid virtual pathname ~S" x))

(define-vp-namestring :obj (env &rest ns)
  (declare (ignore env))
  `(:obj ,@ns))

(define-dependency-namestring :path (env name)
  (declare (ignore env))
  name)

(define-dependency-namestring :manifest (env name)
  (object-namestring env (strcat name "__manifest") +lisp-pathname+))



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

(define-vp-namestring :src (env buildname rel)
  (declare (ignore env))
  (registered-build buildname)
  (portable-pathname-from-string rel :allow-absolute nil)
  `(:src ,buildname ,rel))

(define-vp-namestring :root (env ns)
  (declare (ignore env))
  (portable-namestring-absolute-p ns)
  `(:root ,ns))

;; Make sure that a pathname is only seen in one way,
;; as far as normalized vps go.
(defparameter *ns-to-vp-cache* (make-hash-table :test 'equal))

(defun vp-pathname (env vp)
  (ensure-absolute-pathname (vp-namestring env vp)))

(defun vp-namestring (env vp)
  (let ((ns (vp-namestring-dispatcher env vp))
	(cached (gethash pn *ns-to-vp-cache*)))
    (if cached
        (unless (equal cached vp)
          (error "Pathname ~S is referred as both ~S and ~S" ns cached vp))
        (setf (gethash ns *ns-to-vp-cache*) vp))
    ns))

(define-simple-dispatcher vp-namestring #'vp-namestring-atom :generic)

(defun vp-namestring-atom (env x)
  (declare (ignore env))
  (error "Invalid virtual pathname ~S" x))

(define-vp-namestring :obj (env ns)
  (declare (ignore env))
  (strcat *object-directory* "/" ns))

(define-vp-namestring :src (env buildname rel)
  (declare (ignore env))
  (strcat (build-namestring (registered-build build)) rel))

(define-vp-namestring :root (env ns)
  (declare (ignore env))
  ns)

(defvar +lisp-pathname+ (make-pathname :type "lisp"))
(defvar +fasl-pathname+ (make-pathname :type "fasl"))
(defvar +cfasl-pathname+ (make-pathname :type "cfasl"))
(defvar +image-pathname+ (make-pathname :type "image"))

;;; dependency-namestring: extract Makefile-printable pathname from dependency spec
(define-simple-dispatcher dependency-namestring #'dependency-namestring-for-atom)

(defun dependency-pathname (env fullname)
  (vp-pathname (dependency-vp env fullname)))

(defun dependency-namestring (env fullname)
  (vp-namestring (dependency-vp env fullname)))

(defun dependency-vp (env fullname)
  (dependency-vp-dispatcher env fullname))

(defun dependency-vp-for-atom (env name)
  (declare (ignore env))
  (let ((module (resolve-absolute-module-name name)))
    (grain-vp module)))

(define-dependency-vp :lisp (env name)
  (dependency-namestring-for-atom env name))

|#


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
|#
