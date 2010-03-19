;;;;; Mapping virtual pathnames to actual pathnames
#+xcvb (module (:depends-on ("specials" "utilities")))

(in-package :xcvb)

;;;;; Virtual pathname object.

(defclass xcvb-interface (eq:<hashable>) ())

(defclass virtual-pathname ()
  ((hash :reader vpn-hash :initarg hash)
   (root :initarg :root :reader vpn-root)
   (subpath :initarg :path :reader vpn-subpath)
   (resolved-namestring :accessor vpn-resolved-namestring)))

(defmethod eq:hash ((i xcvb-interface) (vpn virtual-pathname))
  (vpn-hash vpn))
(defmethod eq:== ((i xcvb-interface) (vpn1 virtual-pathname) (vpn2 virtual-pathname))
  (and (equal (vpn-root vpn1) (vpn-root vpn2))
       (equal (vpn-subpath vpn1) (vpn-subpath vpn2))))

(defun make-vpn (x)
  (destructuring-bind (root &rest subpath) x
    (let ((hash (sxhash x)))
      (make-instance 'virtual-pathname
        :hash hash :root root :subpath subpath))))

(defmethod print-object ((x virtual-pathname) stream)
  (if *print-readably*
      (format stream "#.~S" `(make-vpn '(,(vpn-root x) ,@(vpn-subpath x))))
      (format stream "#<VPN: ~S>" `(,(vpn-root x) ,@(vpn-subpath x)))))


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

(defun normalize-vpn (env vpn)
  (normalize-vpn-dispatcher env vpn))

(define-simple-dispatcher normalize-vpn #'normalize-vpn-atom :generic)

(defmethod normalize-vpn-atom (env vpn)
  (declare (ignore env))
  (error "Invalid virtual pathname ~S" vpn))

(define-normalize-vpn :obj (env &rest names)
  (declare (ignore env))
  ;;(portable-pathname-from-string ns :allow-absolute nil)
  `(:obj ,@names))

(define-normalize-vpn :path (env name)
  (declare (ignore env))
  name)

(define-normalize-vpn :manifest (env &rest names)
  (normalize-vpn env `(:obj ,@names "__manifest.lisp")))


;;;;; Define a little-language for virtual pathnames.

(defun normalize-vpn (env vpn)
  (normalize-vpn-dispatcher env vpn))

(define-simple-dispatcher normalize-vpn #'normalize-vpn-atom :generic)

(defmethod normalize-vpn-atom (env vpn)
  (declare (ignore env))
  (error "Invalid virtual pathname ~S" x))

(define-vpn-namestring :obj (env &rest ns)
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

(define-vpn-namestring :src (env buildname rel)
  (declare (ignore env))
  (registered-build buildname)
  (portable-pathname-from-string rel :allow-absolute nil)
  `(:src ,buildname ,rel))

(define-vpn-namestring :root (env ns)
  (declare (ignore env))
  (portable-namestring-absolute-p ns)
  `(:root ,ns))

;; Make sure that a pathname is only seen in one way,
;; as far as normalized vpns go.
(defparameter *ns-to-vpn-cache* (make-hash-table :test 'equal))

(defun vpn-pathname (env vpn)
  (ensure-absolute-pathname (vpn-namestring env vpn)))

(defun vpn-namestring (env vpn)
  (let ((ns (vpn-namestring-dispatcher env vpn))
	(cached (gethash pn *ns-to-vpn-cache*)))
    (if cached
        (unless (equal cached vpn)
          (error "Pathname ~S is referred as both ~S and ~S" ns cached vpn))
        (setf (gethash ns *ns-to-vpn-cache*) vpn))
    ns))

(define-simple-dispatcher vpn-namestring #'vpn-namestring-atom :generic)

(defun vpn-namestring-atom (env x)
  (declare (ignore env))
  (error "Invalid virtual pathname ~S" x))

(define-vpn-namestring :obj (env ns)
  (declare (ignore env))
  (strcat *object-directory* "/" ns))

(define-vpn-namestring :src (env buildname rel)
  (declare (ignore env))
  (strcat (build-namestring (registered-build build)) rel))

(define-vpn-namestring :root (env ns)
  (declare (ignore env))
  ns)

(defvar +lisp-pathname+ (make-pathname :type "lisp"))
(defvar +fasl-pathname+ (make-pathname :type "fasl"))
(defvar +cfasl-pathname+ (make-pathname :type "cfasl"))
(defvar +image-pathname+ (make-pathname :type "image"))

;;; dependency-namestring: extract Makefile-printable pathname from dependency spec
(define-simple-dispatcher dependency-namestring #'dependency-namestring-for-atom)

(defun dependency-pathname (env fullname)
  (vpn-pathname (dependency-vpn env fullname)))

(defun dependency-namestring (env fullname)
  (vpn-namestring (dependency-vpn env fullname)))

(defun dependency-vpn (env fullname)
  (dependency-vpn-dispatcher env fullname))

(defun dependency-vpn-for-atom (env name)
  (declare (ignore env))
  (let ((module (resolve-absolute-module-name name)))
    (grain-vpn module)))

(define-dependency-vpn :lisp (env name)
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
