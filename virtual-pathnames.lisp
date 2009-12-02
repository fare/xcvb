;;;;; Mapping virtual pathnames to actual pathnames
#+xcvb (module (:depends-on ("specials" "utilities")))

(in-package :xcvb)

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
