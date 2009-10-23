#+xcvb
(module
 (:depends-on
  ("macros"
   (:when (:featurep :sbcl) (:require :sb-posix)))))

(in-package :xcvb)

#|
0- don't use hash consing
1- instead keep a TTH(?) hash for each "state of the world" -
 hashname and fullhashnames
2- equal-hash that!
|#


(defun mkfifo (pathname mode)
  #+sbcl (sb-posix:mkfifo pathname mode)
  #+clozure (ccl::with-filename-cstrs ((p pathname))(#_mkfifo p mode))
  #+clisp (LINUX:mkfifo pathname mode)
  #-(or sbcl clozure clisp) (error "mkfifo not implemented for your Lisp"))

(defvar *workers* (make-hash-table :test 'equal)
  "maps intentional state of the world identifiers to descriptors of worker processes
waiting at this state of the world.")

(defclass worker ()
  ())
(defgeneric worker-send (worker form)
  (:documentation "send a form to be executed on the worker"))
(defmethod worker-send (worker (x cons))
  (worker-send worker (readable-string x)))

(defvar *worlds* (make-hash-table :test 'equal))
(defclass world ()
  ((hash :reader world-hash :initarg :hash)
   (setup :reader world-setup :initarg :setup)
   (commands-r :reader world-commands-r :initarg :commands-r)
   (included-dependencies :reader included-dependencies :initarg :included-dependencies)
   (issued-load-commands :reader issued-load-commands :initarg :issued-load-commands)))
(defun make-world-summary (setup commands-r)
  (cons setup commands-r))
(defun world-summary (world)
  (make-world-summary (world-setup world) (world-commands-r world)))
(defun world-summary-hash (world-summary)
  (sxhash world-summary)) ; use tthsum?
(defun compute-world-hash (world)
  (world-summary-hash (world-summary world))) ; use tthsum?
(defun world-equal (w1 w2)
  (equal (world-summary w1) (world-summary w2)))
(defun intern-world-summary (setup commands-r thunk)
  (loop
    :with summary = (make-world-summary setup commands-r)
    :with hash = (world-summary-hash summary)
    :for w :in (gethash hash *worlds*)
    :do (when (equal summary (world-summary w)) (return w))
    :finally (let ((world (apply #'make-instance 'world
                                 :hash hash
                                 :setup setup
                                 :commands-r commands-r
                                 (funcall thunk))))
               (push world (gethash hash *worlds* '()))
               (return world))))
(defun make-initial-world ()
  (intern-world-summary
   () ()
   (lambda ()
     (list
      :setup () :commands-r ()
      :included-dependencies (make-hash-table :test 'equal)
      :issued-load-commands (make-hash-table :test 'equal)))))
(defun setup-world (world fullname)
  (let ((new-setup (append1 (world-setup world) fullname)))
    (intern-world-summary
     new-setup
     (world-commands-r world)
     (lambda ()
       (list
        :included-dependencies (make-hash-table :test 'equal)
        :issued-load-commands (make-hash-table :test 'equal))))))

(defclass farmer-traversal (xcvb-traversal)
  ((world
    :accessor current-world
    :documentation "world object representing the current state of the computation")
   (dependencies-r
    :initform nil
    :accessor traversed-dependencies-r
    :documentation "dependencies issued as part of the current computation, in reverse order")
   (issued-load-commands
    :initform (make-hashset :test 'equal)
    :accessor issued-load-commands
    :documentation "load commands issued so far to run the current compilation, as a set")
   (load-commands-r
    :initform nil
    :accessor traversed-load-commands-r
    :documentation "load commands issued so far to run the current compilation, in reverse order")))

(defmethod included-dependencies ((traversal farmer-traversal))
  (included-dependencies (current-world traversal)))
(defmethod dependency-already-included-p ((env farmer-traversal) grain)
  (or (gethash grain (included-dependencies env))
      (call-next-method)))

