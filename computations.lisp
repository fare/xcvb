(in-package :xcvb)

(defclass computation () ())
(defclass computation-type () ())

;;(defparameter *computations-inputing-grain*
;;  (make-hash-table :test 'equal)
;;  "hash mapping each grain to a list of computations that take said grain as input")

(defclass concrete-computation (computation)
  ((inputs
    :initarg :inputs
    :accessor computation-inputs)
   (outputs
    :initarg :outputs
    :accessor computation-outputs)
   ;; (side-effects) ; for additional files being side-effected
   (command
    :initarg :command
    :accessor computation-command)))

(defgeneric computation-inputs (computation))
(defgeneric computation-outputs (computation))
;;(defgeneric computation-side-effects (computation))

#|
(defgeneric computation-command (type computation &key))

(defclass lisp-command ()
  ((implementation
    :initarg :implementation
    :initform *lisp-implementation-type*
    :accessor lisp-implementation)
   (executable
    :initarg :executable
    :initform *lisp-executable-pathname*
    :accessor lisp-executable)
   (image
    :initarg :image
    :initform *lisp-image-pathname*
    :accessor lisp-image)
   (flags
    :initarg :flags
    :initform *lisp-flags*
    :accessor lisp-flags)))

(defun make-lisp-command (&rest r)
  (apply #'make-the 'lisp-command r))

(defvar *lisp-command* (make-lisp-command))

(defun lisp-command-using-image (lisp-command image)
   (make-the 'lisp-command
    :implementation (lisp-implementation lisp-command)
    :executable (lisp-executable lisp-command)
    :image image
    :flags (lisp-flags lisp-command)))

(defclass shell-command (computation-type) ())

(defvar *shell-command* (make-the 'shell-command))

(defclass concrete-shell-computation (concrete-computation)
  ())

(defclass concrete-lisp-computation (concrete-computation)
  ())

(defmethod computation-command ((s shell-command) (c concrete-shell-computation))
  (slot-value c 'command))

(defmethod computation-command ((l lisp-command) (c concrete-lisp-computation))
  (slot-value c 'command))

(defmethod computation-command ((s shell-command) (c concrete-lisp-computation)
                                &key (lisp-command (make-lisp-command)))
  (let* ((a (lisp-invocation-arglist
             :implementation-type (lisp-implementation lisp-command)
             :lisp-path (lisp-executable lisp-command)
             :image-path (lisp-image lisp-command)
             :lisp-flags (lisp-flags lisp-command)
             :eval (strcat (computation-command l c)
                           (quit-form 0 (lisp-implementation lisp-command))))))
    (shell-tokens-to-string a)))
|#

(defun make-computation (class &rest keys &key inputs outputs command &allow-other-keys)
  (declare (ignore inputs command))
  (let ((computation (apply #'make-instance class keys)))
    (loop for target in outputs
          for n from 0 do
          (setf (grain-computation target) computation
                (grain-computation-index target) n))
    computation))

(defun make-nop-computation (dependencies &optional targets)
  (make-computation 'concrete-computation
                    :inputs dependencies
                    :outputs targets
                    :command nil))

(defun make-phony-grain (&key name dependencies)
  (let* ((grain (make-grain 'phony-grain :fullname name)))
    (make-nop-computation dependencies (list grain))
    grain))

;;; TODO: use a more declarative model to describe the various types of objects
;;; and the types of relations between them within a given first-class context,
;;; so that there can be pure functions from context to context,
;;; mapping sets of facts (atoms and relationships) to sets of facts.
;;; make good use of linear relationships for in-place modification,
;;; automatically create indices, etc.
