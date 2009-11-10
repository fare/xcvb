#+xcvb (module (:depends-on ("grain-interface" "registry")))

(in-package :xcvb)

;;(defparameter *computations-inputing-grain*
;;  (make-hash-table :test 'equal)
;;  "hash mapping each grain to a list of computations that take said grain as input")

(defclass computation ()
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

(defgeneric make-computation (env &key))

(defmethod make-computation ((env null) &rest keys &key &allow-other-keys)
  (let ((computation (apply #'make-instance 'computation keys)))
    (link-computation-outputs computation)
    (push computation *computations*)
    computation))

(defun link-computation-outputs (computation)
  (loop :for target :in (computation-outputs computation)
    :for n :from 0 :do
    (when (slot-boundp target 'computation)
      (error "Grain ~S already is the output of an existing computation!" target))
    (setf (grain-computation target) computation
          (grain-computation-index target) n)))

(defun make-nop-computation (dependencies &optional targets)
  (make-computation ()
                    :inputs dependencies
                    :outputs targets
                    :command nil))

(defun make-phony-grain (&key name dependencies)
  (let* ((grain (make-grain 'phony-grain :fullname name)))
    (make-nop-computation dependencies (list grain))
    grain))

(defmethod print-object ((x computation) stream)
  (print-unreadable-object (x stream :type t :identity nil)
    (with-slots (inputs outputs command) x
    (format stream ":inputs ~S :outputs ~S :command ~S"
            (mapcar #'fullname inputs)
            (mapcar #'fullname outputs)
            command))))

(defun computation-target (computation)
  (first (computation-outputs computation)))

(defun grain-computation-target (grain)
  (let ((computation (grain-computation grain)))
    (if computation
      (computation-target computation)
      grain)))

;;; TODO: use a more declarative model to describe the various types of objects
;;; and the types of relations between them within a given first-class context,
;;; so that there can be pure functions from context to context,
;;; mapping sets of facts (atoms and relationships) to sets of facts.
;;; make good use of linear relationships for in-place modification,
;;; automatically create indices, etc.
