#+xcvb (module (:depends-on ("grain-interface" "grain-registry")))

(in-package :xcvb)

;;(defparameter *computations-inputing-grain*
;;  (make-hash-table :test 'equal)
;;  "hash mapping each grain to a list of computations that take said grain as input")

(defclass computation ()
  ((inputs ;; a list of grains
    :initarg :inputs
    :accessor computation-inputs)
   (outputs ;; a list of grains
    :initarg :outputs
    :accessor computation-outputs)
   ;; (side-effects) ; for additional files being side-effected
   (command ;; SEXP in the command language as used by e.g. Makefile-commands-for-computation
    :initarg :command
    :accessor computation-command)))

(defgeneric make-computation (env &key))

(defmethod make-computation ((env null) &rest keys &key &allow-other-keys)
  (let ((computation (apply #'make-instance 'computation keys)))
    (link-computation-outputs computation)
    (link-computation-inputs computation)
    (push computation *computations*)
    computation))

(defun link-computation-outputs (computation)
  (loop :for target :in (computation-outputs computation)
    :for n :from 0 :do
    (when (slot-boundp target 'computation)
      (error "Grain ~S already is the output of an existing computation!" target))
    (setf (grain-computation target) computation
          (grain-computation-index target) n)))

(defun link-computation-inputs (computation)
  (loop :for input :in (computation-inputs computation) :do
    (pushnew computation (grain-users input))))

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
      (let ((*print-pretty* t)
	    (*print-miser-width* 80))
	(fresh-line stream)
	(format stream "  :inputs~%    ~S~%" (mapcar #'fullname inputs))
	(format stream "  :outputs~%    ~S~%" (mapcar #'fullname outputs))
	(format stream "  :command~%    ~S" command))))
  (fresh-line stream))

(defun computation-target (computation)
  (first (computation-outputs computation)))

(defun grain-computation-target (grain)
  (let ((computation (grain-computation grain)))
    (if computation
      (computation-target computation)
      grain)))

(defun computation-children (computation)
  (mappend #'grain-users (computation-outputs computation)))

(defun map-computations (fun &key from-end)
  (dolist (c (if from-end *computations* (reverse *computations*)))
    (funcall fun c)))

(defun map-computation-grains (fun &key from-end)
  (let ((h (make-hash-table)))
    (map-computations
     (lambda (c)
       (loop :for g :in (append (computation-inputs c) (computation-outputs c))
         :unless (gethash g h) :do
         (setf (gethash g h) t)
         (funcall fun g)))
     :from-end from-end)))

(defun list-computation-grains (&key from-end)
  (while-collecting (c)
    (map-computation-grains #'c :from-end from-end)))
