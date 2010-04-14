#+xcvb (module (:depends-on ("macros")))

(in-package :xcvb)

(defun list-of-length-p (n x)
  (length=n-p x n))

(defun mapcar/ (function env arguments)
  (mapcar (lambda (x) (funcall function env x)) arguments))

;;; CLOS magic (depends on closer-mop) (from philip-jose)
(defun collect-slots (object &key (slots t))
  (loop :with class = (class-of object)
        :with slots = (if (eq slots t) (compute-slots class) slots)
        :for slot :in slots
        :for name = (slot-definition-name slot)
        :for iarg = (or (first (slot-definition-initargs slot)) name)
        :nconc (when (slot-boundp object name)
                 `(,iarg ,(slot-value object name)))))

(defun simple-print-object (object stream &key identity (slots t))
  (with-output (stream)
    (print-unreadable-object (object stream :type t :identity identity)
      (write (collect-slots object :slots slots) :stream stream))))

(defgeneric slots-to-print (object)
  (:method ((object t))
    t))

(defclass simple-print-object-mixin (standard-object)
  ())

(defmethod print-object ((object simple-print-object-mixin) stream)
  (simple-print-object object stream :slots (slots-to-print object)))

(defun remake-object (object &rest keys &key &allow-other-keys)
  (loop :with class = (class-of object)
        :with slots = (compute-slots class)
        :for slot :in slots
        :for name = (slot-definition-name slot)
        :for initarg = (first (slot-definition-initargs slot))
        :when (and initarg (slot-boundp object name))
        :nconc `(,initarg ,(slot-value object name)) :into old-keys
        :finally (return (apply #'make-instance class (append keys old-keys)))))

;;; hash tables
(defun sequence-function-map (function sequence &key key)
  (let ((map (make-hash-table :test 'equal)))
    (flet ((index (x)
             (let ((k (if key (funcall key x) x)))
               ;; more general would be to merge results when it appears multiple times
               ;; instead of dropping subsequent appearances. But this is enough for our purposes.
               (unless (nth-value 1 (gethash k map))
                 (setf (gethash k map) (funcall function x))))))
      (map () #'index sequence))
    map))

(defun sequence-position-map (sequence)
  (let ((index -1))
    (flet ((index (x) (declare (ignore x)) (incf index)))
      (sequence-function-map #'index sequence))))


;;; I/O
(defun readable-string (x &key (package :cl))
  (with-output-to-string (s)
    (with-safe-io-syntax ()
      (let ((*package* (find-package package)))
        (write x :stream s :readably t :escape t :pretty nil)
        (terpri s)))))
