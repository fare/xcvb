(in-package :xcvb)

;;; Conditions

(define-condition dependency-cycle (simple-error)
  ;; This condition is signaled if the dependency graph has any cycles in it.
  ())

(define-condition syntax-error (simple-error)
  ;; Condition is signaled if there is some syntax error in some user-specified data
  ())

(define-condition grain-not-found (simple-error)
  ;; Condition is signaled if there is some syntax error in some user-specified data
  ())

(defun simply-error (simple-error control &rest args)
  (error (or simple-error 'simple-error)
         :format-control control :format-arguments args))


;;; String functions

(defun strcat (&rest strings)
  "String concatenation function"
  (apply 'concatenate 'string strings))

(defun last-char (string)
  (check-type string string)
  (let ((l (length string)))
    (unless (zerop l)
      (char string (1- l)))))

(defun first-char (string)
  (check-type string string)
  (unless (zerop (length string))
    (char string 0)))

(defun join-strings (separator strings &optional out)
  (with-output (out)
    (loop :for (string . rest) :on strings :do
      (write-string string out)
      (when rest
        (princ separator out)))))


;;; CLOS magic (depends on closer-mop) (from philip-jose)

(defun collect-slots (object &optional (slot-list t))
  (loop :with class = (class-of object)
        :with slots = (if (eq slot-list t) (compute-slots class) slot-list)
        :for slot :in slots
        :for name = (slot-definition-name slot)
        :for iarg = (or (first (slot-definition-initargs slot)) name)
        :nconc (when (slot-boundp object name) `(,iarg ,(slot-value object name)))))

(defun simple-print-object (object stream &key identity (slots t))
  (with-output (stream)
    (print-unreadable-object (object stream :type t :identity identity)
      (write (collect-slots object slots) :stream stream))))

(defgeneric slots-to-print (object)
  (:method ((object t))
    t))

(defclass simple-print-object-mixin (standard-object)
  ())

(defmethod print-object ((object simple-print-object-mixin) stream)
  (simple-print-object object stream :slots (slots-to-print object)))


;;; List stuff

(defun flatten-f (x children-f &key (test #'eql))
  (let ((r nil))
    (labels ((add1 (x)
               (unless (member x r :test test)
                 (push x r)
                 (map () #'add1 (funcall children-f x)))))
      (add1 x))
    (nreverse r)))
