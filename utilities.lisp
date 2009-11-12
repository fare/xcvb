#+xcvb (module (:depends-on ("macros")))

(in-package :xcvb)

;;; String functions
(defun strcat (&rest strings)
  "String concatenation function"
  (apply 'concatenate 'string strings))

(defun last-char (string)
  (check-type string string)
  (let ((l (length string)))
    (unless (zerop l)
      (char string (1- l)))))

(defun but-last-char (string)
  (check-type string string)
  (let ((l (length string)))
    (unless (zerop l)
      (subseq string 0 (1- l)))))

(defun first-char (string)
  (check-type string string)
  (unless (zerop (length string))
    (char string 0)))

(defun join-strings (separator strings &optional out)
  (with-output (out)
    (loop :for (string . more-strings) :on strings :do
      (write-string string out)
      (when more-strings
        (princ separator out)))))

;;; List functions
(defun proper-list-p (x)
  "Returns T if X is a proper list, NIL if it isn't. Checks for circularity"
  (labels
      ((ret (b)
         (return-from proper-list-p b))
       (check (x y)
         (cond
           ((null x) (ret t))
           ((eq x y) (ret nil))
           ((not (consp x)) (ret nil))))
       (recurse (x y)
         (check x y)
         (check (cdr x) y)
         (recurse (cddr x) (cdr y))))
    (check x nil)
    (recurse (cdr x) x)))

(defun list-of-length-p (n x)
  (check-type n (integer 0 *))
  (loop
    :for l = x :then (cdr l)
    :for i :downfrom n :do
    (cond
      ((zerop i) (return (null l)))
      ((not (consp l)) (return nil)))))

(defun mapcar/ (function env arguments)
  (mapcar (lambda (x) (funcall function env x)) arguments))

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
(defun hash-table->alist (table)
  (loop :for key :being :the :hash-keys :of table :using (:hash-value value)
    :collect (cons key value)))

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

(defun make-hashset (&key (test 'eql) list set)
  (let ((h (make-hash-table :test test)))
    (dolist (x list)
      (setf (gethash x h) t))
    (when set
      (loop :for x :being :the :hash-keys :in set :do (setf (gethash x h) t)))
    h))

;;; I/O
(defun readable-string (x &key (package :cl))
  (with-output-to-string (s)
    (with-safe-io-syntax ()
      (let ((*package* (find-package package)))
        (write x :stream s :readably t :escape t :pretty nil)
        (terpri s)))))

