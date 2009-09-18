#+xcvb (module (:depends-on ("macros")))

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

(define-modify-macro funcallf (f &rest args) xfuncall)

(defun xfuncall (x f &rest args)
  (apply f x args))

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

(defun string-prefix<= (x y)
  (check-type x string)
  (check-type y string)
  (and (<= (length x) (length y))
       (string= x y :end2 (length x))))



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
  (and (listp x)
       (loop
           :for l = x :then (cdr l)
           :for i :downfrom n :do
           (cond
             ((zerop i) (return (null l)))
             ((null l) (return nil))))))

;;; Simple Dispatcher

(defun simple-dispatcher (atom-processor function-hash environment expression)
  (if (consp expression)
    (let* ((head (car expression))
           (arguments (cdr expression))
           (function (or (gethash head function-hash)
                         (error "undefined function in ~S" expression))))
      (apply function environment arguments))
    (funcall atom-processor environment expression)))

(defmacro define-simple-dispatcher (name atom-interpreter &optional gf)
  (let ((hash-name (fintern t "*~A-FUNCTIONS*" name))
        (registrar-name (fintern t "REGISTER-~A" name))
        (definer-name (fintern t "DEFINE-~A" name))
        (dispatcher-name (fintern t "~A-DISPATCHER" name)))
    `(progn
       (defvar ,hash-name (make-hash-table :test 'eql))
       (defun ,registrar-name (symbol function)
         (setf (gethash symbol ,hash-name) function))
       (defmacro ,definer-name (symbol formals &body body)
         (let ((fname (fintern t "~A-~A" ',name symbol)))
           `(progn
              (,(if ',gf 'defmethod 'defun)
                ,(fintern t "~A-~A" ',name symbol) ,formals ,@body)
              (,',registrar-name ',symbol ',fname))))
       (defun ,dispatcher-name (environment expression)
         (simple-dispatcher
          ,atom-interpreter
          ,hash-name
          environment expression)))))

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

;;; List stuff

(defun all-descendents-f (x children-f &key (test #'eql))
  (let ((r nil))
    (labels ((add1 (x)
               (unless (member x r :test test)
                 (push x r)
                 (map () #'add1 (funcall children-f x)))))
      (add1 x))
    (nreverse r)))


;;; hash tables

(defun hash-table->alist (table)
  (loop for key being the hash-keys of table using (hash-value value)
	collect (cons key value)))

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

;;; Reading a file's first form
(defun read-first-file-form (filepath &key (package :xcvb-user))
  "Reads the first form from the top of a file"
  (with-standard-io-syntax ()
    (let ((*package* (find-package package))
	  (*read-eval* nil))
      (with-open-file (in filepath)
        (read in nil nil)))))
