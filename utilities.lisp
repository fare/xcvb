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
    (loop :for (string . rest) :on strings :do
      (write-string string out)
      (when rest
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

;;; Parsing Common Lisp formals
;; over-engineering alert: this is only useful if some kind of portable
;; user-friendly error message facility is grown out of it. Not the case now.
(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar +lambda-formals-keywords+ '(&optional &rest &key &allow-other-keys &aux)) ;; &body &environment

(defun bindable-symbol-p (x)
  (and (symbolp x)
       (not (eq (symbol-package x) (load-time-value (find-package :common-lisp))))
       (not (keywordp x))))

(defun normalize-optional-binding (x)
  (etypecase x
    (symbol
	(assert (bindable-symbol-p x))
	(list x nil nil))
    (list
	(assert (and (bindable-symbol-p (car x))
		     (null (cdddr x))
		     (or (null (cddr x)) (bindable-symbol-p (caddr x)))))
	(list (car x) (cadr x) (caddr x)))))

(defun parse-formals-when-keyword (keyword formals parser &rest args)
  (if (and (consp formals)
           (eq (car formals) keyword))
    (apply parser (cdr formals) args)
    (values nil formals nil)))

(defun parse-true (formals)
  (values t formals t))

(defun parse-formals-keyword (keyword formals)
  (parse-formals-when-keyword keyword formals #'parse-true))

(defun parse-formals (formals normalizer &optional (keywords +lambda-formals-keywords+))
  (loop :for x :in formals
        :for tail :on formals
        :for endp = (member x keywords)
        :for rest = (if endp tail (cdr tail))
        :for normal = (and (not endp) (funcall normalizer x))
        :while normal
        :collect normal :into normals
        :finally (return (values normals rest t))))

(defun parse-formal (formals normalizer)
  (unless (consp formals)
    (error "Expected a formal argument after &REST"))
  (values (funcall normalizer (car formals)) (cdr formals) t))

(defun lambda-formals-parser (formals)
  (multiple-value-bind (mandatory f)
      (parse-formals formals #'identifier-validator)
  (multiple-value-bind (optional f)
      (parse-formals-when-keyword '&optional f #'parse-formals #'optional-formal-normalizer)
  (multiple-value-bind (rest f)
      (parse-formals-when-keyword '&rest f #'parse-formal #'identifier-validator)
  (multiple-value-bind (keys f key)
      (parse-formals-when-keyword '&key f #'parse-formals #'key-formal-normalizer)
  (multiple-value-bind (allow-other-keys f)
      (parse-formals-keyword '&allow-other-keys f)
  (multiple-value-bind (aux f)
      (parse-formals-when-keyword '&aux f #'parse-formals #'aux-formal-normalizer)
    (unless (null f)
      (error "Unexpected trailing ~S after parsing formal arguments.
Did you mix up the ordering?" f))
    (when (and allow-other-keys (not key))
      (error "Unexpected &ALLOW-OTHER-KEYS without &KEY"))
    (list :mandatory mandatory
          :optional optional
          :rest rest
          :key key :keys keys
          :allow-other-keys allow-other-keys
          :aux aux))))))))

(defun identifier-validator (x)
  (unless (bindable-symbol-p x)
    (error "Used ~S as a formal parameter, but cannot be bound" x))
  x)

(defun optional-formal-normalizer (x)
  ;; {var | (var [init-form [supplied-p-parameter]])}
  ;; => (var init-form supplied-p-parameter)
  (flet ((err ()
           (error "Invalid optional formal ~S" x)))
    (typecase x
      (symbol
         (unless (bindable-symbol-p x)
           (err))
         (list x nil nil))
      (cons
         (unless (and (bindable-symbol-p (car x))
                      (null (cdddr x))
                      (or (null (cddr x)) (bindable-symbol-p (caddr x))))
           (err))
         (list (car x) (cadr x) (caddr x)))
      (t
         (err)))))

(defun key-formal-normalizer (x)
  ;; {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}
  ;; => (keyword-name var init-form supplied-p-parameter)
  (flet ((err ()
           (error "Invalid keyword formal ~S" x)))
    (typecase x
      (symbol
         (unless (bindable-symbol-p x)
           (err))
         (list (keywordify x) x nil nil))
      (cons
         (let ((key (car x)))
           (multiple-value-bind (keyword-name var)
               (if (consp key)
                 (if (and (keywordp (car key))
                          (consp (cdr key))
                          (null (cddr key)))
                   (values (car key) (identifier-validator (cadr key)))
                   (err))
                 (let ((var (identifier-validator key)))
                   (values (keywordify var) var)))
             (unless (or (null (cdr x))
                         (and (consp (cdr x))
                              (or (null (cddr x))
                                  (and (consp (cddr x))
                                       (null (cdddr x))))))
               (err))
             (list keyword-name var (cadr x) (caddr x)))))
      (t (err)))))

(defun aux-formal-normalizer (x)
  ;; {var | (var [init-form])}
  ;; => (var init-form)
  (flet ((err ()
           (error "Invalid aux formal ~S" x)))
    (typecase x
      (symbol
         (unless (bindable-symbol-p x)
           (err))
         (list x nil))
      (cons
         (unless (and (bindable-symbol-p (car x))
                      (null (cddr x)))
           (err))
         (list (car x) (cadr x)))
      (t (err)))))

(defun argument-number-checker (formals)
  (destructuring-bind (&key mandatory optional rest key keys allow-other-keys aux)
      (lambda-formals-parser formals)
    (declare (ignore keys allow-other-keys aux))
    (let ((n-mandatory (length mandatory))
          (n-optional (length optional))
          (restp (or rest key)))
      (cond
        (restp (list '<= n-mandatory))
        (optional (list 'betweenp n-mandatory (+ n-mandatory n-optional)))
        (t (list '= n-mandatory))))))

(defun betweenp (min max n)
  (<= min n max)))

;;; Simple Dispatcher

(defun simple-dispatcher (atom-processor function-hash environment expression)
  (if (consp expression)
    (let* ((head (car expression))
           (arguments (cdr expression))
           (defined (or (gethash head function-hash)
                        (error "undefined function in ~S" expression)))
           (function (car defined))
           (arity-checker (cdr defined)))
      (unless (funcall arity-checker (1+ (length arguments)))
         (error "incorrect arity for ~S" expression))
      (apply function environment arguments))
    (funcall atom-processor environment expression)))

(defmacro define-simple-dispatcher (name atom-interpreter)
  (let ((hash-name (fintern t "*~A-FUNCTIONS*" name))
        (registrar-name (fintern t "REGISTER-~A" name))
        (definer-name (fintern t "DEFINE-~A" name))
        (dispatcher-name (fintern t "~A-DISPATCHER" name)))
    `(progn
       (defvar ,hash-name (make-hash-table :test 'eql))
       (defun ,registrar-name (symbol arity-checker function)
         (setf (gethash symbol ,hash-name) (cons function arity-checker)))
       (defmacro ,definer-name (symbol formals &body body)
         `(,',registrar-name ',symbol
                             #'(lambda (x) (,@(argument-number-checker formals) x))
                             (defun ,(fintern t "~A-~A" ',name symbol) ,formals ,@body)))
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


;;; Reading a file's first form
(defun read-first-file-form (filepath)
  "Reads the first form from the top of a file"
  (with-standard-io-syntax ()
    (let ((*package* (find-package :xcvb-user))
	  (*read-eval* nil))
      (with-open-file (in filepath)
        (read in)))))
