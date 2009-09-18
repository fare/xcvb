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
