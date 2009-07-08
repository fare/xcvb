;;; Extracting properties from the target lisp implementation
#+xcvb (module (:depends-on ("lisp-invocation")))

(in-package :xcvb)

(defvar *target-properties* ()
  "Properties of the target system")

(defparameter *target-properties-variables*
  '(( *use-cfasls* . "(or #+sbcl (and (find-symbol \"*EMIT-CFASL*\" \"SB-C\") t))")
    ( *target-system-features* . "*features*"))
  "plist of variables and how to compute them in the target system")

(defun target-properties-file ()
  (strcat *object-directory* "/target-properties.lisp-expr"))

(defun get-target-properties ()
  (unless *target-properties*
    (read-target-properties)))

(defun read-target-properties ()
  (let* ((file (target-properties-file))
         (form (read-first-file-form file)))
    (unless (and (consp form) (eq 'setf (car form)))
      (error "Malformed target properties file ~S" file))
    (setf *target-properties* (cdr form))
    (loop :for (var value) :on *target-properties* :by #'cddr :do
          (if (member var *target-properties-variables* :key #'car)
              (set var value)
              (error "Invalid target property ~S in file ~S" var file)))))

(defun extract-target-properties ()
  (let ((file (target-properties-file)))
    (ensure-directories-exist file)
    (query-target-lisp-helper (target-properties-form) file)))

(defun target-properties-form ()
  (with-standard-io-syntax ()
    (let* ((*package* (find-package :xcvb-user))
           (*read-eval* nil)
           (vars (mapcar #'car *target-properties-variables*))
           (exprs (mapcar #'cdr *target-properties-variables*)))
      (format nil "(format t \"(cl:setf~~%~{ xcvb::~(~A~) '~~S~~%~})~~%\"~{ ~A~})"
              vars exprs))))

(defun query-target-lisp-helper (query-string output-filename)
  (assert *lisp-implementation-type*)
  (asdf:run-shell-command ;;; TODO: use something better, someday
   "~A > ~A"
   (shell-tokens-to-string
    (lisp-invocation-arglist
     :eval (format nil "(progn ~A (finish-output) ~A)"
                   query-string
                   (format nil (slot-value
                                (get-lisp-implementation
                                 *lisp-implementation-type*) 'quit-format) 0))))
   output-filename))
