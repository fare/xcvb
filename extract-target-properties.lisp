;;; Extracting properties from the target lisp implementation
#+xcvb (module (:depends-on ("lisp-invocation" "string-escape")))

(in-package :xcvb)

(defvar *target-properties* ()
  "Properties of the target system")

(defparameter *target-properties-variables*
  '((*use-cfasls*
     . "(or #+sbcl (and (find-symbol \"*EMIT-CFASL*\" \"SB-C\") t))")
    (*target-system-features*
     . "*features*")
    (*lisp-implementation-directory*
     . "(or #+sbcl (namestring(sb-int:sbcl-homedir-pathname)) #+ccl (namestring(ccl::ccl-directory)))"))
  "alist of variables and how to compute them in the target system")

(defun get-target-properties ()
  (unless *target-properties*
    (read-target-properties)))

(defun read-target-properties ()
  (let ((forms (extract-target-properties)))
    (unless forms
      (error "Failed to extract properties from your Lisp implementation"))
    (unless (and (list-of-length-p 1 forms)
                 (consp (car forms)) (eq 'setf (caar forms)))
      (error "Malformed target properties"))
    (setf *target-properties* (cdar forms))
    (loop :for (var value) :on *target-properties* :by #'cddr :do
          (cond
            ((not (member var *target-properties-variables* :key #'car))
             (error "Invalid target property ~S in file ~S" var file))
            ((not (and (list-of-length-p 2 value) (eq 'quote (car value))))
             (error "Invalid target property value ~S in file ~S" value file))
            (t
             (set var (second value)))))))

(defun extract-target-properties ()
  (with-safe-io-syntax (:package :xcvb-user)
    (apply #'run-program/read-output-forms
           (query-target-lisp-command (target-properties-form)))))

(defun target-properties-form ()
  (with-safe-io-syntax (:package :xcvb-user)
    (format nil "(format t \"(cl:setf~~%~{ xcvb::~(~A~) '~~S~~%~})~~%\"~{ ~A~})"
            (mapcar #'car *target-properties-variables*)
            (mapcar #'cdr *target-properties-variables*))))

(defun query-target-lisp-command (query-string)
  (assert *lisp-implementation-type*)
  ;; When XCVB is itself compiled with SBCL, and the target is a different SBCL,
  ;; then XCVB's SBCL will export SBCL_HOME which will confuse the target.
  ;; And so, to provide deterministic behavior whatever the build implementation is,
  ;; we unset SBCL_HOME when invoking the target SBCL.
  (append
   (when (eq *lisp-implementation-type* :sbcl) '("env" "-u" "SBCL_HOME"))
   (lisp-invocation-arglist
    :eval (format nil "(progn ~A (finish-output) ~A)"
                  query-string
                  (format nil (slot-value
                               (get-lisp-implementation
                                *lisp-implementation-type*) 'quit-format) 0)))))
