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
     . "(or #+sbcl (namestring(sb-int:sbcl-homedir-pathname)) #+ccl (namestring(ccl::ccl-directory)))")
    (*target-lisp-image-pathname*
     . "(or #+clozure(namestring ccl:*HEAP-IMAGE-NAME*) #+sbcl(namestring sb-ext:*core-pathname*))")
    (*target-lisp-executable-pathname*
     . "(or #+sbcl (symbol-value (find-symbol \"*RUNTIME-PATHNAME*\" :sb-ext))
            #+(and linux (or sbcl clozure)) (namestring(truename\"/proc/self/exe\"))
            #+(and clisp linux) (read-line (run-program \"readlink\" :arguments (list (format nil \"/proc/~A/exe\" (LINUX:getpid))) :output :stream)))"))
    "alist of variables and how to compute them in the target system")

(defun target-system-features ()
  (get-target-properties)
  *target-system-features*)

(defun get-target-properties ()
  (unless *target-properties*
    (read-target-properties)))

(defun read-target-properties ()
  (let ((forms (extract-target-properties)))
    (unless forms
      (error "Failed to extract properties from your Lisp implementation"))
    (log-format 7 "Information from target Lisp:~% ~S" forms)
    (unless (and (list-of-length-p 1 forms)
                 (consp (car forms)) (eq 'setf (caar forms)))
      (error "Malformed target properties"))
    (setf *target-properties* (cdar forms))
    (loop :for (var value) :on *target-properties* :by #'cddr :do
          (cond
            ((not (member var *target-properties-variables* :key #'car))
             (error "Invalid target property ~S" var))
            ((not (and (list-of-length-p 2 value) (eq 'quote (car value))))
             (error "Invalid target property value ~S" value))
            (t
             (set var (second value)))))))

(defun extract-target-properties ()
  (with-safe-io-syntax (:package :xcvb-user)
    (let ((command (query-target-lisp-command (target-properties-form))))
      (log-format 8 "Extract information from target Lisp:~% ~S" command)
      (run-program/read-output-forms command))))

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
  ;;
  ;; On Linux, we append ("env" "-u" "SBCL_HOME") to the arglist
  ;; when the target implementation type was sbcl, and
  ;; whichever implementation XCVB itself was using,
  ;; so the semantics of XCVB would not depend on said implementation,
  ;; but -u is apparently a GNU extension not available on other systems
  ;; (most notably BSD systems including OS X),
  ;; so we'd instead resort to unsetting SBCL_HOME when the XCVB implementation is sbcl
  ;; (in main.lisp)
  ;; Except that sb-posix:putenv is broken before SBCL 1.0.33.21 (SBCL bug 460455).
  (append
   #+(and sbcl (or linux cygwin)) '("env" "-u" "SBCL_HOME")
   (lisp-invocation-arglist
    :eval (format nil "(progn ~A (finish-output) ~A)"
                  query-string (quit-form :code 0)))))
