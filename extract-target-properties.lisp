;;; Extracting properties from the target lisp implementation
#+xcvb (module (:depends-on ("specials" "lisp-invocation" "string-escape")))

(in-package :xcvb)

(defvar *target-properties* ()
  "Properties of the target system")

(defparameter *target-properties-variables*
  '((*use-cfasls*
     . "(or #+sbcl (and (find-symbol \"*EMIT-CFASL*\" \"SB-C\") t))")
    (*fasl-type*
     . "(pathname-type (compile-file-pathname \"foo.lisp\"))")
    (*target-asdf-version* . "(when(member :asdf2 *features*) (funcall (find-symbol(string :asdf-version):asdf)))")
    (*implementation-identifier*
     . "(when(member :asdf2 *features*) (funcall(find-symbol(string :implementation-identifier):asdf)))")
    (*target-system-features*
     ;; lispworks 6.0 on linux/386 has HARP::PC386 HARP::PC386-X, so remove bad features
     . "(remove-if-not 'keywordp *features*)")
    (*target-can-dump-image-p*
     . "(or #+(or allegro ccl clisp cmu gcl (and lispworks (not lispworks-personal-edition)) sbcl scl) t)") ;NO: abcl cormanlisp ecl lispworks-personal-edition mcl xcl
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

(defun target-feature-p (x)
  (get-target-properties)
  (cond
    ((member x *target-suppressed-features*)
     nil)
    ((member x *target-added-features*)
     t)
    ((member x *target-system-features*)
     t)
    (t
     nil)))

(defun tweak-features-around-eval-string (eval)
  (if (or *target-added-features* *target-suppressed-features*)
      (format nil "(progn~
  ~{~#[~;(pushnew ~S *features*)~:;(dolist(x'(~@{~S~^ ~}))(pushnew x *features*))~]~}~
  ~@[(setf *features*(remove~{~#[~; ~S~:;-if(lambda(x)(member x'(~@{~S~^ ~})))~]~} *features*))~]~
  ~A)"
              *target-added-features* *target-suppressed-features* eval)
      eval))

(defun read-target-properties ()
  (let ((string (extract-target-properties)))
    (log-format-pp 7 "Information from target Lisp:~%~A" string)
    (with-safe-io-syntax (:package :xcvb-user)
      (let ((forms (with-input-from-string (s string) (slurp-stream-forms s))))
        (unless forms
          (user-error "Failed to extract properties from your Lisp implementation"))
        (unless (and (list-of-length-p 1 forms)
                     (consp (car forms)) (eq 'setf (caar forms)))
          (user-error "Malformed target properties:~%~A" string))
        (setf *target-properties* (cdar forms))))
    (loop :for (var value) :on *target-properties* :by #'cddr :do
      (cond
        ((not (member var *target-properties-variables* :key #'car))
         (user-error "Invalid target property ~S" var))
        ((not (and (list-of-length-p 2 value) (eq 'quote (car value))))
         (user-error "Invalid target property value ~S" value))
        (t
         (set var (second value)))))
    (case *lisp-implementation-type*
      (:sbcl (setenv "SBCL_HOME" *lisp-implementation-directory*)))))

(defun extract-target-properties-via-pipe ()
  (with-safe-io-syntax (:package :xcvb-user)
    (let ((command (query-target-lisp-command (target-properties-form))))
      (log-format-pp 8 "Extract information from target Lisp:~% ~S" command)
      (handler-case
          (run-program/ command :output :string)
        (t (c) (user-error "Failed to extract properties from target Lisp:~%~
		       Command:~S~%Error:~%~A~%" command c))))))

(defun extract-target-properties-via-tmpfile ()
  (with-temporary-file (:pathname pn :prefix (format nil "~Axtp" *temporary-directory*))
    (with-safe-io-syntax (:package :xcvb-user)
      (let ((command (query-target-lisp-command
                      (format nil "(with-open-file (*standard-output* ~S :direction :output :if-exists :overwrite :if-does-not-exist :error) ~A)"
                              pn (target-properties-form))))
            (stdout nil))
        (log-format-pp 8 "Extract information from target Lisp:~% ~S" command)
        (flet ((slurp (&key if-does-not-exist)
                 (with-open-file (s pn :direction :input :if-does-not-exist if-does-not-exist)
                   (when s (slurp-stream-string s)))))
          (handler-case
              (progn
                (setf stdout (run-program/ command :output :string))
                (slurp :if-does-not-exist :error))
            (t (c) (user-error "Failed to extract properties from target Lisp:~%~
			   Condition: ~A~%Command:~S~%~@[stdout:~%~A~%~]~@[Output:~%~A~%~]"
                          c command stdout (slurp)))))))))

(defun extract-target-properties ()
  (case *lisp-implementation-type*
    ((:clisp :gcl :xcl :lispworks)
     (extract-target-properties-via-tmpfile))
    (otherwise
     (extract-target-properties-via-pipe))))

(defun target-properties-form ()
  (with-safe-io-syntax (:package :xcvb-user)
    (format nil "(format t \"(cl:setf~~%~{ xcvb::~(~A~) '~~S~~%~})~~%\"~{ ~A~})"
            (mapcar #'car *target-properties-variables*)
            (mapcar #'cdr *target-properties-variables*))))

(defun get-asdf-pathname ()
  (let ((build (registered-build "/asdf")))
    (when (typep build 'build-module-grain)
      (subpathname (grain-pathname build) "asdf.lisp"))))

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
  ;; (most notably BSD systems, including OS X),
  ;; so we'd instead resort to unsetting SBCL_HOME when the XCVB implementation is sbcl
  ;; (in main.lisp)
  ;; Except that sb-posix:putenv is broken before SBCL 1.0.33.21 (SBCL bug 460455).
  (append
   ;;#+(and sbcl (or linux cygwin)) '("env" "-u" "SBCL_HOME") ; we now rely on a recent SBCL
   (lisp-invocation-arglist
    :cross-compile t
    :eval (format nil "(progn (setf *load-verbose* nil *print-readably* nil)~
 (ignore-errors (require \"asdf\"))~
 (handler-bind ((style-warning #'muffle-warning))~
   (ignore-errors
    (funcall (find-symbol(string'oos):asdf) (find-symbol(string'load-op):asdf) :asdf :verbose nil)))~
 ~@[(unless (member :asdf2 *features*) (load ~S))~] ~A (finish-output) ~A)"
                  (get-asdf-pathname) query-string (quit-form :code 0)))))
