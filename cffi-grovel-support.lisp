#+xcvb
(module
  (:depends-on ("makefile-backend" "static-traversal" "computations" "driver-commands")))

(in-package :xcvb)

;; TODO: distinguish lisp-module-grain and module-grain,
;; with build-module-grain being a module-grain but not a lisp-module-grain?

;; Do something about cc-flags
(defclass cffi-grovel-lisp-module-grain (lisp-module-grain)
  ((cc-flags :initarg cc-flags :initform nil :reader cc-flags-of)))

(defclass cffi-grovel-results-lisp-module-grain (lisp-module-grain)
  ())

;; Use a generated file, with a (:eval (:call :cffi-grovel :process-grovel-file ...))
;; as the last command to execute?
#|
(defun process-grovel-file (input-file &optional (output-defaults input-file))
  (with-standard-io-syntax
    (let* ((c-file (generate-c-file input-file output-defaults))
           (exe-file (exe-filename c-file))
           (lisp-file (tmp-lisp-filename c-file)))
      (cc-compile-and-link c-file exe-file)
      (invoke exe-file (native-namestring lisp-file))
      lisp-file)))
|#

;;;; asdf-backend
;; export a cffi-grovel:grovel-file component (?)

;;;; dependencies-interpreter

#|
(make-computation
 env
 :inputs ...
  `(:xcvb-driver-command
    (:call :cffi-grovel :process-grovel-file
           ,(grain-pathname cffi-grovel-lisp-module-grain)
           ,(grain-pathname cffi-grovel-results-lisp-module-grain))))
|#
;;;# ASDF component: WRAPPER-FILE
#|
(defclass wrapper-file (asdf:cl-source-file cc-flags-mixin)
  ((soname :initform nil :initarg :soname :accessor soname-of))
  (:documentation
   "This ASDF component defines COMPILE-OP and LOAD-SOURCE-OP
operations that take care of calling PROCESS-WRAPPER-FILE in
order to generate a foreign library and matching CFFI bindings
that are subsequently compiled and/or loaded."))

(defun %perform-process-wrapper-file (op c)
  (let ((fasl-file (ensure-pathname (car (asdf:output-files op c)))))
    (values (process-wrapper-file (asdf:component-pathname c)
                                  fasl-file
                                  (or (soname-of c)
                                      (asdf:component-name c)))
            fasl-file)))

(defmethod asdf:perform ((op asdf:compile-op) (c wrapper-file))
  (multiple-value-bind (generated-source-file fasl-file)
      (%perform-process-wrapper-file op c)
    (compile-file generated-source-file
                  :output-file fasl-file
                  #+ecl :system-p #+ecl t)))

(defmethod asdf:perform ((op asdf:load-source-op) (c wrapper-file))
  (load (%perform-process-wrapper-file op c)))
|#
