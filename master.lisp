;;; XCVB master: call XCVB from a Lisp image and load the results.

#+xcvb
(module
 (:description "XCVB Master"
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT" ;; MIT-style license. See LICENSE
  :build-depends-on nil))

(cl:in-package :cl)
(declaim (optimize (speed 2) (safety 3) (compilation-speed 0) (debug 3))
         #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

(defpackage :xcvb-master
  (:nicknames :xcvbm)
  (:use :cl)
  (:export

   ;;; special variables shared with XCVB itself
   #:*lisp-implementation-type*
   #:*lisp-executable-pathname*
   #:*lisp-image-pathname*
   #:*lisp-implementation-directory*
   #:*lisp-flags*
   #:*features-defined* #:*features-undefined*
   #:*xcvb-verbosity*
   #:*lisp-allow-debugger*
   #:*object-directory*
   #:*tmp-directory-pathname*
   #:*use-base-image*

   ;;; special variables for XCVB master itself
   #:*disable-cfasls*
   #:*source-registry*
   #:*loaded-grains*

   ;;; String utilities - also in fare-utils
   ;#:string-prefix-p
   ;#:string-suffix-p
   ;#:string-enclosed-p

   ;;; I/O utilities
   #:slurp-stream-string
   #:slurp-stream-lines
   #:copy-stream-to-stream
   #:copy-stream-to-stream-line-by-line
   #:read-many
   #:with-safe-io-syntax
   #:read-first-file-form

   ;;; run-program/foo
   #:run-program/process-output-stream
   #:run-program/read-output-lines
   #:run-program/read-output-string
   #:run-program/read-output-form
   #:run-program/read-output-forms

   ;; Magic strings
   #:+xcvb-slave-greeting+
   #:+xcvb-slave-farewell+

   ;;; Using an inferior XCVB
   #:load-grain #:load-grains
   #:build-and-load #:bnl))

(in-package :xcvb-master)

;;; These variables are shared with XCVB itself.
(defvar *lisp-implementation-type*
  (or #+sbcl :sbcl #+clisp :clisp #+clozure :ccl #+cmu :cmucl #+ecl :ecl
      (error "Your Lisp implementation is not supported by XCVB master (yet)."))
  "Type of Lisp implementation for the target system. A keyword.
  Default: same as XCVB itself.")

(defvar *lisp-executable-pathname* nil
  "Path to the Lisp implementation to use for the target system.
  NIL, or a string.
  Default: what's in your PATH.")

(defvar *lisp-image-pathname* nil
  "What path to a Lisp image do we need invoke the target Lisp with?
  Default: whatever's the default for your implementation.")

(defvar *lisp-implementation-directory*
  (or #+sbcl (namestring (sb-int:sbcl-homedir-pathname)))
  "Where is the home directory for the Lisp implementation,
  in case we need it to (require ...) special features?
  Default: whatever's the default for your implementation.")

(defvar *lisp-flags* :default
  ;;; TODO: add support for overriding this feature at the command-line?
  "What options do we need invoke the target Lisp with?
A list of strings, or the keyword :DEFAULT.")

(defvar *features-defined* nil
  "What additional features to define in the target image")

(defvar *features-undefined* nil
  "What additional features to undefine in the target image")

(defvar *disable-cfasls* nil
  "Should we disable CFASL support when the target Lisp has it?")

(defvar *xcvb-verbosity* 10
  "Level of verbosity of XCVB:
  0 - quiet
  5 - usual warnings
  9 - plenty of debug info")

(defvar *lisp-allow-debugger* nil
  "Should we allow interactive debugging of failed build attempts?")

(defvar *object-directory* "obj"
  "where to store object files")

(defvar *tmp-directory-pathname* #p"/tmp/"
  "pathname of directory where to store temporary files")

(defvar *use-base-image* t
  "Should we be using a base image for all builds?")

;;; These variables are specific to XCVB master.
(defvar *xcvb-binary* "xcvb"
  "Path to the XCVB binary (a string)")

(defvar *source-registry* nil
  "CL source registry specification. A sexp or string.
Will override the shell variable CL_SOURCE_REGISTRY when calling slaves.")

(defvar *xcvb-setup* nil
  "Lisp file to load to setup the target build system, if any")

(defvar *manifest* nil
  ;; Note that older versions are kept in the tail, documenting the command history,
  ;; without affecting the behavior of ASSOC on the alist.
  "an alist of the XCVB load commands executed in this image,
with associated pathnames and tthsums.")

;;; Magic strings. Do not change. Constants, except we can't portably use defconstant here.
(defvar +xcvb-slave-greeting+ #.(format nil "XCVB-SLAVE~%"))
(defvar +xcvb-slave-farewell+ #.(format nil "~%Your desires are my orders~%"))

;;; String utilities
(defun string-prefix-p (prefix string)
  (let* ((x (string prefix))
         (y (string string))
         (lx (length x))
         (ly (length y)))
    (and (<= lx ly) (string= x y :end2 lx))))

(defun string-suffix-p (string suffix)
  (let* ((x (string string))
         (y (string suffix))
         (lx (length x))
         (ly (length y)))
    (and (<= ly lx) (string= x y :start1 (- lx ly)))))

(defun string-enclosed-p (prefix string suffix)
  (and (string-prefix-p prefix string)
       (string-suffix-p string suffix)))

;;; I/O utilities
(defun copy-stream-to-stream (input output &key (element-type 'character))
  (loop :with length = 8192
    :for buffer = (make-array length :element-type element-type)
    :for end = (read-sequence buffer input)
    :until (zerop end)
    :do (write-sequence buffer output :end end)
    :do (when (< end length) (return))))

(defun copy-stream-to-stream-line-by-line (input output)
  (loop :for (line eof) = (multiple-value-list (read-line input nil nil))
    :while line
    :do (progn
          (princ line output)
          (unless eof (terpri output))
          (finish-output output)
          (when eof (return)))))

(defun slurp-stream-string (input)
  (with-output-to-string (output)
    (copy-stream-to-stream input output :element-type 'character)))

(defun slurp-stream-lines (input)
  (loop :for l = (read-line input nil nil)
    :while l :collect l))

(defun read-many (s)
  (loop :with eof = '#:eof
    :for form = (read s nil eof)
    :until (eq form eof)
    :collect form))

(defmacro with-safe-io-syntax ((&key (package :cl)) &body body)
  `(call-with-safe-io-syntax (lambda () ,@body) :package ,package))
(defun call-with-safe-io-syntax (thunk &key (package :cl))
  (with-standard-io-syntax ()
    (let ((*package* (find-package package))
          (*print-readably* t)
          (*print-escape* t)
	  (*read-eval* nil))
      (funcall thunk))))

(defun read-first-file-form (filepath &key (package :cl))
  "Reads the first form from the top of a file"
  (with-safe-io-syntax (:package package)
    (with-open-file (in filepath)
      (read in nil nil))))

;;; Simple variant of run-program with no input, and capturing output
(defun run-program/process-output-stream (command output-processor
                                          &key ignore-error-status)
  #+(or clisp ecl) (declare (ignorable ignore-error-status))
  (let* ((p (find-package :quux-iolib))
         (r 'run-program/process-output-stream)
         (s (and p (find-symbol (string r) p))))
    (when s (return-from run-program/process-output-stream
              (funcall s command output-processor :ignore-error-status ignore-error-status))))
  #-(or sbcl cmu scl clozure clisp)
  (error "RUN-PROGRAM/PROCESS-OUTPUT-STREAM not implemented for this Lisp")
  (let* ((process
          (#+sbcl sb-ext:run-program
           #+(or clisp cmu ecl scl) ext:run-program
           #+clozure ccl:run-program
           (car command) #+clisp :arguments (cdr command)
           :input nil :output :stream
           #+(or clozure cmu ecl sbcl scl) :error #+(or clozure cmu ecl sbcl scl) t
           :wait nil
           #+sbcl :search #+sbcl t))
         (stream (#+sbcl sb-ext:process-output
                  #+(or cmu scl) ext:process-output
                  #+clozure ccl::external-process-output
                  #+(or clisp ecl) identity
                  process)))
    (unwind-protect
         (multiple-value-prog1
             (funcall output-processor stream)
           #-(or clisp ecl)
           (#+sbcl sb-ext:process-wait
            #+(or cmu scl) ext:process-wait
            #+clozure ccl::external-process-wait
            process)
           #-(or clisp ecl)
           (let ((return-code
                  (#+sbcl sb-ext:process-exit-code
                   #+(or cmu scl) ext:process-exit
                   #+clozure (lambda (p) (nth-value 1 (ccl:external-process-status p)))
                   process)))
             (unless (or ignore-error-status (zerop return-code))
               (cerror "ignore error code~*~*"
                       "Process ~S exited with error code ~D"
                       command return-code))))
      (close stream))))

(defun run-program/read-output-lines (command &rest keys)
  (apply 'run-program/process-output-stream command
         'slurp-stream-lines keys))

(defun run-program/read-output-string (command &rest keys)
  (apply 'run-program/process-output-stream command
         'slurp-stream-string keys))

(defun run-program/read-output-form (command &rest keys)
  (apply 'run-program/process-output-stream command
         'read keys))

(defun run-program/read-output-forms (command &rest keys)
  (apply 'run-program/process-output-stream command
         'read-many keys))

;;; Maintaining memory of which grains have been loaded in the current image.
(defun process-manifest-entry (&rest entry &key command tthsum pathname &allow-other-keys)
  ;; also source source-tthsum source-pathname
  (unless (and tthsum
               (equal tthsum
                      (getf (find command *manifest* :test #'equal
                                  :key (lambda (x) (getf x :command)))
                            :tthsum))
               (progn
                 (when (>= *xcvb-verbosity* 8)
                   (format *error-output* "~&Skipping XCVB command ~S ~@[from already loaded file ~S (tthsum: ~A)~]~%"
                           command pathname tthsum))
                 t))
    (when (>= *xcvb-verbosity* 7)
      (format *error-output* "~&Executing XCVB command ~S ~@[from ~S (tthsum: ~A)~]~%"
              command pathname tthsum))
    (cond
      (pathname
       (assert (and (consp command) (eq :load-file (car command))
                    (consp (cdr command)) (null (cddr command))))
       (load pathname :verbose (>= *xcvb-verbosity* 8) :print (>= *xcvb-verbosity* 9)))
      (t
       ;; the driver better be loaded by the time any command is issued
       (funcall (find-symbol (string '#:run-command) :xcvb-driver) command)))
    (push entry *manifest*)))

(defun process-manifest (manifest)
  (dolist (entry manifest)
    (apply 'process-manifest-entry entry)))

;;; Extend XCVB driver
(defun initialize-manifest (pathname)
  "XCVB driver extension to initialize the manifest for an image"
  (assert (not *manifest*))
  (setf *manifest* (reverse (read-first-file-form pathname))))
(defun load-manifest (pathname)
  "XCVB driver extension to load a list of files from a manifest"
  (process-manifest (read-first-file-form pathname)))

;;; Run a slave, obey its orders.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *bnl-keys-with-defaults*
    '((xcvb-binary *xcvb-binary*)
      (setup *xcvb-setup*)
      (source-registry *source-registry*)
      (output-path nil)
      (object-directory *object-directory*)
      (lisp-implementation *lisp-implementation-type*)
      (lisp-binary-path *lisp-executable-pathname*)
      (lisp-image-path *lisp-image-pathname*)
      (features-defined *features-defined*)
      (features-undefined *features-undefined*)
      (disable-cfasl *disable-cfasls*)
      (use-base-image *use-base-image*)
      (verbosity *xcvb-verbosity*)
      (debugging *lisp-allow-debugger*)
      (profiling nil)))
  (defparameter *bnl-keys* (mapcar #'car *bnl-keys-with-defaults*)))

(defun build-slave-command-line (build &key . #.*bnl-keys-with-defaults*)
  (flet ((list-option-arguments (string values)
           (loop
             :for value :in values
             :nconc (list string value))))
    (macrolet
        ((to-option-name (name)
                 (format nil "--~(~a~)" name))
         (pathname-option (var)
           `(when ,var
              (list (to-option-name ,var) (namestring ,var))))
         (string-option (var)
           `(when ,var
              (list (to-option-name ,var) (let ((*print-case* :downcase))
                                            (princ-to-string ,var)))))
         (boolean-option (var)
           `(when ,var
              (list (to-option-name ,var))))
         (pluralize (wrapper &rest vars)
           `(append ,@(loop :for var :in vars :collect `(,wrapper ,var))))
         (string-options (&rest vars)
           `(pluralize string-option ,@vars))
         (pathname-options (&rest vars)
           `(pluralize pathname-option ,@vars))
         (boolean-options (&rest vars)
           `(pluralize boolean-option ,@vars)))
      (append
       (list xcvb-binary "slave-builder")
       (string-options build setup lisp-implementation verbosity source-registry)
       (pathname-options output-path object-directory lisp-binary-path lisp-image-path)
       (list-option-arguments "define-feature" features-defined)
       (list-option-arguments "undefine-feature" features-undefined)
       (boolean-options disable-cfasl use-base-image debugging profiling)))))

(defun build-and-load (build &rest args &key . #.*bnl-keys*)
  (declare (ignore . #.(remove 'verbosity *bnl-keys*)))
  (let* ((slave-command (apply 'build-slave-command-line build args))
         (slave-output
          (with-safe-io-syntax ()
            (run-program/read-output-string slave-command :ignore-error-status t)))
         (manifest
          (progn
            (unless (and slave-output
                         (string-enclosed-p
                          +xcvb-slave-greeting+ slave-output +xcvb-slave-farewell+))
              (format *error-output*
                     "Failed to execute a build slave.~%~
			Slave command:~%  ~S~%~
			Slave output:~%~A~%~
			(If using SLIME, you might have useful error output in your *inferior-lisp* buffer.)"
                     slave-command
                     slave-output)
              (error "XCVB slave failed"))
            (read-from-string
             slave-output t nil
             :start (length +xcvb-slave-greeting+)
             :end (- (length slave-output) (length +xcvb-slave-farewell+)))))
         (*xcvb-verbosity* (+ (or verbosity *xcvb-verbosity*) 2)))
    (when (>= *xcvb-verbosity* 9)
      (format *error-output* "~&Slave XCVB returned following manifest:~%~S~%" manifest))
    (process-manifest manifest)))

(defun bnl (build &rest keys &key . #.*bnl-keys*)
  "Short hand for build-and-load"
  (declare (ignore . #.*bnl-keys*))
  (apply 'build-and-load build keys))
