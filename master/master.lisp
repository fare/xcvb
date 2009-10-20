;;; XCVB master: call XCVB from a Lisp image and load the results.

#+xcvb
(module
 (:description "XCVB Master"
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT" ;; MIT-style license. See LICENSE
  :build-depends-on nil))

(in-package :cl)
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
   #:*xcvb-verbosity*
   #:*search-path*
   #:*lisp-allow-debugger*
   #:*object-directory*
   #:*tmp-directory-pathname*
   #:*use-base-image*

   ;;; special variables for XCVB master itself
   #:*disable-cfasls*
   #:*xcvb-path*
   #:*loaded-grains*

   ;;; I/O utilities
   #:slurp-stream-to-string
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

   ;;; Using an inferior XCVB
   #:load-grain #:load-grains
   #:build-and-load #:bnl))

(in-package :xcvb-master)

;;; These variables are shared with XCVB itself.
(defvar *lisp-implementation-type*
  (or #+sbcl :sbcl #+clisp :clisp #+ccl :ccl #+cmu :cmucl)
  "Type of Lisp implementation for the target system.
  Default: same as XCVB itself.")

(defvar *lisp-executable-pathname* nil
  "Path to the Lisp implementation to use for the target system.
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
  "What options do we need invoke the target Lisp with?")

(defvar *disable-cfasls* nil
  "Should we disable CFASL support when the target Lisp has it?")

(defvar *xcvb-verbosity* 5
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
  "Path to the XCVB binary")

(defvar *xcvb-path* nil
  "XCVB Search path override")

(defvar *xcvb-setup* nil
  "Lisp file to load to setup the target build system, if any")

(defvar *loaded-grains* nil
  ;; Note that older versions are kept in the tail, documenting the load history,
  ;; without affecting the behavior of ASSOC on the alist.
  "an alist of fullname of the XCVB grains loaded in the current image, with tthsum")

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

(defun slurp-stream-to-string (input)
  (with-output-to-string (output)
    (copy-stream-to-stream input output :element-type 'character)))

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
	  (*read-eval* nil))
      (funcall thunk))))

(defun read-first-file-form (filepath &key (package :cl))
  "Reads the first form from the top of a file"
  (with-safe-io-syntax (:package package)
    (with-open-file (in filepath)
      (read in nil nil))))

;;; Simple variant of run-program with no input, and capturing output
(defun run-program/process-output-stream (command arguments output-processor)
    #+(or clozure sbcl cmu scl clisp)
    (let* ((process
            (#+sbcl sb-ext:run-program
             #+(or cmu scl clisp) ext:run-program
             #+clozure ccl:run-program
             command #-clisp arguments #+clisp :arguments #+clisp arguments
             :input nil :output :stream
             #+(or sbcl cmu scl) :error #+(or sbcl cmu scl) t
             :wait nil
             #+sbcl :search #+sbcl t))
           (stream (#+sbcl sb-ext:process-output
                    #+(or cmu scl) ext:process-output
                    #+clozure ccl::external-process-output
                    #+clisp identity
                    process)))
      (multiple-value-prog1
          (funcall output-processor stream)
        (close stream)
        #-clisp
        (#+sbcl sb-ext:process-wait
         #+(or cmu scl) ext:process-wait
         #+clozure ccl::external-process-wait
         process)
        #-clisp
        (let ((return-code
               (#+sbcl sb-ext:process-exit-code
                #+(or cmu scl) ext:process-exit
                #+clozure (lambda (p) (nth-value 1 (ccl:external-process-status p)))
                process)))
          (unless (zerop return-code)
            (cerror "ignore error code~*~*"
                    "Process ~S exited with error code ~D" (cons command arguments) return-code)))))
    #-(or sbcl cmu scl clozure clisp)
    (error "RUN-PROGRAM/PROCESS-OUTPUT-STREAM not implemented for this Lisp"))

(defun run-program/read-output-lines (command &rest args)
  (run-program/process-output-stream
   command args
   (lambda (stream)
     (loop :for l = (read-line stream nil nil)
       :while l :collect l))))

(defun run-program/read-output-string (command &rest args)
  (run-program/process-output-stream
   command args #'slurp-stream-to-string))

(defun run-program/read-output-form (command &rest args)
  (run-program/process-output-stream command args #'read))

(defun run-program/read-output-forms (command &rest args)
  (run-program/process-output-stream command args #'read-many))

;;; Maintaining memory of which grains have been loaded in the current image.
(defun load-grain (&key command fullname tthsum pathname &allow-other-keys)
  ;; also source source-tthsum source-pathname
  (unless (and tthsum (equal tthsum (cdr (assoc fullname *loaded-grains* :test #'equal))))
    (cond
      (command
       (when (>= *xcvb-verbosity* 7)
         (format *error-output* "~&Executing xcvb-driver command ~S~%" command))
       ;; the driver better be loaded by the time any command is issued
       (funcall (find-symbol "RUN-COMMAND" :xcvb-driver) command))
      (t
       (when (>= *xcvb-verbosity* 7)
         (format *error-output* "~&Loading grain ~S (tthsum: ~A) from ~S~%"
                 fullname tthsum pathname))
       (load pathname)))
    (push (cons fullname tthsum) *loaded-grains*)))
(defun load-grains (manifest)
  (dolist (grain-spec manifest) (apply #'load-grain grain-spec)))

;;; Extend XCVB driver
(defun initialize-manifest (pathname)
  "XCVB driver extension to initialize the manifest for an image"
  (assert (not *loaded-grains*))
  (setf *loaded-grains* (read-first-file-form pathname)))
(defun load-manifest (pathname)
  "XCVB driver extension to load a list of files from a manifest"
  (load-grains (read-first-file-form pathname)))

;;; Simplifying options for XCVB invocation
(defun string-option-arguments (string value)
  (when value (list string (princ-to-string value))))
(defun pathname-option-arguments (string value)
  (when value (list string (namestring value))))
(defun boolean-option-arguments (string value)
  (when value (list string)))
(defmacro option-string (name)
  (format nil "--~(~a~)" name))
(defmacro string-option (var)
  `(string-option-arguments (option-string ,var) ,var))
(defmacro string-options (&rest vars)
  `(append ,@(loop :for var :in vars :collect `(string-option ,var))))
(defmacro pathname-option (var)
  `(pathname-option-arguments (option-string ,var) ,var))
(defmacro pathname-options (&rest vars)
  `(append ,@(loop :for var :in vars :collect `(pathname-option ,var))))
(defmacro boolean-option (var)
  `(boolean-option-arguments (option-string ,var) ,var))
(defmacro boolean-options (&rest vars)
  `(append ,@(loop :for var :in vars :collect `(boolean-option ,var))))

;;; Run a slave, obey its orders.
(defun build-and-load
    (build &key
     (xcvb-binary *xcvb-binary*)
     (setup *xcvb-setup*)
     (xcvb-path *xcvb-path*)
     output-path
     (object-directory *object-directory*)
     (lisp-implementation *lisp-implementation-type*)
     (lisp-binary-path *lisp-executable-pathname*)
     (disable-cfasl *disable-cfasls*)
     (base-image *use-base-image*)
     (verbosity *xcvb-verbosity*)
     profiling)
  (let* ((forms
          (with-safe-io-syntax ()
            (apply #'run-program/read-output-forms xcvb-binary "slave-builder"
                   (append
                    (string-options
                     build setup lisp-implementation verbosity)
                    (pathname-options
                     xcvb-path output-path object-directory lisp-binary-path)
                    (boolean-options
                     disable-cfasl base-image profiling))))))
    (unless (and forms (consp forms) (null (cdr forms))
                 (consp (car forms)) (eq (caar forms) :xcvb))
      (error "XCVB subprocess failed."))
    (destructuring-bind (requires manifest) (cdar forms)
      (map () #'require requires)
      (let ((*xcvb-verbosity* (+ *xcvb-verbosity* 2)))
        (load-grains manifest)))))

(defun bnl (build &rest keys &key
            xcvb-binary setup xcvb-path output-path object-directory
            lisp-implementation lisp-binary-path
            disable-cfasl base-image verbosity profiling)
  "Short hand for build-and-load"
  (declare (ignore
            xcvb-binary setup xcvb-path output-path object-directory
            lisp-implementation lisp-binary-path
            disable-cfasl base-image verbosity profiling))
  (apply #'build-and-load build keys))
