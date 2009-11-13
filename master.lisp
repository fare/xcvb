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
   #:*lisp-allow-debugger*
   #:*object-directory*
   #:*tmp-directory-pathname*
   #:*use-base-image*

   ;;; special variables for XCVB master itself
   #:*disable-cfasls*
   #:*xcvb-path*
   #:*loaded-grains*

   ;;; String utilities
   #:string-prefix-p
   #:string-postfix-p

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
  (or #+sbcl :sbcl #+clisp :clisp #+ccl :ccl #+cmu :cmucl)
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
  "Path to the XCVB binary (a string)")

(defvar *xcvb-path* nil
  "XCVB search path. A string.
Will override the shell variable XCVB_PATH when calling slaves
If you have a list of pathnames and namestrings, you can get a string with
	(format nil \"~@{~A~^:~}\" search-path-as-list)")

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

(defun string-postfix-p (string postfix)
  (let* ((x (string string))
         (y (string postfix))
         (lx (length x))
         (ly (length y)))
    (and (<= ly lx) (string= x y :start1 (- lx ly)))))

(defun string-enclosed-p (prefix string postfix)
  (and (string-prefix-p prefix string)
       (string-postfix-p string postfix)))

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
          (*print-readably* nil)
	  (*read-eval* nil))
      (funcall thunk))))

(defun read-first-file-form (filepath &key (package :cl))
  "Reads the first form from the top of a file"
  (with-safe-io-syntax (:package package)
    (with-open-file (in filepath)
      (read in nil nil))))

#+sbcl
(defun run-program* (&rest args) ;; FIX THAT BUG WITH posix-environ!
  (let ((sb-alien::*default-c-string-external-format* :iso-8859-1))
    (apply 'sb-ext:run-program args)))
    
;;; Simple variant of run-program with no input, and capturing output
(defun run-program/process-output-stream (command output-processor
                                          &key ignore-error-status)
  #+clisp (declare (ignore ignore-error-status))
  #-(or sbcl cmu scl clozure clisp)
  (error "RUN-PROGRAM/PROCESS-OUTPUT-STREAM not implemented for this Lisp")
  (let* ((process
          (#+sbcl run-program*
           #+(or cmu scl clisp) ext:run-program
           #+clozure ccl:run-program
           (car command) #+clisp :arguments (cdr command)
           :input nil :output :stream
           #+(or sbcl cmu scl) :error #+(or sbcl cmu scl) t
           :wait nil
           #+sbcl :search #+sbcl t))
         (stream (#+sbcl sb-ext:process-output
                  #+(or cmu scl) ext:process-output
                  #+clozure ccl::external-process-output
                  #+clisp identity
                  process)))
    (unwind-protect
         (multiple-value-prog1
             (funcall output-processor stream)
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
(defun process-manifest-entry (&key command tthsum pathname &allow-other-keys)
  ;; also source source-tthsum source-pathname
  (unless (and tthsum
               (equal tthsum (cdr (assoc command *manifest* :test #'equal)))
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
       (load pathname))
      (t
       ;; the driver better be loaded by the time any command is issued
       (funcall (find-symbol "RUN-COMMAND" :xcvb-driver) command)))
    (push (cons command tthsum) *manifest*)))

(defun process-manifest (manifest)
  (dolist (entry manifest)
    (apply 'process-manifest-entry entry)))

;;; Extend XCVB driver
(defun initialize-manifest (pathname)
  "XCVB driver extension to initialize the manifest for an image"
  (assert (not *manifest*))
  (setf *manifest* (read-first-file-form pathname)))
(defun load-manifest (pathname)
  "XCVB driver extension to load a list of files from a manifest"
  (process-manifest (read-first-file-form pathname)))

;;; Simplifying options for XCVB invocation
(defun string-option-arguments (string value)
  (when value (list string (let ((*print-case* :downcase)) (princ-to-string value)))))
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
  (let* ((slave-command
          (append
           (list xcvb-binary "slave-builder")
           (string-options
            build setup lisp-implementation verbosity)
           (pathname-options
            xcvb-path output-path object-directory lisp-binary-path)
           (boolean-options
            disable-cfasl base-image profiling)))
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
			(If using SLIME, you might have useful output in your *inferior-lisp* buffer.)"
                     slave-command
                     slave-output)
              (error "XCVB slave failed"))
            (read-from-string
             slave-output t nil
             :start (length +xcvb-slave-greeting+)
             :end (- (length slave-output) (length +xcvb-slave-farewell+)))))
         (*xcvb-verbosity* (+ verbosity 2)))
    (when (>= *xcvb-verbosity* 9)
      (format *error-output* "~&Slave XCVB returned following manifest:~%~A~%" manifest))
    (process-manifest manifest)))

(defun bnl (build &rest keys &key
            xcvb-binary setup xcvb-path output-path object-directory
            lisp-implementation lisp-binary-path
            disable-cfasl base-image verbosity profiling)
  "Short hand for build-and-load"
  (declare (ignore
            xcvb-binary setup xcvb-path output-path object-directory
            lisp-implementation lisp-binary-path
            disable-cfasl base-image verbosity profiling))
  (apply 'build-and-load build keys))
