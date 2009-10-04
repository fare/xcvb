;;; XCVB master to call XCVB from a Lisp image and load the results.

#|
The current plan for in-image XCVB use (beside having users use the ASDF backend) is that
xcvb-master will run-program an XCVB process that would do all the compilation out-of-image
then tell you what to load, and how to update your configuration.
XCVB will do all the tth checksumming on its side, so you don't have to have ironclad
in your image -- or anything beside xcvb-master,
which ought to be a small standalone Lisp file, smaller than ASDF.

So xcvb-master would::
     xcvb eval '(xcvb:slave-build ...)'
where the form would including a specification of modules already loaded
(i.e. fullname and hashvalue of each included component),
and the subprocess would return a specification of modules to load:
fullname, hashvalue and current pathname.
After loading, a further call to xcvb would allow it to cleanup any
theretofore unneeded temporary file.
|#

#+xcvb
(module
 (:author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT" ;; MIT-style license. See LICENSE
  :description "XCVB Master"))

(in-package :cl)


(defpackage :xcvb-master
  (:nicknames :xcvbm)
  (:use :cl)
  (:export

   ;;; special variables
   #:*lisp-implementation-type*
   #:*lisp-executable-pathname*
   #:*lisp-image-pathname*
   #:*lisp-implementation-directory*
   #:*disable-cfasls*
   #:*xcvb-verbosity*
   #:*search-path*
   #:*lisp-allow-debugger*
   #:*object-directory*
   #:*tmp-directory-pathname*
   #:*use-base-image*
   #:*xcvb-path*

   ;;; run-program/foo
   #:run-program/process-output-stream
   #:run-program/read-output-lines
   #:run-program/read-output-string
   #:slurp-stream-to-string
   #:copy-stream-to-stream
   #:run-program/read-output-form
   #:run-program/read-output-forms
   #:read-many

   ;;; Using an inferior XCVB
   #:build-and-load))

(in-package :xcvb-master)

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

(defvar *disable-cfasls* nil
  "Should we disable CFASL support when the target Lisp has it?")

(defvar *xcvb-verbosity* 5
  "Level of verbosity of XCVB:
0 - quiet
5 - usual warnings
9 - plenty of debug info")

(defvar *search-path* '()
  "Path to search for XCVB modules")

#|
(defvar *xcvb-path* nil
  "XCVB Search path override")
|#

(defvar *lisp-allow-debugger* nil
  "Should we allow interactive debugging of failed build attempts?")

(defvar *object-directory* "obj"
  "where to store object files")

(defvar *tmp-directory-pathname* #p"/tmp/"
  "pathname of directory where to store temporary files")

(defvar *use-base-image* t
  "Should we be using a base image for all builds?")


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
        (#+sbcl sb-ext:process-wait
         #+(or cmu scl) ext:process-wait
         #+clozure ccl::external-process-wait
         #+clisp close
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
   (lambda (pos)
     (loop :for l = (read-line pos nil nil)
       :while l :collect l))))

(defun run-program/read-output-string (command &rest args)
  (run-program/process-output-stream
   command args #'slurp-stream-to-string))

(defun slurp-stream-to-string (input)
  (with-output-to-string (output)
    (copy-stream-to-stream input output :element-type 'character)))

(defun copy-stream-to-stream (input output &key (element-type 'character))
  (loop :for buffer = (make-array 8192 :element-type element-type)
    :for end = (read-sequence buffer input)
    :until (zerop end) :do (write-sequence buffer output :end end)))

(defun run-program/read-output-form (command &rest args)
  (run-program/process-output-stream command args #'read))

(defun run-program/read-output-forms (command &rest args)
  (run-program/process-output-stream command args #'read-many))

(defun read-many (s)
  (loop :with eof = '#:eof
    :for form = (read s nil eof)
    :until (eq form eof)
    :collect form))

#|
(defun build-and-load
    (build &key
     setup
     xcvb-path
     output-path
     object-directory
     lisp-implementation
     lisp-binary-path
     disable-cfasl
     base-image
     verbosity
     profiling)
  nil);;...
|#
