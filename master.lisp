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

   ;;; String utilities - copied from fare-utils
   ;#:string-prefix-p
   ;#:string-suffix-p
   ;#:string-enclosed-p

   ;;; I/O utilities
   ;#:with-output ; copied from fare-utils
   #:slurp-stream-string
   #:slurp-stream-lines
   #:copy-stream-to-stream
   #:copy-stream-to-stream-line-by-line
   #:read-many
   #:with-safe-io-syntax
   #:read-first-file-form

   ;;; Escaping the command invocation madness
   #:escape-windows-token
   #:escape-windows-command
   #:escape-sh-token
   #:escape-sh-command
   #:escape-token
   #:escape-command

   ;;; run-program/foo
   #:run-program/process-output-stream
   #:run-program/read-output-lines
   #:run-program/read-output-string
   #:run-program/read-output-form
   #:run-program/read-output-forms
   #:run-program/echo-output

   ;; Magic strings
   #:+xcvb-slave-greeting+
   #:+xcvb-slave-farewell+

   ;;; Using an inferior XCVB
   #:load-grain #:load-grains
   #:build-and-load #:bnl))

(in-package :xcvb-master)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or unix cygwin)
  (pushnew :os-unix *features*))
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and (or win32 windows mswindows mingw32) (not os-unix))
  (pushnew :os-windows *features*))
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and os-unix os-windows)
  (error "Your operating system is simultaneously Unix and Windows? Congratulations.~%~
Now fix XCVB for it assumes that's impossible")
  #-(or os-unix os-windows)
  (error "Congratulations for trying XCVB on an operating system~%~
that is neither Unix, nor Windows.~%Now you port it."))

(defun getenv (x)  ;; from xcvb-driver, to use $TMP or %TEMP%
  #-(or abcl allegro cmu clisp clozure ecl gcl lispworks sbcl scl)
  (error "GETENV not supported for your Lisp implementation")
  (#+(or abcl clisp xcl) ext:getenv
   #+allegro sys:getenv
   #+clozure ccl:getenv
   #+(or cmu scl) (lambda (x) (cdr (assoc x ext:*environment-list* :test #'string=)))
   #+ecl si:getenv
   #+gcl system:getenv
   #+lispworks lispworks:environment-variable
   #+sbcl sb-ext:posix-getenv
   x))

;;; These variables are shared with XCVB itself.
(defvar *lisp-implementation-type*
  ;; TODO: test on all OS and implementation platform combinations!
  ;; MAYBE: #+gcl :gcl
  ;; PROBABLY NOT: cormancl mcl genera
  #+abcl :abcl
  #+allegro :allegro
  #+clisp :clisp
  #+clozure :ccl
  #+cmu :cmucl
  #+ecl :ecl
  #+lispworks :lispworks ;; run-program currently only available on UNIX
  #+sbcl :sbcl
  #+scl :scl
  #+xcl :xcl
  #-(or abcl allegro clisp clozure cmu ecl lispworks sbcl scl xcl)
  (error "Your Lisp implementation is not supported by XCVB master (yet).")
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
  (or #+sbcl (namestring (sb-int:sbcl-homedir-pathname))
      #+ccl (namestring (ccl::ccl-directory)))
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
(defgeneric call-with-output (x thunk)
  (:documentation ;; from fare-utils
   "Calls FUN with an actual stream argument, behaving like FORMAT with respect to stream'ing:
If OBJ is a stream, use it as the stream.
If OBJ is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OBJ is T, use *STANDARD-OUTPUT* as the stream.
If OBJ is a string with a fill-pointer, use it as a string-output-stream.
Otherwise, signal an error.")
  (:method ((x null) thunk)
    (with-output-to-string (s) (funcall thunk s)))
  (:method ((x (eql t)) thunk)
    (funcall thunk *standard-output*) nil)
  #-genera
  (:method ((x stream) thunk)
    (funcall thunk x) nil)
  (:method ((x string) thunk)
    (assert (fill-pointer x))
    (with-output-to-string (s x) (funcall thunk s)))
  (:method (x thunk)
    (declare (ignorable thunk))
    (cond
      #+genera
      ((typep x 'stream) (funcall thunk x) nil)
      (t (error "not a valid stream designator ~S" x)))))

(defmacro with-output ((x &optional (value x)) &body body)
  `(call-with-output ,value #'(lambda (,x) ,@body)))

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

(defun escape-token (token &key stream quote good-chars bad-chars escaper)
  (cond
    ((some
      (cond
        ((functionp good-chars) (complement good-chars))
        ((typep good-chars 'sequence) (lambda (c) (not (find c good-chars))))
        ((functionp bad-chars) bad-chars)
        ((typep bad-chars 'sequence) (lambda (c) (find c bad-chars)))
        (t (error "maybe-escape-token: no good-char criterion")))
      token)
     (with-output (stream)
       (apply escaper token stream (when quote `(:quote ,quote)))))
     ((null stream)
      token)
     (t
      (with-output (stream)
        (princ token stream)))))

(defun escape-command (command s &optional
                       (escaper #+os-unix 'escape-sh-token
                                #+os-windows 'escape-windows-token))
  (with-output (s)
    (loop :for first = t :then nil :for token :in command :do
      (unless first (princ #\space s))
      (funcall escaper token s))))

(defun escape-windows-token-within-double-quotes (x &optional s)
  (labels ((issue (c) (princ c s))
           (issue-backslash (n) (loop :repeat n :do (issue #\\))))
    (loop
      :initially (issue #\") :finally (issue #\")
      :with l = (length x) :with i = 0
      :for i+1 = (1+ i) :while (< i l) :do
      (case (char x i)
        ((#\") (issue-backslash 1) (issue #\") (incf i))
        ((#\\)
         (let* ((j (and (< i+1 l) (position-if-not (lambda (c) (eql c #\\)) x :start i+1)))
                (n (- (or j l) i)))
           (cond
             ((null j)
              (issue-backslash (* 2 n)) (setf i l))
             ((and (< j l) (eql (char x j) #\"))
              (issue-backslash (1+ (* 2 n))) (issue #\") (setf i (1+ j)))
             (t
              (issue-backslash n) (setf i j)))))
        (otherwise
         (issue (char x i)) (incf i))))))

(defun escape-windows-token (token &optional s)
  (escape-token token :stream s :bad-chars #(#\space #\tab #\") :quote #\"
                :escaper 'escape-windows-token-within-double-quotes))

(defun escape-windows-command (command &optional s)
    ;; encode a list of arguments into a string suitable for parsing by CommandLineToArgv
    ;; http://msdn.microsoft.com/en-us/library/bb776391(v=vs.85).aspx
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft(v=vs.85).aspx
  (etypecase command
    (string command)
    (list (escape-command command s 'escape-windows-token))))

(defun escape-sh-token-within-double-quotes (x s &key (quote t))
  (when quote (princ #\" s))
  (loop :for c :across x :do
    (when (find c "$`\\\"") (princ #\\ s))
    (princ c s))
  (when quote (princ #\" s)))

(defun escape-sh-token (token &optional s)
  (escape-token token :stream s :quote #\" :good-chars
                #'(lambda (x) (or (alphanumericp x) (find x "+-_.,%@:/")))
                :escaper 'escape-sh-token-within-double-quotes))

(defun escape-sh-command (command &optional s)
  (escape-command command s 'escape-sh-token))

(defun call-with-temporary-file (thunk &key
                                 prefix
                                 keep
                                 (direction :io)
                                 (element-type :default)
                                 (external-format :default))
  (check-type direction (member :output :io))
  (loop
    :with prefix = (or prefix
		       #+os-unix (format nil "~A/xm" (or (getenv "TMP") "/tmp"))
                       #+os-windows (format nil "~A\\xm" (getenv "TEMP")))
    :for counter :from (random (ash 1 32))
    :for pathname = (pathname (format nil "~A~36R" prefix counter)) :do
     ;; TODO: on Unix, do something about umask
     ;; TODO: on Unix, audit the code so we make sure it uses O_CREAT|O_EXCL
     ;; TODO: on Unix, use CFFI and mkstemp -- but the master is precisely meant to not depend on CFFI or on anything! Grrrr.
    (with-open-file (stream pathname
                            :direction direction
                            :element-type element-type :external-format external-format
                            :if-exists nil :if-does-not-exist :create)
      (when stream
        (return
          (if keep
              (funcall thunk stream pathname)
              (unwind-protect
                   (funcall thunk stream pathname)
                (ignore-errors (delete-file pathname)))))))))

(defmacro with-temporary-file ((&key (stream (gensym "STREAM") streamp)
                                (pathname (gensym "PATHNAME") pathnamep)
                                prefix keep element-type external-format)
                               &body body)
  `(flet ((think (,stream ,pathname)
            ,@(unless pathnamep `((declare (ignore ,pathname))))
            ,@(unless streamp `((close ,stream)))
            ,@body))
     (declare (dynamic-extent #'think))
     (call-with-temporary-file
      #'think
      ,@(when prefix `(:prefix ,prefix))
      ,@(when keep `(:keep ,keep))
      ,@(when element-type `(:element-type ,element-type))
      ,@(when external-format `(:external-format external-format)))))

(defun escape-shell-command (command &optional stream)
  (#+os-unix escape-sh-command
   #+os-windows escape-windows-command
   command stream))

;;; Simple variant of run-program with no input, and capturing output
;;; On some implementations, may output to a temporary file...
(defun run-program/process-output-stream (command output-processor
                                          &key ignore-error-status force-shell
                                          (element-type :default)
                                          (external-format :default))
  (declare (ignorable ignore-error-status element-type external-format))
  (let* ((p (find-package :quux-iolib))
         (r 'run-program/process-output-stream)
         (s (and p (find-symbol (string r) p))))
    (when s (return-from run-program/process-output-stream
              (funcall s command output-processor :ignore-error-status ignore-error-status))))
  #-(or abcl allegro clisp clozure cmu ecl lispworks sbcl scl xcl)
  (error "RUN-PROGRAM/PROCESS-OUTPUT-STREAM not implemented for this Lisp")
  (labels (#+(or allegro clisp clozure cmu ecl
                 (and lispworks os-unix) sbcl scl)
           (run-program (command &key pipe)
             "runs the specified command (a list of program and arguments).
              If using a pipe, returns two values: process and stream
              If not using a pipe, returns one values: the process result"
             (let* ((command (etypecase command
                              (string
                               #+os-unix `("/bin/sh" "-c" ,command)
                               #+os-windows `("cmd" "/c" ,command))
                              (list
                               command)))
                    (process*
                     #+(or allegro lispworks)
                     (multiple-value-list
                      #+allegro
                      (excl:run-shell-command
                       #+os-unix (coerce (cons (first command) command) 'vector)
                       #+os-windows (escape-windows-command command)
                       :input nil :output (and pipe :stream) :wait (not pipe))
                      #+lispworks
                      (system:run-shell-command
                       (cons "/usr/bin/env" command) ; lispworks wants a full path.
                       :input nil :output (and pipe :stream)
                       :wait (not pipe) :save-exit-status (and pipe t)))
                     #-(or allegro lispworks)
                     (#+(or clisp cmu ecl scl) ext:run-program
                      #+clozure ccl:run-program
                      #+sbcl sb-ext:run-program
                      (car command) #+clisp :arguments (cdr command)
                      :input nil :output (and pipe :stream) :wait (not pipe)
                      . #.(append
                           #+(or clozure cmu ecl sbcl scl) '(:error t)
                           #+sbcl '(:search t
                                    :external-format external-format))))
                    (process
                     #+(or allegro lispworks) (third process*)
                     #-(or allegro lispworks) process*)
                    (stream
                     #+(or allegro lispworks) (first process*)
                     #+(or clisp ecl) process
                     #+clozure (ccl::external-process-output process)
                     #+(or cmu scl) (ext:process-output process)
                     #+sbcl (sb-ext:process-output process)))
               (if pipe
                   (values process stream)
                   (process-result process))))
           (process-result (process)
             #+(or clisp ecl (and lispworks os-windows)) nil
             ;; 1- wait
             #+clozure (ccl::external-process-wait process)
             #+(or cmu scl) (ext:process-wait process)
             #+sbcl (sb-ext:process-wait process)
             ;; 2- extract result
             #+allegro (sys:reap-os-subprocess :pid process :wait t)
             #+clozure (nth-value 1 (ccl:external-process-status process))
             #+(or cmu scl) (ext:process-exit process)
             #+lispworks (system:pid-exit-status process :wait t)
             #+sbcl (sb-ext:process-exit-code process))
           (check-result (return-code)
             (unless (or ignore-error-status
                         #+(or ecl clisp) t
                         (zerop return-code))
               (cerror "ignore error code~*~*"
                       "Process ~S exited with error code ~D"
                       command return-code)))
           (system (command)
             #+(or abcl xcl)
             (ext:run-shell-command command)
             #+(or allegro clisp clozure cmu ecl
                   (and lispworks os-unix) sbcl scl)
             (run-program command :pipe nil)
             #+(and lispworks os-windows)
             (system:call-system-showing-output
              command :show-cmd nil :prefix "" :output-stream nil)))
    (if (and (not force-shell)
             #+(or abcl (and lispworks os-windows) xcl) nil)
        ;; run-program
        (multiple-value-bind (process stream)
            (run-program command :pipe t)
          (unwind-protect
               (funcall output-processor stream)
            (close stream)
            (check-result (process-result process))))
        ;; system
        (with-temporary-file (:pathname tmp)
          (let* ((command-string
                  (format nil "~A > ~A"
                          (etypecase command
                            (string command)
                            (list (escape-shell-command command)))
                          tmp)))
            (check-result (system command-string))
            (with-open-file (stream tmp
                             :direction :input :if-does-not-exist :error
                             :element-type element-type
                             :external-format external-format)
              (funcall output-processor stream)))))))

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

(defun run-program/echo-output (command &key prefix (stream t) ignore-error-status)
  (run-program/process-output-stream
   command #'(lambda (s) (loop :for line = (read-line s nil nil) :while line
                           :do (format stream "~@[~A~]~A~&" prefix line)))
   :ignore-error-status ignore-error-status))

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
