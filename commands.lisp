;;; Shell command-line interface for XCVB

#+xcvb
(module (:depends-on ("specials" "macros")))

(in-package :xcvb)

(declaim (optimize (speed 2) (safety 3) (compilation-speed 0) (debug 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Command Spec ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-option-spec (name options)
  `(eval-now
     (defparameter ,name ,options)))

(defmacro define-command (name-keys (names args options
                                           short-description description
                                           &optional ignorable)
                          &body body)
  (let* ((name-keys (ensure-list name-keys))
         (name (first name-keys))
         (keys (rest name-keys))
         (option-spec-var (conc-symbol '+ name '-option-spec+))
         (option-spec-val (eval options))
         (option-args (mapcar (lambda (option) (intern (string-upcase (caar option))))
                              option-spec-val))
         (ignore (case ignorable ((ignore ignorable) ignorable) (otherwise 'ignore)))
         (ignored (if (consp ignorable) (set-difference option-args ignorable) option-args)))
    `(progn
       ,@(when options ;; make it available immediately as well as in further commands
           (setf (symbol-value option-spec-var) option-spec-val)
           `((define-option-spec ,option-spec-var ,options)))
       (register-command-properties
        ',name :names ',names :option-spec ,(when options option-spec-var)
        :short-description ,short-description :description ,description
        ,@(when keys `(:handle-command-line-keys ',keys)))
       (register-command ',name)
       (defun ,name
           ,(append (or args '(&key)) option-args)
         ,@(when ignorable `((declare (,ignore ,@ignored))))
         ,@body))))

(defun register-command-properties
    (name &rest keys &key
     names option-spec short-description description handle-command-line-keys)
  "Register the properties associated to a function as a command"
  (declare (ignore names option-spec short-description description handle-command-line-keys))
  (setf (get name 'command) keys))

(defun registered-command-properties (name)
  "Registered properties associated to a function as a command"
  (get name 'command))

(defvar *commands* (cons () (make-hash-table :test 'equal))
  "A registry of command-line accessible commands, as a cons cell
whose car is a list of command function symbols in reverse,
and whose cdr is a hash-table mapping command-line names to command symbols.
Each symbol must have registered 'command properties.")

(defun commands () ;; associated with current package. Meh.
  (symbol-value (conc-symbol '*commands*)))

(defun register-command (command)
  (let ((properties (registered-command-properties command))
        (commands (commands)))
    (pushnew command (car commands))
    (dolist (name (getf properties :names))
      (setf (gethash name (cdr commands)) command))))

(defun lookup-command (command-name)
  "Lookup the command spec for the given command name, or return nil if the
given command name is invalid."
  (let ((command (gethash command-name (cdr (commands)))))
    (cons command (registered-command-properties command))))

(defun interpret-command (arguments)
  (let* ((command (first arguments))
         (args (rest arguments))
         (registered (lookup-command command))
         (fun (first registered))
         (properties (rest registered))
         (option-spec (getf properties :option-spec))
         (keys (getf properties :handle-command-line-keys)))
    (cond
      (option-spec
       (apply 'handle-command-line
              option-spec fun
              :command-line args :name command
              keys))
      (fun
       (funcall fun args))
      ((not command)
       (errexit 2 "~:@(~A~) requires a command -- try '~:*~A help'." *program*))
      (t
       (errexit 2 "Invalid ~:@(~A~) command ~S -- try '~2:*~A help'." *program* command)))))

;;;; Common Option Specs

(define-option-spec +xcvb-program-option-spec+
  '((("xcvb-program" #\X) :type string :optional t
     :documentation "specify where to find the xcvb binary")
    (("required-xcvb-version" #\V) :type string :optional t
     :documentation "specify a minimum xcvb version")))

(define-option-spec +source-registry-option-spec+
  '((("source-registry" #\S) :type string :optional t
     :documentation "override your source-registry")))

(define-option-spec +lisp-implementation-option-spec+
  '((("lisp-implementation" #\l) :type string :initial-value "sbcl" :documentation "specify type of Lisp implementation")
    (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
    (("define-feature" #\D) :type string :list t :optional t :documentation "define a CL into the target")
    (("undefine-feature" #\U) :type string :list t :optional t :documentation "undefine a CL from the target")))

(define-option-spec +cfasl-option-spec+
  '((("disable-cfasl" #\d) :type boolean :optional t :documentation "disable the CFASL feature")))

(define-option-spec +verbosity-option-spec+
  '((("verbosity" #\v) :type integer :initial-value 5 :documentation "set verbosity")
    (("debugging" #\Z) :type boolean :optional t :initial-value nil :documentation "debug")))

(define-option-spec +setup-option-spec+
  '((("setup" #\s) :type string :optional t :documentation "specify a Lisp setup file")))

(define-option-spec +base-image-option-spec+
  '((("use-base-image" #\B) :type boolean :optional t :initial-value t :documentation "use a base image")))

(define-option-spec +profiling-option-spec+
  '((("profiling" #\P) :type boolean :optional t :documentation "profiling")))

(define-option-spec +workspace-option-spec+
  '((("workspace" #\W) :type string :optional t :documentation "specify workspace directory")
    (("cache" #\C) :type string :optional t :documentation "specify cache directory")
    (("object-cache" #\O) :type string :optional t :documentation "specify object-cache directory")))

(define-option-spec +install-option-spec+
  '((("install-prefix") :type string :optional t :documentation "specify install directory prefix")
    (("install-program") :type string :optional t :documentation "specify program install directory")
    (("install-configuration") :type string :optional t :documentation "specify configuration install directory")
    (("install-data") :type string :optional t :documentation "specify data install directory")
    (("install-library") :type string :optional t :documentation "specify library install directory")
    (("install-image") :type string :optional t :documentation "specify image install directory")
    (("install-lisp") :type string :optional t :documentation "specify lisp code install directory")))

(define-option-spec +build-option-spec+
  '((("build" #\b) :type string :optional nil :documentation "specify what build to process")))

(define-option-spec +multi-build-option-spec+
  '((("build" #\b) :type string :list t :optional nil :documentation "specify what builds to process")))
