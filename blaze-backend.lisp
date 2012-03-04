#+xcvb
(module
  (:author ("Francois-Rene Rideau")
   :maintainer "Francois-Rene Rideau"
   ;; :run-depends-on ("string-escape")
   :depends-on ("profiling" "specials" "virtual-pathnames"
                "static-traversal" "computations"
                "external-commands" "target-lisp-commands" "commands")))

(in-package :xcvb)

;; Backend for the Google BUILD.

(defclass blaze-traversal (static-traversal)
  ())

(defvar *BUILD-root* nil
  "Path of the root to (writeable) BUILD packages.")
(defvar *READONLY-BUILD-root* nil
  "Path of the root to read-only BUILD packages.")
(defvar *BUILD-package* nil
  "current BUILD package")
(defvar *BUILD-package-directory* nil
  "current BUILD package directory")

(define-option-spec +blaze-option-spec+
  '((("BUILD-root" #\r) :type string :optional t :documentation "root of BUILD packges") ;; default: google3 ancestor of current directory
    (("READONLY-root" #\R) :type string :optional t :documentation "google3 root (default: from current dir)") ;; default: ${BUILD-root}/../READONLY/google3/
    (("package" #\P) :type string :optional t :documentation "BUILD package"))) ;; default: package for current dir under BUILD-root

(defun resolve-blaze-source-registry (source-registry &key root readonly-root)
  (cond
    ((emptyp source-registry)
     (let ((bsr (getenv "BLAZE_SOURCE_REGISTRY")))
       (if (emptyp bsr)
           (error "No --source-registry or BLAZE_SOURCE_REGISTRY specified")
           (resolve-blaze-source-registry
            bsr :root root :readonly-root readonly-root))))
    ((find (char source-registry 0) "\"(")
     ;; (warn "Using a Lisp source-registry at your own risk")
     source-registry)
    (t
     (with-output-to-string (s)
       (loop :for entry :in (split-string source-registry :separator ";")
         :for previousp = nil
         :for recursep = (string-suffix-p entry "//") :do
         (labels ((x (p)
                    (when p
                      (format s "~@[:~]~A~@[/~]"
                              previousp (ensure-directory-pathname p)
                              recursep))))
           (if (find #\: entry)
               (x (label->pathname
                   entry :root root :readonly-root readonly-root))
               (x (BUILD-package->pathname
                   entry :root root :readonly-root readonly-root)))))))))

(defun handle-blaze-options (&rest keys
                             &key build-root readonly-root package
                             source-registry lisp-binary-path
                             &allow-other-keys)
  (let* ((current-dir
          (unless (and build-root package)
            (truename *default-pathname-defaults*)))
         (build-dir
          (unless package (find-BUILD-directory current-dir)))
         (build-root
          (or build-root (find-BUILD-root (or build-dir current-dir))))
         (package-name
          (or package (enough-namestring build-dir build-root)))
         (package-dir
          (if package
              (subpathname build-root package)
              current-dir))
         (readonly-root
          (if readonly-root
              (truename readonly-root)
              (probe-file* (subpathname build-root "../READONLY/google3/"))))
         (source-registry (resolve-blaze-source-registry
                           source-registry
                           :root build-root :readonly-root readonly-root))
         (lisp-binary-path (label->pathname lisp-binary-path)))
    (setf *BUILD-root* build-root
          *BUILD-package* package-name
          *BUILD-package-directory* package-dir
          *READONLY-BUILD-root* readonly-root)
    (apply 'handle-global-options
           :source-registry source-registry
           :lisp-binary-path lisp-binary-path
           keys)))

(defparameter *BUILD-root-p* (make-hash-table :test 'equal))

(define-memo-function (BUILD-root-p :table *BUILD-root-p*) (dir)
  ;; This is way too specific to Google
  (equal (with-open-file (s (subpathname dir "__init__.py")
                            :direction :input :if-does-not-exist nil)
           (and s (read-line s nil nil)))
         "# DO NOT EDIT THIS FILE EXCEPT AS //depot/google3/tools/google3__init__.py"))

(defun find-BUILD-root (&optional (dir *default-pathname-defaults*))
  (find-proper-ancestor dir 'BUILD-root-p))

(defparameter *BUILD-directory-p* (make-hash-table :test 'equal))

(define-memo-function (BUILD-directory-p :table *BUILD-directory-p*) (dir)
  (and (probe-file (subpathname dir "BUILD")) t))

(defun find-BUILD-directory (&optional (dir *default-pathname-defaults*))
  (find-proper-ancestor dir 'BUILD-directory-p))

(defun pathname->label (pathname &optional
                        (package-name *BUILD-package*))
  (let* ((truename (truename pathname))
         (build-dir (find-BUILD-directory truename))
         (build-root (and build-dir (find-BUILD-root build-dir))))
    (when build-root
      (let ((filename (enough-namestring truename build-dir))
            (package (enough-namestring build-dir build-root)))
        (if (equal package package-name)
            filename
            (strcat package ":" filename))))))

(defun split-label (label &optional (package-name *BUILD-package*))
  (destructuring-bind (&optional x y)
      (split-string label :max 2 :separator ":")
    (cond
      (y (values x y))
      ((and x package-name) (values package-name x))
      (t nil))))

(defun BUILD-package->pathname
    (package-name
     &key (root *BUILD-root*) (readonly-root *READONLY-BUILD-root*))
  (flet ((sub (x) (probe-file* (subpathname x (strcat package-name "/")))))
    (or (sub root) (sub readonly-root))))

(defun label->pathname (label &key
                        (package-name *BUILD-package*)
                        (package-directory *BUILD-package-directory*)
                        (root *BUILD-root*)
                        (readonly-root *READONLY-BUILD-root*))
  (multiple-value-bind (label-package-name label-filename)
      (split-label label package-name)
    (let ((package-dir
           (if (and package-directory
                    (equal package-name label-package-name))
               package-directory
               (BUILD-package->pathname
                label-package-name
                :root root :readonly-root readonly-root))))
      (subpathname package-dir label-filename))))

(defun computations-to-blaze-BUILD (env)
  (with-output-to-string (s)
    (dolist (computation *computations*)
      (write-computation-to-blaze-BUILD env s computation))))

(defun write-blaze-BUILD (fullname &key output-path)
  "Write a blaze BUILD file to output-path"
  (multiple-value-bind (target-dependency build directory) (handle-target fullname)
    (declare (ignore build directory))
    (let* ((env (make-instance 'blaze-traversal))
           (default-output-path (subpathname *default-pathname-defaults* "BUILD"))
           (actual-output-path
            (if output-path
                (merge-pathnames* output-path default-output-path)
                default-output-path))
           (blaze-BUILD-path (ensure-absolute-pathname actual-output-path))
           (blaze-BUILD-dir (pathname-directory-pathname blaze-BUILD-path))
           (*default-pathname-defaults* blaze-BUILD-dir)
           (*print-pretty* nil); otherwise SBCL will slow us down a lot.
           (lisp-env-var (lisp-environment-variable-name :prefix nil))
           (*lisp-executable-pathname* ;; magic escape!
            ;; NIY, blaze escape rather than Makefile escape
            (list :blaze-BUILD "${" lisp-env-var "}")))
      (log-format 9 "output-path: ~S" output-path)
      (log-format 9 "default-output-path: ~S" default-output-path)
      (log-format 9 "actual-output-path: ~S" actual-output-path)
      (log-format 6 "blaze-BUILD-path: ~S" blaze-BUILD-path)
      (log-format 9 "*default-pathname-defaults*: ~S" *default-pathname-defaults*)
      (log-format 7 "workspace: ~S" *workspace*)
      (log-format 7 "cache: ~S" *cache*)
      (log-format 7 "object-cache: ~S" *object-cache*)
      ;; Pass 1: Traverse the graph of dependencies
      (log-format 8 "T=~A building dependency graph" (get-universal-time))
      (graph-for env target-dependency)
      ;; Pass 2: Build a Blaze-BUILD out of the *computations*
      (log-format 8 "T=~A computing blaze-BUILD body" (get-universal-time))
      (log-format 8 "All *computations*=~%~S" (reverse *computations*))
      (let ((body (computations-to-blaze-BUILD env)))
        (log-format 8 "T=~A creating blaze-BUILD" (get-universal-time))
        (ensure-directories-exist blaze-BUILD-path)
        (with-open-file (out blaze-BUILD-path
                             :direction :output
                             :if-exists :supersede)
          (log-format 8 "T=~A printing blaze-BUILD" (get-universal-time))
          (write-blaze-BUILD-prelude
           :stream out :lisp-env-var lisp-env-var)
          (princ body out)
          (write-blaze-BUILD-conclusion out)))
      (log-format 8 "T=~A done" (get-universal-time))
      ;; Return data for use by the non-enforcing Blaze-BUILD backend.
      (values blaze-BUILD-path blaze-BUILD-dir))))

(defun write-blaze-BUILD-prelude (&key stream lisp-env-var)
  (let ((vars (list lisp-env-var))
        (implementation-pathname
         (or *target-lisp-executable-pathname*
             (lisp-implementation-name (get-lisp-implementation)))))
    (write-generated-file-warning stream implementation-pathname)
    vars))

;; TODO: clean
;; * a clean-xcvb target that removes the object directory
(defun write-blaze-BUILD-conclusion (&optional stream)
  (format stream "~%"))

(defmethod vp-namestring :around ((env blaze-traversal) vp)
  (let ((namestring (call-next-method)))
    namestring))

#|
(defmethod grain-pathname-text ((env blaze-traversal) (grain file-grain))
  (let ((pathname (call-next-method)))
    (values (escape-sh-token-for-blaze-BUILD (enough-namestring pathname)) pathname)))
|#

(defmethod grain-pathname-text :around ((env blaze-traversal) grain)
  (declare (ignorable env grain))
  (or (call-next-method) ""))

(defun blaze-BUILD-commands-for-computation (env computation-command)
  (mapcar 'shell-tokens-to-Makefile ;; BOGUS
          (external-commands-for-computation env computation-command)))

#|
;;; TODO: find labels!
(defmethod effective-namestring ((env blaze-traversal) fullname)
  (fullname-enough-namestring env fullname))
(defmethod pseudo-effective-namestring ((env blaze-traversal) fullname)
  (pseudo-fullname-enough-namestring env fullname))
|#

(defun write-computation-to-blaze-BUILD (env stream computation)
  (with-accessors ((command computation-command)
                   (inputs computation-inputs)
                   (outputs computation-outputs)) computation
    (let* ((target (grain-pathname-text env (first outputs)))
           (tools ())) ;;TODO include the compiler and its support files!!!
      ;; TODO: C compiling extensions require the C compiler, too,
      ;; and a proper setup(!)
      (format stream "~&genrule(~%    ~
         name = ~S,~%    ~
         cmd = '~{~A~^ ; ~}'~%    ~
         srcs = [ ~{~S~^, ~} ],~%    ~
         outs = [ ~{~S~^, ~} ],~%    ~
         tools = [ ~{~S~^, ~} ],~%)~%~%"
              target
              (mapcar/ #'blaze-BUILD-commands-for-computation env command)
              (mapcar/ #'grain-pathname-text env inputs)
              (mapcar/ #'grain-pathname-text env outputs)
              (mapcar/ #'grain-pathname-text env tools)))))

(defmethod grain-pathname-text ((env blaze-traversal) (grain phony-grain))
  (declare (ignore env))
  (let ((n #|(normalize-name-for-blaze-BUILD|# (princ-to-string (fullname grain))));)
    n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make-Blaze-BUILD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command make-blaze-BUILD
    (("make-blaze-BUILD" "mbb")
     (&rest keys &key)
     `(,@+build-option-spec+
       ,@+setup-option-spec+
       ,@+base-image-option-spec+
       ,@+source-registry-option-spec+
       ,@+xcvb-program-option-spec+
       ,@+lisp-implementation-option-spec+
       ,@+blaze-option-spec+
       ,@+workspace-option-spec+
       ,@+cfasl-option-spec+
       ,@+verbosity-option-spec+
       ,@+profiling-option-spec+)
     "Create some blaze BUILD"
     "Create blaze BUILD rules to build a project." ignore)
  (apply 'blaze-build :blaze-BUILD-only t keys))

(defun invoke-blaze (&key target directory makefile ignore-error-status env)
  (let* ((make (or (getenv "MAKE") "make"))
         (make-command
          `(,@(when env `("env" ,@env))
            ,make
            ,@(when directory `("-C" ,(namestring directory)))
            ,@(when makefile `("-f" ,(namestring makefile)))
            ,@(when target (ensure-list target)))))
      (log-format 6 "Building with ~S" make-command)
      (run-program/
       make-command ; (strcat (escape-shell-command make-command) " >&2")
       :output nil ;; for side-effects only
       :ignore-error-status ignore-error-status)))

(define-command blaze-build
    (("blaze-build" "blaze" "bb")
     (&rest keys &key blaze-BUILD-only (exit t))
     `(,@+make-blaze-BUILD-option-spec+)
     "Use blaze to build your project"
     "Create blaze BUILD rules to build a package, use them."
     (build))
  (apply 'handle-blaze-options keys)
  (with-maybe-profiling ()
    (multiple-value-bind (blaze-BUILD-path blaze-BUILD-dir)
        (write-blaze-BUILD build)
      (if blaze-BUILD-only
          (values blaze-BUILD-path blaze-BUILD-dir)
          (let ((code (invoke-blaze
                       :directory blaze-BUILD-dir
                       :ignore-error-status t)))
            (if exit
                (exit code)
                (values code blaze-BUILD-dir)))))))
