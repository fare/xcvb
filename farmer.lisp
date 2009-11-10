#+xcvb
(module
 (:depends-on
  ("macros" "specials" "static-backends"
   (:when (:featurep :sbcl)
     (:require :sb-grovel)
     (:require :sb-posix)))))

;;TODO: split and rename into active-traversal and standalone-backend

(in-package :xcvb)

#|
0- don't use hash consing
1- instead keep a TTH(?) hash for each "state of the world" -
 hashname and fullhashnames
2- equal-hash that!
|#

(defun mkfifo (pathname mode)
  #+sbcl (sb-posix:mkfifo pathname mode)
  #+clozure (ccl::with-filename-cstrs ((p pathname))(#.(read-from-string "#_mkfifo") p mode))
  #+clisp (LINUX:mkfifo pathname mode)
  #-(or sbcl clozure clisp) (error "mkfifo not implemented for your Lisp"))

(defvar *workers* (make-hash-table :test 'equal)
  "maps intentional state of the world identifiers to descriptors of worker processes
waiting at this state of the world.")

(defclass worker ()
  ())
(defgeneric worker-send (worker form)
  (:documentation "send a form to be executed on the worker"))
(defmethod worker-send (worker (x cons))
  (worker-send worker (readable-string x)))

(defclass active-world (world-grain)
  ((futures
    :initform nil
    :accessor world-futures
    :documentation "a list of (action . grain) in the future of this world")
   (handler
    :initform nil
    :accessor world-handler
    :documentation "a handler that will accept commands to run actions on this world")))

(defvar *root-world* nil
  "root of active worlds")
(defvar *worlds* (make-hash-table :test 'equal))

(defun make-world-summary (setup commands-r)
  (cons setup commands-r))
(defun world-summary (world)
  (make-world-summary (image-setup world) (build-commands-r world)))
(defun world-summary-hash (world-summary)
  (sxhash world-summary)) ; use tthsum?
(defun compute-world-hash (world)
  (world-summary-hash (world-summary world))) ; use tthsum?
(defun world-equal (w1 w2)
  (equal (world-summary w1) (world-summary w2)))
(defun intern-world-summary (setup commands-r thunk)
  (loop
    :with summary = (make-world-summary setup commands-r)
    :with hash = (world-summary-hash summary)
    :for w :in (gethash hash *worlds*)
    :do (when (equal summary (world-summary w)) (return w))
    :finally (let ((world (apply #'make-instance 'active-world
                                 :hash hash
                                 :setup setup
                                 :commands-r commands-r
                                 (funcall thunk))))
               (push world (gethash hash *worlds* '()))
               (return world))))
(defun make-initial-world ()
  (intern-world-summary
   () ()
   (lambda ()
     (list
      :setup () :commands-r ()
      :included-dependencies (make-hash-table :test 'equal)
      :issued-build-commands (make-hash-table :test 'equal)))))
(defun setup-world (world fullname)
  (let ((new-setup (append1 (image-setup world) fullname)))
    (intern-world-summary
     new-setup
     (build-commands-r world)
     (lambda ()
       (list
        :included-dependencies (make-hash-table :test 'equal)
        :issued-build-commands (make-hash-table :test 'equal))))))

(defclass farmer-traversal (static-traversal)
  ())

(defun simplified-xcvb-driver-command (computation-command)
  (cond
    ((and (list-of-length-p 2 computation-command)
          (eq :progn (first computation-command)))
     (simplified-xcvb-driver-command (second computation-command)))
    ((and (consp computation-command)
          (eq :xcvb-driver-command (first computation-command)))
     (values (second computation-command)
             (simplify-xcvb-driver-commands (cddr computation-command))))
    (t (error "Unrecognized computation command ~S" computation-command))))

(defun simplify-xcvb-driver-commands (commands)
  (while-collecting (c) (emit-simplified-commands #'c commands)))

(defvar *simple-xcvb-driver-commands*
  '(:load-file :require :load-asdf :register-asdf-directory :debugging))

(defun emit-simplified-commands (collector commands)
  (flet ((collect (x) (funcall collector x)))
    (dolist (c commands)
      (let ((l (length c))
            (h (first c)))
        (cond
          ((and (= 2 l) (member h *simple-xcvb-driver-commands*))
           (collect c))
          ((and (<= 2 l) (eq h :compile-lisp))
           (emit-simplified-commands collector (cddr c))
           (collect `(:compile-lisp ,(second c))))
          ((and (<= 2 l) (eq h :create-image))
           ;; TODO: distinguish the case when the target lisp is linking rather than dumping,
           ;; e.g. ECL. -- or in the future, any Lisp when linking C code.
           (emit-simplified-commands collector (cddr c))
           (collect `(:create-image ,(second c))))
          (t
           (error "Unrecognized xcvb driver command ~S" c)))))))

(defun setup-dependencies (env setup)
  (destructuring-bind (&key image load) setup
    (mapcar/
     env #'graph-for
     (append
      ;; TODO: include the lisp implementation itself, binary and image, when image is the default.
      (when image (list image))
      (when load load)))))

(defmethod make-computation ((env farmer-traversal)
                             &key inputs outputs command &allow-other-keys)
  (declare (ignore inputs))
  (multiple-value-bind (setup commands)
      (simplified-xcvb-driver-command command)
    (loop
      :for command = nil :then (if commands (pop commands) (return computation))
      :for commands-r = nil :then (cons commands-r command)
      :for grain-name = (unwrap-load-file-command command)
      :for grain = (when grain-name (registered-grain grain-name))
      :for world-name = `(:world :setup ,setup :commands-r ,commands-r)
      :for previous = *root-world* :then world
      :for world = (intern-world-summary
                    setup commands-r
                    (lambda ()
                      (list)))
      :for computation = (make-computation
                          ()
                          :inputs (append (when previous (list previous))
                                          (setup-dependencies env (image-setup world))
                                          (when grain (list grain)))
                          :outputs (if commands (list world) outputs)
                          :command `(:active-command ,(fullname previous) ,command))
      :do (pushnew world (world-futures previous)))))

#|
((world
    :accessor current-world
    :documentation "world object representing the current state of the computation")))
(defmethod included-dependencies ((traversal farmer-traversal))
  (included-dependencies (current-world traversal)))
(defmethod dependency-already-included-p ((env farmer-traversal) grain)
  (or (gethash grain (included-dependencies env))
      (call-next-method)))

|#

(defun standalone-build (fullname &key output-path)
  (declare (ignore output-path))
  (let* ((target (resolve-absolute-module-name fullname))
         (build (if target (build-grain-for target)
                    (errexit 3 "User requested build ~S but it can't be found.~%~
				You may check available builds with xcvb ssp.~%" fullname)))
         (*use-master* nil)
         (*root-world* (make-initial-world))
         (traversal (make-instance 'farmer-traversal)))
    (declare (ignore build))
    (graph-for traversal target)
    ;;; TODO: actually walk the world tree
    (break)
    (error "NIY")))

(defun standalone-build-command
    (arguments &key
     xcvb-path setup verbosity output-path
     build lisp-implementation lisp-binary-path
     disable-cfasl master object-directory base-image)
  ;;; TODO: parse arguments (see other backends)
  (when arguments
    (error "Invalid arguments to build"))
  (when xcvb-path
    (set-search-path! xcvb-path))
  (when verbosity
    (setf *xcvb-verbosity* verbosity))
  (when output-path
    (setf *default-pathname-defaults*
          (ensure-absolute-pathname (pathname-directory-pathname output-path))))
  (when object-directory
    (setf *object-directory* ;; strip last "/"
          (but-last-char (enough-namestring (ensure-pathname-is-directory object-directory)))))
  (when lisp-implementation
    (setf *lisp-implementation-type*
          (find-symbol (string-upcase lisp-implementation) (find-package :keyword))))
  (when lisp-binary-path
    (setf *lisp-executable-pathname* lisp-binary-path))
  (extract-target-properties)
  (read-target-properties)
  (when disable-cfasl
    (setf *use-cfasls* nil))
  (setf *use-base-image* base-image)
  (search-search-path)
  (setf *use-master* master)
  (when master
    (ensure-tthsum-present)
    (append1f *lisp-setup-dependencies* '(:fasl "/xcvb/master")))
  (when setup
    (let ((module (resolve-absolute-module-name setup)))
      (unless module
        (error "Cannot find setup module ~A" setup))
      (append1f *lisp-setup-dependencies* `(:lisp ,(fullname module)))))
  (standalone-build (canonicalize-fullname build) :output-path output-path))
