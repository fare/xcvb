#+xcvb
(module
 (:depends-on
  ("macros" "specials" "static-traversal" "profiling" "main" "makefile-backend")))

#|
* TODO: debug current scheduler by commenting out tthsum thing,
  having a trivial fake job worker that returns immediately,
  and computes the time. Make that a parameter.

* TODO: between the scheduler and the worker backend,
  there is a layer of state management (1-to-n relationship
  between states and workers) that might require its own layer,
  including management of keeping the successor world alive
  if and only if it itself has users.

* TODO: complete a simple local worker backend based on the forker,
  i.e. communicate with a descendant.

* TODO: complete a simple local worker backend based on forking
  new processes anew for every job? For testing purposes, do like make!

* TODO: split and rename into active-traversal and standalone-backend

* TODO: cache world hash, compute it incrementally in an O(1) way
  instead of O(n) by keeping a link to the previous world.

|#

(in-package :xcvb)

(defvar *workers* (make-hash-table :test 'equal)
  "maps intentional state of the world identifiers to descriptors of worker processes
waiting at this state of the world.")

(defvar *ready-computations* (make-hash-table)) ;; set of computations ready to be issued
(defvar *waiting-computations* (make-hash-table)) ;; set of computations waiting for inputs
(defvar *pending-processes* (make-hash-table :test 'equal)) ;; pid to pending computation
(defvar *expected-latencies* (make-hash-table)) ;; computation to latency evaluation
(defvar *poll-timeout* .2) ;; how often to re-poll in seconds

(defvar *pipes* (make-hash-table :test 'equal)) ; maps pipe streams to computations
(defvar *event-base* nil)
(defvar *sigchldfd* nil)

(defparameter +fifo-subpathname+ "_fifo/")
(defvar +working-directory-subpathname+ "")
(defparameter *fifo-name->computation* (make-hash-table :test 'equal))



(defclass worker ()
  ())
(defgeneric worker-send (worker form)
  (:documentation "send a form to be executed on the worker"))
(defmethod worker-send (worker (x cons))
  (worker-send worker (readable-string x)))

(defclass active-world (world-grain buildable-grain)
  ((futures
    :initform nil
    :accessor world-futures
    :documentation "a list of computations in the future of this world")
   (hash :accessor world-hash)
   (fifo-name :accessor world-fifo-name)
   (handler
    :initform nil
    :accessor world-handler
    :documentation "a handler that will accept commands to run actions on this world")))

(defvar *root-worlds* nil "root of active worlds")

(defun make-world-summary (setup commands-r)
  ;;(cons (canonicalize-image-setup setup) commands-r)
  (make-world-name setup commands-r))
(defun world-summary (world)
  (make-world-summary (image-setup world) (build-commands-r world)))
(defun world-summary-hash (world-summary)
  (sxhash world-summary)) ; use tthsum?
(defun compute-world-hash (world)
  (world-summary-hash (world-summary world))) ; use tthsum?
(defun world-equal (w1 w2)
  (equal (world-summary w1) (world-summary w2)))
(defun intern-world-summary (setup commands-r key-thunk fun)
  (let ((fullname (make-world-name setup commands-r)))
    (call-with-grain-registration
     fullname
     (lambda ()
       (let* ((summary (make-world-summary setup commands-r))
              (hash (world-summary-hash summary)))
         #|(loop
           :for w :in (gethash hash *worlds*)
           :when (equal summary (world-summary w))
           :do (error "new world already hashed??? ~S" w))|#
         (let ((world (apply #'make-instance 'active-world
                             :fullname fullname
                             :hash hash
                             (funcall key-thunk))))
           #|(push world (gethash hash *worlds* '()))|#
           (funcall fun world)
           world))))))

(defclass farmer-traversal (static-traversal)
  ())

(defun simplified-xcvb-driver-command (computation-command)
  (unless (consp computation-command)
    (error "Unrecognized computation command ~S" computation-command))
  (ecase (first computation-command)
    ((:progn)
     (assert (list-of-length-p 2 computation-command))
     (simplified-xcvb-driver-command (second computation-command)))
    ((:xcvb-driver-command)
     (destructuring-bind (setup &rest commands) (rest computation-command)
       (values setup
               (simplified-xcvb-driver-commands commands))))
    ((:compile-file-directly)
     (assert (<= 2 (length computation-command) 4))
     (values () (list computation-command))))) ;;; TODO: what need we do in this magic case?

(defun simplified-xcvb-driver-commands (commands)
  (while-collecting (c) (emit-simplified-commands #'c commands)))

(defvar *simple-xcvb-driver-commands*
  '(:load-file :require :load-asdf :register-asdf-directory :debugging))

(defun emit-simplified-commands (collector commands)
  (flet ((collect (x) (funcall collector `(:xcvb-driver-command ,x))))
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
     #'graph-for env
     (remove-if #'null
      (remove-duplicates
       (append
        (unless (lisp-implementation-image-executable-p (get-lisp-implementation))
          (list *lisp-executable-pathname*))
        (list
         (or image *lisp-image-pathname*))
        (when load load))
       :test 'equal)))))

(defmethod make-computation ((env farmer-traversal)
                             &key inputs outputs command &allow-other-keys)
  (declare (ignore inputs))
  (multiple-value-bind (setup commands)
      (simplified-xcvb-driver-command command)
    (loop
      :for command = nil :then (if commands (pop commands) (return (grain-computation world)))
      :for commands-r = nil :then (cons command commands-r)
      :for grain-name = (unwrap-load-file-command command)
      :for grain = (when grain-name (registered-grain grain-name))
      :for previous = nil :then world
      :for world = (intern-world-summary
                    setup commands-r
                    (lambda ()
                      (unless previous '(:computation nil)))
                    (lambda (world)
                      (DBG :w world previous grain)
                      (if previous
                        (let ((computation
                               (make-computation
                                ()
                                :inputs (append (list previous)
                                                (setup-dependencies env (image-setup world))
                                                (when grain (list grain)))
                                :outputs (cons world (unless commands outputs))
                                :command `(:active-command ,(fullname previous) ,command))))
                          (push computation (world-futures previous))
                          computation)
                        (push world *root-worlds*))))
      :do (DBG :mc command commands-r grain-name grain previous world))))

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

(defmethod object-namestring ((env farmer-traversal) name &optional merge)
  ;; TODO: replace that by something that will DTRT, whatever THAT is.
  ;; probably we need to refactor or gf away the parts that currently depend on it,
  ;; notably fasl-grains-for-name's :pathname thingie.
  (let* ((pathname (portable-pathname-from-string name))
         (merged (if merge (merge-pathnames merge pathname) pathname))
         (namestring (strcat *object-directory* (portable-namestring merged))))
    (ensure-makefile-will-make-pathname env namestring)
    namestring))

(defun compute-grain-hash (grain)
  (when (typep grain 'buildable-grain)
    (setf (grain-content-digest grain) (tthsum-for-file (grain-pathname grain)))))

(defclass latency-parameters ()
  ((total-lisp-compile-duration
    :initform 0)
   (total-lisp-compile-size
    :initform 0)
   (total-fasl-load-duration
    :initform 0)
   (total-fasl-load-size
    :initform 0)
   (total-lisp-load-duration
    :initform 0)
   (total-lisp-load-size
    :initform 0)
   (total-fork-size
    :initform 0)
   (total-fork-duration
    :initform 0)))

(defun direct-computation-latency
    (computation &key
     latencies
     parameters
     current-measurements
     previous-parameters
     previous-measurements)
  (declare (ignorable computation latencies parameters current-measurements
                      previous-parameters previous-measurements)) ;; TODO: do better than that!
  1)

(defun compute-latency-model (computations &key
                              (latencies (make-hash-table))
                              (parameters (make-instance 'latency-parameters))
                              (current-measurements (make-hash-table))
                              (previous-parameters (make-instance 'latency-parameters))
                              (previous-measurements (make-hash-table)))
  ;; assumes computations in reverse chronological order,
  ;; as is the case for *computations* after a traversal.
  (loop :for c :in computations
    :for latency = (+ (direct-computation-latency
                           c :latencies latencies
                           :parameters parameters
                           :current-measurements current-measurements
                           :previous-parameters previous-parameters
                           :previous-measurements previous-measurements)
                      (loop :for child :in (computation-children c)
                        :maximize (gethash child latencies)))
    :do (setf (gethash c latencies) latency)
    :maximize latency)
  latencies)

;; TODO: parameterize the farming, so that
;; 1- a first version computes the best possible latency assuming infinite cpu
;; 2- a second version computes latency assuming finite cpu (specified or detected)
;; 3- a third version actually goes on and does it, using strategy based on above estimates
;; compute maximum possible parallelization using the Hopcroft-Karp algorithm?
;;   http://en.wikipedia.org/wiki/Hopcroft–Karp_algorithm

;; TODO: for a scheduler,
;; 1- minimize total latency, maximize parallelism
;; 2- maximize ... minimize memory usage
;; 3- estimate cost by duration of previous successful runs (or last one)
;;   interpolated with known (+ K (size file)),
;;   using average from known files if new file, and 1 if all unknown.
;; 4- allow for a pure simulation, just adding up estimates.

(defmethod world-fifo-name :before (world)
  (unless (slot-boundp world 'fifo-name)
    (let* ((hash (world-hash world)))
      (loop :with dir := (subpathname
                          (merge-pathnames* +working-directory-subpathname+)
                          +fifo-subpathname+)
        :for i :from 0
        :for name := (format nil "~A/~A~@[-~A~]" dir hash (and (not (zerop i)) i))
        :for existing := (gethash name *fifo-name->computation*)
        :while existing
        :finally
        (setf (gethash name *fifo-name->computation*) world
              (world-fifo-name world) name)))))

(defun make-named-fifo (name)
  (isys:mkfifo name #o600))

(defun work-on-computation (env computation)
  (ecase (first (computation-command computation)) ;; should be using object dispatch instead!
    (:xcvb-driver-command
     (work-on-xcvb-driver-command env computation))
    ;; (:shell-command ...)
    (:compile-file-directly
     (work-on-direct-file-compilation env computation))))

(defclass process (iolib.os:process)
  ((should-exit-p :initarg :should-exit-p :accessor process-should-exit-p)))

(defun work-on-direct-file-compilation (env computation)
  (with-nesting ()
    (destructuring-bind (fullname &key cfasl) (cdr (computation-command computation)))
    (let* ((*renamed-targets* nil)
           (command (lisp-invocation-for
                     env ()
                     (compile-file-directly-shell-token env fullname :cfasl cfasl)))
           (process
            (iolib.os:create-process
             (car command) (cdr command)
             :stdin nil :stdout (NIY :log-file)
             :stderr (NIY :error-file))))
      (change-class process 'process :should-exit-p t)
      (setf (gethash process *pending-processes*)
            (lambda (successp)
              (NIY successp computation *renamed-targets*))))))

(defun work-on-xcvb-driver-command (env computation)
  (with-nesting ()
    (let* ((input-world? (first (computation-inputs computation)))
           (output-world? (first (computation-outputs computation)))
           (output-fifo
            (cond
              ((typep output-world? 'active-world)
               (let* ((fifo-name (world-fifo-name output-world?))
                      (output-fifo (make-named-fifo fifo-name)))
                 (values output-fifo)))))
           (continuation
            (NIY))))
    (progn
      (if (typep input-world? 'active-world)
          (let ((input-fifo (NIY 'world-fifo input-world?)))
            (NIY 'handle-computation input-fifo (world-handler input-world?)
                 output-fifo))
          (NIY 'create-new-process output-fifo))
      (NIY env computation continuation))))

(defun cpu-resources-available-p ()
  ;; use getloadavg
  t)

(defun computation-should-exit-p (computation)
  (null (world-futures (NIY 'computation-world computation))))

(defun event-step ()
  (iomux:event-dispatch *event-base* :one-shot t))

(defun handle-dead-children (env)
  (lambda (fd event exception)
    (declare (ignore event exception))
    (when (signalfd-signalled-p fd)
      (handle-signalling-children fd #'handle-dead-child)
      (maybe-issue-computation env))))

(defun handle-dead-child (pid status)
  (let ((finalizer (gethash pid *pending-processes*)))
    (assert finalizer)
    (remhash pid *pending-processes*)
    (funcall finalizer
             (and (isys:wifexited status) (zerop (isys:wexitstatus status))))))

(defun finalize-process (process successp)
  ;; TODO: terminate the whole thing gracefully, direct user to error log.
  (unless (process-should-exit-p process)
    (error "Process ~A exited unexpectedly" process))
  (unless successp
    (error "Process ~A failed" process))
  (map () #'handle-done-grain (computation-outputs (NIY :computation))))

(defun maybe-issue-computation (env)
  (when (and (ready-computation-p) (cpu-resources-available-p))
    (issue-one-computation env)))

(defun issue-one-computation (env)
  (when-bind (computation) (pick-one-computation-amongst-the-ready-ones)
    (issue-computation env computation)))

(defun pick-one-computation-amongst-the-ready-ones ()
  (loop :with best-computation = nil
    :with max-latency = 0
    :for computation :being :the :hash-values :of *ready-computations*
    :for latency = (gethash computation *expected-latencies*)
    :when (> latency max-latency) :do
    (setf best-computation computation max-latency latency)
    :finally (return best-computation)))

(defun issue-computation (env computation)
  (work-on-computation env computation)
  (setf (gethash computation *pending-processes*) t))

(defun ready-computation-p ()
  (not (zerop (hash-table-count *ready-computations*))))

(defun waiting-computation-p ()
  (not (zerop (hash-table-count *waiting-computations*))))

(defun pending-computation-p ()
  (not (zerop (hash-table-count *pending-processes*))))

(defun notify-users-grain-is-ready (grain)
  (map () #'notify-one-fewer-dependency (grain-users grain)))

(defun notify-one-fewer-dependency (computation)
  (when (zerop (decf (gethash computation *waiting-computations*)))
    (remhash computation *waiting-computations*)
    (mark-computation-ready computation)))

(defun mark-computation-ready (computation)
  (setf (gethash computation *ready-computations*) t))

(defun issue-initial-computations ()
  (loop :for grain :being :the :hash-values :of *grains*
    :when (and (null (grain-computation grain))
               (grain-users grain))
    :do (handle-done-grain grain)))

(defun work-in-progress-p ()
  (or (pending-computation-p) (ready-computation-p) (waiting-computation-p)))

(defun handle-done-grain (grain)
  (compute-grain-hash grain)
  (notify-users-grain-is-ready grain))

(defun setup-computation-model ()
  ;; Compute a static cost model.
  (setf *ready-computations* (make-hash-table)) ;; set of computations ready to be issued
  (setf *waiting-computations* (make-hash-table)) ;; set of computations waiting for inputs
  (setf *pending-processes* (make-hash-table :test 'equal)) ;; pid to pending computation
  (setf *expected-latencies* (make-hash-table)) ;; computation to latency evaluation
  (compute-latency-model *computations* :latencies *expected-latencies*)
  (loop :for c :in *computations* :do
    (setf (gethash c *waiting-computations*) (length (computation-inputs c)))))

(defun setup-event-loop (env)
  (setf *event-base* (make-instance 'iomux:event-base))
  (setf *sigchldfd* (quux-iolib::install-signalfd isys:SIGCHLD isys:SA-NOCLDSTOP))
  (iomux:set-io-handler *event-base* *sigchldfd* :read (handle-dead-children env))
  (iomux:add-timer *event-base* #'(lambda () (maybe-issue-computation env)) *poll-timeout*)
  (issue-initial-computations))

(defun run-event-loop ()
  (loop :while (work-in-progress-p) :do (event-step)))

(defun farm-out-world-tree (env)
  ;; TODO: parametrize the a- the scheduling b- the action itself (or lack thereof)
  (setup-computation-model)
  (setup-event-loop env)
  (run-event-loop))

(defun standalone-build (name)
  ;;#+DEBUG
  (trace handle-target graph-for build-command-for issue-dependency
         graph-for-fasls graph-for-image-grain make-computation issue-image-named
         simplified-xcvb-driver-command simplified-xcvb-driver-commands
         make-world-name intern-world-summary
         call-with-grain-registration register-computed-grain)
  (multiple-value-bind (fullname build) (handle-target name)
    (declare (ignore build))
    ;; TODO: use build for default pathname to object directory?
    (let* ((*use-master* nil)
           (*root-worlds* nil)
           (traversal (make-instance 'farmer-traversal)))
      (graph-for traversal fullname)
      (farm-out-world-tree traversal))))

(defparameter +standalone-build-option-spec+
  `((("build" #\b) :type string :optional nil :documentation "specify what system to build")
    (("setup" #\s) :type string :optional t :documentation "specify a Lisp setup file")
    ,@+source-registry-option-spec+
    (("output-path" #\o) :type string :initial-value "xcvb.mk" :documentation "specify output path")
    (("object-directory" #\O) :type string :initial-value "obj" :documentation "specify object directory")
    (("lisp-implementation" #\i) :type string :initial-value "sbcl" :documentation "specify type of Lisp implementation")
    (("lisp-binary-path" #\p) :type string :optional t :documentation "specify path of Lisp executable")
    (("disable-cfasl" #\C) :type boolean :optional t :documentation "disable the CFASL feature")
    (("verbosity" #\v) :type integer :initial-value 5 :documentation "set verbosity")
    (("base-image" #\B) :type boolean :optional t :initial-value t :documentation "use a base image")
    (("master" #\m) :type boolean :optional t :initial-value t :documentation "enable XCVB-master")
    (("debugging" #\Z) :type boolean :optional t :documentation "enable debugging")
    (("profiling" #\P) :type boolean :optional t :documentation "profiling")))

(defun standalone-build-command
    (&rest keys &key
     source-registry setup verbosity output-path
     build lisp-implementation lisp-binary-path
     disable-cfasl master object-directory base-image debugging profiling)
  (declare (ignore source-registry setup verbosity output-path
                   lisp-implementation lisp-binary-path
                   disable-cfasl master object-directory base-image debugging))
  (with-maybe-profiling (profiling)
    (apply 'handle-global-options keys)
    (standalone-build (canonicalize-fullname build))))
