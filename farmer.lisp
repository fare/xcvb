#+xcvb
(module
 (:depends-on
  ("macros" "specials" "static-traversal" "profiling" "main" "driver-commands")))

#|
* TODO: a private per-run toplevel directory for fifos and error logs.

* TODO: debug current scheduler by commenting out tthsum thing,
  having a trivial fake job worker that returns immediately,
  and computes the time. Make that a parameter.

* TODO: between the scheduler and the worker backend,
  there is a layer of state management (1-to-n relationship
  between states and workers) that might require its own layer,
  including management of keeping the successor world alive
  if and only if it itself has users.

* TODO: complete a simple local worker backend based on the forker,
  i.e. communicate with a descendant through named pipes.

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

(defvar *working-directory*)
(defvar *logs-directory*)
(defvar *fifo-directory*)

(defvar *environment* nil);;DEBUG only
(defvar *process* nil);;DEBUG only

(defun new-work-directory (base)
  (ensure-directories-exist base)
  (loop :for i :from 1
    :for n = (format nil "~A~D" base i)
    :for newdir = (ignore-errors (isys:mkdir n #o755) t)
    :when newdir :return (values n i)))

(defvar *worker-id* -1)

(defclass worker ()
  ())
(defgeneric worker-send (worker form)
  (:documentation "send a form to be executed on the worker"))
(defmethod worker-send (worker (x cons))
  (worker-send worker (readable-string x)))

(defgeneric world-pending-futures (world))

(defclass active-world (world-grain buildable-grain)
  ((futures
    :initform nil
    :accessor world-futures
    :documentation "a list of computations in the future of this world")
   (pending-futures
    :accessor world-pending-futures
    :documentation "a count of active computations in the future of this world")
   (hash :accessor world-hash)
   (input-fifo :accessor world-input-fifo :initform nil)
   (output-fifo :accessor world-output-fifo :initform nil)
   (process :accessor world-process :initform nil)
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
(defun intern-world-summary (setup commands-r
                             &optional key-thunk fun)
  "Assuming no other world with same summary exists,
make a world with summary from specified SETUP and COMMANDS-R,
additional keys resulting from evaluating the KEY-THUNK,
and extra finalization from calling FUN on the world."
  ;; TODO: 1- use tthsum
  ;; 2- have the summary only include the hash of previous world and what changes from it.
  (let ((fullname (make-world-name setup commands-r)))
    (call-with-grain-registration
     fullname
     (lambda ()
       (let* ((summary (make-world-summary setup commands-r))
              (hash (world-summary-hash summary)))
         (loop
           :for w :in (gethash hash *worlds*)
           :when (equal summary (world-summary w))
           :do (error "new world already hashed??? ~S" w))
         (let ((world (apply #'make-instance 'active-world
                             :fullname fullname
                             :hash hash
                             (when key-thunk (funcall key-thunk)))))
           (push world (gethash hash *worlds* '()))
           (when fun (funcall fun world))
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
               (simplified-xcvb-driver-commands commands))))))

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
  (cond
    ((or (eq :compile-file-directly (first command))
        (equal '(:fasl "/xcvb/forker") (fullname (first outputs))))
     (call-next-method))
    (t
     (multiple-value-bind (setup commands)
         (simplified-xcvb-driver-command command)
       (make-xcvb-driver-computations env inputs outputs setup commands)))))

(defun make-xcvb-driver-computations (env inputs outputs setup commands)
  (declare (ignore inputs))
  (loop
    :for command = nil :then (if commands (pop commands) (return computation))
    :for commands-r = nil :then (cons command commands-r)
    :for grain-name = (unwrap-load-file-command (second command))
    :for grain = (when grain-name (registered-grain grain-name))
    :for previous = nil :then world
    :for world = (when commands
                   (intern-world-summary setup commands-r))
    :for () = (DBG :mxdc commands-r commands grain-name grain previous world)
    :for old-computation = (and world (slot-boundp world 'computation))
    :for computation = (if old-computation
                           (grain-computation world)
                           (make-computation
                            ()
                            :inputs (append (when previous (list previous))
                                            (setup-dependencies env setup)
                                            (when grain (list grain)))
                            :outputs (if commands (list world) outputs)
                            :command (if previous
                                         `(:active-command ,@(rest command))
                                         `(:start-world ,setup))))
    :when (and previous (not old-computation)) :do
    (push computation (world-futures previous))))

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
  (when (typep grain 'file-grain)
    (setf (grain-content-digest grain)
          (tthsum-for-file (grain-namestring *environment* grain)))))

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

(defmethod world-pending-futures :before (world)
  (unless (slot-boundp world 'pending-futures)
    (setf (world-pending-futures world) (length (world-futures world)))))

(defun make-fifo (name)
  (ensure-directories-exist name)
  (isys:mkfifo name #o600))

(defun open-fifo (name direction &key block)
  (apply 'make-file-stream name :direction direction :block block
         (ecase direction
           (:input '(:if-exists nil :if-does-not-exist :error))
           (:output '(:if-exists :overwrite :if-does-not-exist :error)))))

(defun make-and-open-input-fifo (name)
  (make-fifo name)
  (open-fifo name :input))

(defun work-on-computation (env computation)
  (incf *worker-id*)
  (ecase (first (computation-command computation)) ;; should be using object dispatch instead!
    ((nil)
     (mark-computation-done computation))
    (:compile-file-directly
     (work-on-direct-file-compilation env computation))
    (:xcvb-driver-command
     (work-on-xcvb-driver-command env computation))
    (:start-world
     (work-on-starting-world env computation))
    (:active-command
     (work-on-active-command env computation)))
  (values))

(defclass process* (iolib.os:process)
  ((id :initarg :id :reader worker-id)
   (logpath :initarg :logpath :reader process-logpath :initform nil)
   (renamed-targets :initarg :renamed-targets :reader renamed-targets :initform nil)
   (continuation :initarg :continuation :reader process-continuation)
   (world :initarg :world :reader process-world :initform nil)
   (should-exit-p :initform nil
                  :initarg :should-exit-p
                  :accessor process-should-exit-p)))

(defun fifopath (id suffix)
  (format nil "~A~A~A" *fifo-directory* id suffix))

(defun logpath (id)
  (format nil "~A~A" *logs-directory* id))

(defun start-process* (command &key continuation should-exit-p id world
                       (renamed-targets *renamed-targets*))
  (with-nesting ()
    (let ((logpath (logpath id)))
      (ensure-directories-exist logpath)
      (with-open-file-stream (log logpath :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :rename-and-delete)
        (shell-tokens-to-Makefile command log)
        (terpri log)))
    (let ((process
           (iolib.os:create-process
            (car command) (cdr command)
            :stdin :null
            :stdout (list logpath :append)
            :stderr :stdout)))
      (change-class process 'process*
                    :id id :should-exit-p should-exit-p
                    :logpath logpath :world world
                    :renamed-targets renamed-targets
                    :continuation continuation)
      (setf (gethash (process-pid process) *pending-processes*) process)
      process)))

(defun rename-targets (renamed-targets)
  (loop :for (target . tmppath) :in renamed-targets :do
    (rename-file-overwriting-target tmppath target)))

(defun unregister-and-close (x)
  (when x
    (DBG :uac x)
    (ignore-errors (unregister-handler x))
    (close x)))

(defun close-world (world)
  (when world
    (DBG :close-world world)
    (with-slots (input-fifo output-fifo process) world
      (unregister-and-close input-fifo)
      (unregister-and-close output-fifo)
      (setf input-fifo nil output-fifo nil))))

(defun finalize-process (process successp)
  (setf *process* process) ;DEBUG
  (with-slots (id logpath continuation world renamed-targets
                  should-exit-p (pid iolib.os::pid)) process
    (DBG :finalize-process successp pid id logpath continuation world renamed-targets should-exit-p)
    (unless successp
      (error "process ~D failed.~%See logs in ~A"
             pid logpath))
    (unless should-exit-p
      (error "process ~D exited unexpectedly.~%See logs in ~A"
             pid logpath))
    (close-world world)
    (rename-targets renamed-targets)
    (when continuation
      (funcall continuation))))

(defun work-on-direct-file-compilation (env computation)
  (destructuring-bind (fullname &key cfasl) (cdr (computation-command computation))
    (let* ((*renamed-targets* nil)
           (command (lisp-invocation-arglist-for
                     env ()
                     (compile-file-directly-shell-token env fullname :cfasl cfasl))))
      (start-process*
       command
       :id *worker-id*
       :should-exit-p t
       :continuation (lambda () (mark-computation-done computation))))))

(defun work-on-xcvb-driver-command (env computation)
  (destructuring-bind (token setup &rest commands) (computation-command computation)
    (assert (eq :xcvb-driver-command token))
    (let* ((*renamed-targets* nil)
           (command (lisp-invocation-arglist-for
                     env setup
                     (xcvb-driver-commands-to-shell-token env commands))))
      (start-process*
       command
       :id *worker-id*
       :should-exit-p t
       :continuation (lambda () (mark-computation-done computation))))))

(defun unregister-handler (x)
  (iomux:remove-fd-handlers *event-base* (iolib.streams:fd-of x)))

(defun register-read-handler (stream handler)
  (iomux:set-io-handler
   *event-base* (iolib.streams:fd-of stream)
   :read (lambda (fd event exception)
           (declare (ignore fd event exception))
           (funcall handler))))

(define-text-for-xcvb-driver-command :do-work (env id in out err &rest commands)
  (text-for-xcvb-driver-helper
   env ()
   ":do-work ~S ~S ~S ~S~{ ~A~}" id in out err
   (mapcar/ 'text-for-xcvb-driver-command env commands)))

(define-text-for-xcvb-driver-command :spawn-worker (env id in out err &rest commands)
  (text-for-xcvb-driver-helper
   env ()
   ":spawn-worker ~S ~S ~S ~S~{ ~A~}" id in out err
   (mapcar/ 'text-for-xcvb-driver-command env commands)))

(define-text-for-xcvb-driver-command :read-eval-loop (env)
  (text-for-xcvb-driver-helper env () ":read-eval-loop"))

(define-text-for-xcvb-driver-command :remote-work (env id &rest commands)
  (text-for-xcvb-driver-helper
   env ()
   ":remote-work ~S~{ ~A~}" id
   (loop :for c :in commands :collect (text-for-xcvb-driver-command env c))))

(defun expect-sexp (sexp stream)
  (DBG :expect-sexp sexp stream)
  (assert (equal sexp (safe-read stream))))

(defun register-handshake (id outfifo inpath world continuation)
  (DBG :register-handshake id outfifo inpath world continuation)
  (register-read-handler
   outfifo (lambda () (do-handshake id outfifo inpath world continuation))))

(defun do-handshake (id outfifo inpath world continuation)
  (expect-sexp `(:ready ,id) outfifo)
  (setf (world-input-fifo world) (open-fifo inpath :output :block t))
  (unregister-handler outfifo)
  (DBG :register-continuation id outfifo inpath world continuation)
  (register-read-handler outfifo continuation))

(defun work-on-starting-world (env computation)
  (destructuring-bind (token setup &rest commands) (computation-command computation)
    (assert (eq :start-world token))
    (let* ((*renamed-targets* nil)
           (id *worker-id*)
           (outputs (computation-outputs computation))
           (inpath (fifopath id ".in"))
           (outpath (fifopath id ".out"))
           (logpath (logpath id))
           (outfifo (progn
                      (make-fifo inpath)
                      (make-and-open-input-fifo outpath)))
           (world (progn (assert (null (cdr outputs)))
                         (first outputs)))
           (command (lisp-invocation-arglist-for
                     env setup
                     (xcvb-driver-commands-to-shell-token
                      env `((:do-work ,id ,inpath ,outpath ,logpath
                                      (:remote-work ,id ,@commands)
                                      (:read-eval-loop))))))
           (process
            (start-process*
             command
             :world world
             :id id)))
      (setf (world-process world) process)
      (setf (world-output-fifo world) outfifo)
      (register-handshake
       id outfifo inpath world
       (lambda ()
         (expect-sexp `(:done ,id) outfifo)
         (unregister-handler outfifo)
         (mark-computation-done computation))))))

(defun dummy-world ()
  (make-instance
   'active-world
   :fullname :dummy
   :issued-build-commands nil
   :included-dependencies nil
   :hash nil))

(defun work-on-active-command (env computation)
  (destructuring-bind (token &rest commands) (computation-command computation)
    (assert (eq token :active-command))
    (let* ((id *worker-id*)
           (*renamed-targets* nil)
           (world (first (computation-inputs computation)))
           (command-fifo (world-input-fifo world))
           (pending-futures (decf (world-pending-futures world)))
           (inpath (fifopath id ".in"))
           (outpath (fifopath id ".out"))
           (logpath (logpath id))
           (outfifo (progn
                      (make-fifo inpath)
                      (make-and-open-input-fifo outpath)))
           (work-command `(:remote-work ,id ,@commands))
           (output-world (first (computation-outputs computation)))
           (output-world? (typep output-world 'active-world))
           (output-world (if output-world? output-world (dummy-world)))
           (work-and-listen-commands
            (cons work-command
                  (when output-world? '((:read-eval-loop)))))
           (command
            `(,(if (plusp pending-futures) :spawn-worker :do-work)
              ,id ,inpath ,outpath ,logpath
              ,@work-and-listen-commands)))
      (setf (world-output-fifo output-world) outfifo)
      (when (zerop pending-futures)
        (when (world-process world)
          (setf (process-should-exit-p (world-process world)) t)))
      (let ((text
             (format nil "(xcvb-driver:run-command '~A)~%"
                    (text-for-xcvb-driver-command env command))))
        (DBG :woac id text command-fifo world)
        (write-sequence text command-fifo)
        (finish-output command-fifo))
      (when (zerop pending-futures)
        (close-world world))
      (register-handshake
       id outfifo inpath output-world
       (let ((renamed-targets *renamed-targets*))
         (lambda ()
           (expect-sexp `(:done ,id) outfifo)
           (unregister-handler outfifo)
           (rename-targets renamed-targets)
           (mark-computation-done computation)))))))

(defun cpu-resources-available-p ()
  ;; use getloadavg
  t)

(defun event-step ()
  (iomux:event-dispatch *event-base* :one-shot t))

(defun handle-dead-children (env)
  (lambda (fd event exception)
    (declare (ignore event exception))
    (when (signalfd-signalled-p fd)
      (handle-signalling-children #'handle-dead-child)
      (maybe-issue-computation env))))

(defun handle-dead-child (pid status)
  (let ((process (gethash pid *pending-processes*)))
    (assert process)
    (remhash pid *pending-processes*)
    (finalize-process process
                      (and (not (zerop (isys:wifexited status)))
                           (zerop (isys:wexitstatus status))))))

(defun maybe-issue-computation (env)
  (when (and (ready-computation-p) (cpu-resources-available-p))
    (issue-one-computation env)))

(defun issue-one-computation (env)
  (when-bind (computation) (pick-one-computation-amongst-the-ready-ones)
    (issue-computation env computation)))

(defun pick-one-computation-amongst-the-ready-ones ()
  (loop :with best-computation = nil
    :with max-latency = 0
    :for computation :being :the :hash-keys :of *ready-computations*
    :for latency = (gethash computation *expected-latencies*)
    :do (DBG :pocatro best-computation max-latency computation latency)
    :when (> latency max-latency) :do
    (setf best-computation computation max-latency latency)
    :finally (return best-computation)))

(defun issue-computation (env computation)
  (remhash computation *ready-computations*)
  (work-on-computation env computation))

(defun ready-computation-p ()
  (not (zerop (hash-table-count *ready-computations*))))

(defun waiting-computation-p ()
  (not (zerop (hash-table-count *waiting-computations*))))

(defun pending-process-p ()
  (not (zerop (hash-table-count *pending-processes*))))

(defun mark-computation-done (computation)
  (dolist (output (computation-outputs computation))
    (handle-done-grain output)))

(defun handle-done-grain (grain)
  (compute-grain-hash grain)
  (notify-users-grain-is-ready grain))

(defun notify-users-grain-is-ready (grain)
  (map () #'notify-one-fewer-dependency (grain-users grain)))

(defun notify-one-fewer-dependency (computation)
  (DBG :nofd computation (gethash computation *waiting-computations*))
  (when (zerop (decf (gethash computation *waiting-computations*)))
    (remhash computation *waiting-computations*)
    (mark-computation-ready computation)))

(defun mark-computation-ready (computation)
  (setf (gethash computation *ready-computations*) t))

(defun issue-initial-computations ()
  (loop :for grain :being :the :hash-values :of *grains* :using (:hash-key name)
    :when (and (equal name (fullname grain))
               (null (grain-computation grain)))
    :do (handle-done-grain grain)))

(defvar *break-count* 0)
(defvar *break-mod* 20)
(defun break? ()
  (when (zerop (mod (incf *break-count*) *break-mod*))
    (break)))

(defun work-in-progress-p ()
  (DBG :wipp (hash-table->alist *pending-processes*)
       (hash-table->alist *ready-computations*)
       (hash-table->alist *waiting-computations*))
  ;; (break?)
  (or (pending-process-p) (ready-computation-p) (waiting-computation-p)))

(defun setup-computation-model ()
  ;; Compute a static cost model.
  (setf *ready-computations* (make-hash-table)) ;; set of computations ready to be issued
  (setf *waiting-computations* (make-hash-table)) ;; set of computations waiting for inputs
  (setf *pending-processes* (make-hash-table :test 'equal)) ;; pid to pending computation
  (setf *expected-latencies* (make-hash-table)) ;; computation to latency evaluation
  (compute-latency-model *computations* :latencies *expected-latencies*)
  (loop :for c :in *computations*
    :for n = (length (computation-inputs c)) :do
    (if (zerop n)
        (setf (gethash c *ready-computations*) t)
        (setf (gethash c *waiting-computations*) n))))

(defun setup-event-loop (env)
  (setf *event-base* (make-instance 'iomux:event-base))
  (setf *sigchldfd* (install-signalfd isys:SIGCHLD isys:SA-NOCLDSTOP))
  (iomux:set-io-handler *event-base* *sigchldfd* :read (handle-dead-children env))
  (iomux:add-timer *event-base* #'(lambda () (maybe-issue-computation env)) *poll-timeout*))

(defun run-event-loop ()
  (loop :while (work-in-progress-p) :do (event-step)))

(defun check-computation-model ()
  ;;(break) ; TODO: better debugging
  (values))

(defun farm-out-world-tree (env)
  ;; TODO: parametrize the a- the scheduling b- the action itself (or lack thereof)
  (setup-computation-model)
  (check-computation-model)
  (setup-event-loop env)
  (issue-initial-computations)
  (run-event-loop))

(defun standalone-build (name)
  ;;#+DEBUG
  (trace
   do-handshake
   finalize-process
   handle-dead-child
   handle-done-grain
   install-signalfd
   iolib.os:create-process
   iomux:add-timer
   iomux:set-io-handler
   issue-computation
   issue-initial-computations
   issue-one-computation
   logpath
   mark-computation-done
   notify-one-fewer-dependency
   notify-users-grain-is-ready
   open-fifo
   pick-one-computation-amongst-the-ready-ones
   run-event-loop
   setup-event-loop
   shell-tokens-to-Makefile
   start-process*
   unwrap-load-file-command
   work-on-active-command
   work-on-computation
   work-on-direct-file-compilation
   work-on-starting-world
   work-on-xcvb-driver-command)

  (setf *print-pretty* nil)
  (multiple-value-bind (fullname build) (handle-target name)
    (declare (ignore build))
    ;; TODO: use build for default pathname to object directory?
    (let* ((*use-master* nil)
           (*root-worlds* nil)
           (traversal (make-instance 'farmer-traversal)))
      (setf *working-directory* (new-work-directory (strcat *object-directory* "/_work/")))
      (setf *fifo-directory* (strcat *working-directory* "/fifo/"))
      (setf *worker-id* -1)
      (setf *logs-directory* (strcat *working-directory* "/logs/"))
      #|#+DEBUG|# (setf *environment* traversal)
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
    (append1f *lisp-setup-dependencies* '(:fasl "/xcvb/forker"))
    (standalone-build (canonicalize-fullname build))))
