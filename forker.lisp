#+xcvb
(module
 (:build-depends-on nil
  :depends-on
  ("/xcvb/driver"
   (:when (:featurep :sbcl)
     (:require :sb-grovel)
     (:require :sb-posix)))))

(in-package :xcvb-driver)

#+sbcl ;;; SBCL specific fork support
(progn
;; Simple heuristic: if we have allocated more than the given ratio
;; of what is allowed between GCs, then trigger the GC.
;; Note: can possibly modify parameters and reset in sb-ext:*after-gc-hooks*
(defparameter *prefork-allocation-reserve-ratio* .80) ; default ratio: 80%

(defun should-i-gc-p ()
  (let ((available-bytes (- (sb-alien:extern-alien "auto_gc_trigger" sb-alien:long)
                            (sb-kernel:dynamic-usage)))
        (allocation-threshhold (sb-ext:bytes-consed-between-gcs)))
    (< available-bytes (* *prefork-allocation-reserve-ratio* allocation-threshhold))))

(defun posix-fork ()
  (when (should-i-gc-p)
    (sb-ext:gc))
  (sb-posix:fork))

(defun posix-close (x)
  (sb-posix:close x))

(defun posix-setpgrp ()
  (sb-posix:setpgrp))

(defun posix-waitpid (pid options)
  (sb-posix:waitpid pid options))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun posix-waitpid-options (&key nohang untraced)
  (logior (if nohang sb-unix:wnohang 0)
          (if untraced sb-unix:wuntraced 0))))

(defun posix-wexitstatus (x)
  (sb-posix:wexitstatus x))

(defun posix-pipe ()
  (sb-posix:pipe))

);#+sbcl


#+clozure ;;; Clozure CL specific fork support
(progn

(defun posix-fork ()
  (finish-outputs)
  (ccl:external-call "fork" :int))

(defun posix-close (x)
  (ccl::fd-close x))

(defun posix-setpgrp ()
  (ccl::external-call "setpgrp" :int))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun posix-waitpid-options (&key nohang untraced)
  (logior (if nohang (load-time-value (read-from-string "#$WNOHANG")) 0)
          (if untraced (load-time-value (read-from-string "#$WUNTRACED")) 0))))

(defun posix-waitpid (pid options)
  (ccl::rlet ((status :signed))
    (let* ((retval (ccl::external-call
                    "waitpid"
                    :signed pid :address status :signed options :signed)))
      (values retval (ccl::pref status :signed)))))

(defun posix-wexitstatus (x)
  (ccl::wexitstatus x))

(defun posix-pipe ()
  (ccl::pipe))

);#+clozure

#+clisp ;;; CLISP specific fork support
(progn

(defun posix-fork ()
  (linux:fork))

(defun posix-close (x)
  (linux:close x))

(defun posix-setpgrp ()
  (posix:setpgrp))

(defun posix-waitpid (pid options)
  (multiple-value-list (apply #'posix:wait :pid pid options)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun posix-waitpid-options (&rest keys &key nohang untraced)
  (declare (ignore nohang untraced))
  keys))

(defun posix-wexitstatus (x)
  (if (eq :exited (second x))
    (third x)
    (cons (second x) (third x))))

(defun posix-pipe ()
  (linux:pipe))

);#+clisp

(defun xdelete (sequence item &rest keys)
  (apply #'delete item sequence keys))
(define-modify-macro deletef (item &rest keys)
  xdelete "Delete item from list")

;;; Do we claim to catch all forks? Probably.
;;; Then we need a hook per process.
;;(defvar *subprocesses-lock* (make-lock))
(defvar *subprocesses* (make-hash-table :test 'equal))
(defvar *only-reap-registered-subprocesses* nil)
(defun add-subprocess (pid handler)
  (setf (gethash pid *subprocesses*) handler))
(defun reap-subprocess (pid status)
  (multiple-value-bind (handler foundp) (gethash pid *subprocesses*)
    (unless foundp
      (error "Trying to reap unregistered PID ~D" pid))
    (remhash pid *subprocesses*)
    (funcall handler status)))
(defun maybe-reap-one-subprocess (&key nohang)
  (unless (zerop (hash-table-count *subprocesses*))
    (if *only-reap-registered-subprocesses*
      (loop :with options = (posix-waitpid-options :nohang t)
        :thereis (loop :for pid :being :the :hash-keys :of *subprocesses*
                   :thereis (maybe-reap-specified-subprocess pid options))
        :until nohang :do (sleep .1))
      (maybe-reap-specified-subprocess -1 (posix-waitpid-options :nohang nohang)))))
(defun maybe-reap-specified-subprocess (pidspec options)
  (multiple-value-bind (pid status) (posix-waitpid pidspec options)
    (unless (zerop pid)
      (reap-subprocess pid status)
      t)))
(defun reap-subprocesses (&key nohang)
  (loop :while (maybe-reap-one-subprocess :nohang nohang)))

(defun initialize-child-process (thunk)
  (setf *subprocesses* (make-hash-table :test 'equal))
  ;; run atfork equivalent hooks?
  ;; reset stack, free resources from other threads
  (unwind-protect
       (progn
         (funcall thunk)
         (quit 0))
    (quit 99)))

(defun spawn-fork (in-fork &optional (reaper (constantly nil)))
  (finish-outputs)
  (let ((pid (posix-fork)))
    (cond
      ((zerop pid) ;; child
       (initialize-child-process in-fork))
      (t
       (add-subprocess pid reaper))))
  (values))

(defun read-eval-loop (&optional (i *standard-input*))
  (loop :with eof = '#:eof
    :for form = (read i nil eof)
    :until (eq form eof)
    :do (eval form)))

(defun spawn-listener (inpath outpath)
  ;; NB: we open the new input/output pipes from the parent,
  ;; and close them right after the fork,
  ;; so that closing of the pipe be a signal to the controller
  ;; that the controlled process is dead.
  ;; Note that the controller process will have to open the pipe
  ;; in non-blocking mode, and to select on it.
  (with-open-file (o outpath :direction :output
                     :if-exists :overwrite :if-does-not-exist :create)
    (with-open-file (i inpath :direction :input :if-does-not-exist :error)
      (spawn-fork
       (lambda ()
         (close *standard-input*)
         (close *standard-output*)
         (let ((*standard-input* i)
               (*standard-output* o)
               (*standard-error* o))
           (read-eval-loop))))
      (close o)
      (close i))))
