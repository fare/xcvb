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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel)
  (require :sb-posix))
#+sbcl
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
(defun maybe-reap-specified-subprocess (pidspec options)
  (multiple-value-bind (pid status) (posix-waitpid pidspec options)
    (unless (zerop pid)
      (reap-subprocess pid status)
      t)))
(defun maybe-reap-one-subprocess (&key nohang)
  (unless (zerop (hash-table-count *subprocesses*))
    (if *only-reap-registered-subprocesses*
      (loop :with options = (posix-waitpid-options :nohang t)
        :thereis (loop :for pid :being :the :hash-keys :of *subprocesses*
                   :thereis (maybe-reap-specified-subprocess pid options))
        :until nohang :do (sleep .1))
      (maybe-reap-specified-subprocess -1 (posix-waitpid-options :nohang nohang)))))
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

(defun in-fork (in-fork &optional (reaper (constantly nil)))
  (finish-outputs)
  (let ((pid (posix-fork)))
    (cond
      ((zerop pid) ;; child
       (initialize-child-process in-fork))
      (t
       (add-subprocess pid reaper)
       pid))))

(defun read-eval-loop (&optional (i *standard-input*))
  (format *error-output* "~&Starting read-eval-loop at ~36R" (get-universal-time))
  (loop :with eof = '#:eof
    #|:with size = 4096
    :with buffer = (make-array size :element-type 'character)
    :for start = 0 :then newstart
    :for end = 0 :then newend
    :for position = (- end start)
    :for () = (replace buffer buffer :start1 0 :end1 position :start2 start :end2 end)
    :for newend = (read-sequence buffer i :start position :end size)
    :for (form newstart) =
    (multiple-value-list (read-from-string buffer nil eof :start 0 :end newend))|#
    :for form = (read-preserving-whitespace i nil eof)
    :until (eq form eof) :do
    (format *error-output* "~&At ~36R received form~%  ~S~%" (get-universal-time) form)
    (finish-outputs)
    (let ((values (multiple-value-list (eval form))))
      (format *error-output* "~&; At ~36R it returned~%~{  ~S~%~}" (get-universal-time) values))
    (finish-outputs))
  (format *error-output* "~&; EOF at ~36R~%" (get-universal-time))
  (finish-outputs))

(defun work-on-job (id inpath outpath errpath commands close)
  (when close (close *error-output*))
  (with-open-file (*error-output* errpath :direction :output
                                  :if-exists :append :if-does-not-exist :create)
    (when close
      (setf *stderr* *error-output*)
      (close *standard-output*))
    (with-open-file (*standard-output* outpath :direction :output
                                       :if-exists :overwrite :if-does-not-exist :create)
      (write-sequence (format nil "(:ready ~S)~%" id) *standard-output*) ; handshake
      (finish-outputs)
      (when close (close *standard-input*))
      (with-open-file (*standard-input* inpath
                                        :direction :input :if-does-not-exist :error)
        (let ((*trace-output* *error-output*))
          (run-commands commands))))))

(defun fork-job (id inpath outpath errpath &rest commands)
  ;; NB: we open the new input/output pipes from the parent,
  ;; and close them right after the fork,
  ;; so that closing of the pipe be a signal to the controller
  ;; that the controlled process is dead.
  ;; Note that the controller process will have to open the pipe
  ;; in non-blocking mode, and to select on it.
  (format *error-output* "Forked process ~D to run job ~S ~S ~S ~S~{ ~S~}~%"
          (in-fork
           (lambda ()
             (work-on-job id inpath outpath errpath commands t))) ;; t
          id inpath outpath errpath commands)
  (finish-outputs))

(defun execute-job (id inpath outpath errpath &rest commands)
  (work-on-job id inpath outpath errpath commands nil)
  (format *error-output* "Terminating process at ~36R" (get-universal-time))
  (quit 0))

(defun run-job (id &rest commands)
  (let ((*standard-output* *error-output*))
    (format t "~&Running job ~S at ~36R:~%  ~{ ~S~}~%" id (get-universal-time) commands)
    (finish-outputs)
    (run-commands commands)
    (format t "~&Done with job ~S at ~36R~%" id (get-universal-time)))
  (write-sequence (format nil "(:done ~S)~%" id) *standard-output*)
  (finish-outputs))
