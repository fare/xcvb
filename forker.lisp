;;; XCVB forker. To be used with the XCVB driver compile images.
#|
Goals and Constraints:
* Should provide a XCVB farmer with a listener to execute arbitrary commands
* Should be able to fork a new listener for the XCVB farmer
* Should extend the XCVB driver
* Should not otherwise have any dependencies beyond what the implementation provides,
* It is OK to fail to reap a completely hosed process -- in these cases,
 it is up to the driving XCVB to take action.

Design choices:
* the XCVB farmer will use mkfifo to create something the forker can simply with-open-file,
 rather than anything requiring lots of filesystem or socket or pipe-relaying infrastructure.
 Maybe for even increased portability we could communicate only through files,
 but there's no simple way to synchronize on that.

Some code taken from POIU.
|#

#+xcvb
(module
 (:author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT" ;; MIT-style license. See LICENSE
  :description "XCVB Forker"
  :long-description "Driver code to be loaded in all buildee images for XCVB."
  :compile-depends-on ((:fasl "/xcvb/driver"))
  :load-depends-on ("/xcvb/driver")))


(in-package :xcvb-driver)

(defun process-result (status result-pipe)
  (prog1
      (if (= 0 (posix-wexitstatus status))
        (read result-pipe nil nil)
        *failed-process-result*)
    (close result-pipe)))

(defun process-return (proc result)
  (prin1 result (status-pipe proc)))

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


#+clozure
(progn

(defun posix-fork ()
  (ccl::finish-outputs)
  (ccl:external-call "fork" :int))

(defun posix-close (x)
  (ccl::fd-close x))

(defun posix-setpgrp ()
  (ccl::external-call "setpgrp" :int))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun posix-waitpid-options (&key nohang untraced)
  (logior (if nohang #$WNOHANG 0) (if untraced #$WUNTRACED))))

(defun posix-waitpid (pid options)
  (ccl::rlet ((status :signed))
    (let* ((retval (ccl::external-call "waitpid" :address status :signed options :signed)))
      (values retval (ccl::pref status :signed)))))

(defun posix-wexitstatus (x)
  (ccl::wexitstatus x))

(defun posix-pipe ()
  (ccl::pipe))

);#+clozure

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
         (posix-exit 0))
    (posix-exit 99)))

(defun spawn-fork (in-fork &optional (reaper (constantly nil)))
  (let ((pid (posix-fork)))
    (cond
      ((zerop pid) ;; parent
       (add-subprocess pid reaper))
      (t
       (initialize-child-process in-fork)))))

(defun start-listener (inpath outpath)
  (with-open-file (o outpath :direction :output
                     :if-exists :overwrite :if-does-not-exist :error)
    (with-open-file (i inpath :direction :input :if-does-not-exist :error)
      (let ((*standard-input* i)
            (*standard-output* o))
        (read-eval-loop i)))))

(defun read-eval-loop (&optional (i *standard-input*))
  (loop :with eof = '#:eof
    :for form = (read i nil eof)
    :until (eq form eof)
    :do (eval form)))

(defun spawn-listener (inpath outpath)
  (spawn-fork (lambda () (start-listener inpath outpath))))
