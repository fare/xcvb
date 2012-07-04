#+xcvb (module (:depends-on ("macros")))

(in-package :xcvb)

;;; Lists
(defun list-of-length-p (n x)
  (length=n-p x n))

(defun mapcar/ (function env arguments)
  (mapcar (lambda (x) (funcall function env x)) arguments))

;;; hash tables
(defun sequence-function-map (function sequence &key key)
  (let ((map (make-hash-table :test 'equal)))
    (flet ((index (x)
             (let ((k (if key (funcall key x) x)))
               ;; more general would be to merge results when it appears multiple times
               ;; instead of dropping subsequent appearances. But this is enough for our purposes.
               (unless (nth-value 1 (gethash k map))
                 (setf (gethash k map) (funcall function x))))))
      (map () #'index sequence))
    map))

(defun sequence-position-map (sequence)
  (let ((index -1))
    (flet ((index (x) (declare (ignore x)) (incf index)))
      (sequence-function-map #'index sequence))))


;;; I/O
(defun readable-string (x &key (package :cl) output)
  (with-output (output)
    (with-safe-io-syntax ()
      (let ((*package* (find-package package)))
        (write x :stream output :readably t :escape t :pretty nil)
        (terpri output)))))

;;; Filesystem
(defun find-proper-ancestor (dir properf)
  (loop :for x = (pathname-directory-pathname dir)
    :then (pathname-parent-directory-pathname x) :do
    (cond
      ((funcall properf x) (return x))
      ((member (pathname-directory x) '(() (:absolute)) :test 'equal)
       (return nil)))))

;;; Environment control
;;; This better be moved to some portability package...
(defun setenv (name value &optional (overwrite t))
  (or #+sbcl (sb-posix:setenv name value (if overwrite 1 0))
      #+clozure (ccl:setenv name value overwrite)
      #+clisp (unless (and (not overwrite) (ext:getenv name)) (system::setenv name value))
      (error "~S not supported in your implementation" 'setenv)))
