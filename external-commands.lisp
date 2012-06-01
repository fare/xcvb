;;; Lisp implementations
#+xcvb (module (:depends-on ("macros")))

(in-package :xcvb)

;; ECL is very special, with its link model.
(defun target-ecl-p ()
  (eq *lisp-implementation-type* :ecl))

(defun xcvb-driver-commands-to-shell-token (env commands)
  (with-output-to-string (s)
    (write-string "(xcvb-driver::run " s)
    (dolist (c commands)
      (write-string (text-for-xcvb-driver-command env c) s))
    (write-string ")" s)))

;; Renaming of targets ensures reasonable atomicity
;; whereas CL implementations may create bad invalid stale output files
;; when interrupted in the middle of their computation,
;; -- whether a bad bug is found in the way the user stresses the compiler,
;; or the process is killed in the midst of an unsuccessful debug attempt,
;; or the plug is simply pulled on the computer.
;; This isn't done in the target Lisp side, because
;; CL implementations don't usually do that for you implicitly, and
;; while we could do it explicitly for :compile-lisp,
;; doing it for :create-image would be a pain in at least SBCL,
;; where we would have to fork and wait for a subprocess to SAVE-LISP-AND-DIE
;; which would make the target driver much more complex than desired.

(defvar *renamed-targets* ()
  "alist of targets really desired, and the temporary names under which the XCVB driver commands
will create the desired content. An atomic rename() will have to be performed afterwards.")
(makunbound '*renamed-targets*) ; catch those who try to use it outside of proper context!

(defun register-renamed-target (target tempname)
  (push (cons target tempname) *renamed-targets*)
  t)

(defun rename-target (target tempname)
  (register-renamed-target target tempname)
  tempname)

(defun tempname-target (target)
  (let* ((target (pathname target))
         (tempname (make-pathname :name (strcat (pathname-name target) "__temp")
                                   :defaults target)))
    (rename-target target tempname)))

(define-simple-dispatcher external-commands-for-computation #'external-commands-for-computation-atom)

(defun external-commands-for-computation-atom (env computation-command)
  (declare (ignore env))
  (if (null computation-command)
      nil ;; nothing to do!
      (error "Invalid computation ~S" computation-command)))

(defun external-commands-for-computation (env computation-command)
  ;; We rename secondary targets first, according to the theory that
  ;; in case of interruption, the primary target will be re-built which will
  ;; cause the secondary targets to be implicitly re-built before success.
  (let* ((*renamed-targets* nil)
         (commands (external-commands-for-computation-dispatcher env computation-command)))
    (append commands
	    (loop :for (target . tempname) :in *renamed-targets*
		  :collect (list "mv" (native-namestring tempname) (native-namestring target))))))

(define-external-commands-for-computation :xcvb-driver-command (env keys &rest commands)
  (list
   (lisp-invocation-for env keys (xcvb-driver-commands-to-shell-token env commands))))

(define-external-commands-for-computation :compile-file-directly
    (env fullname &key cfasl lisp-object)
  (list
   (lisp-invocation-for env ()
    (compile-file-directly-shell-token env fullname :cfasl cfasl :lisp-object lisp-object))))

(define-external-commands-for-computation :progn (env &rest commands)
  (loop :for command :in commands
    :append (external-commands-for-computation env command)))

#|
(define-external-commands-for-computation :exec-command (env &rest argv)
  (declare (ignore env))
  (list argv))
|#

(define-external-commands-for-computation :make-manifest (env manifest &rest commands)
  (list
   (cmdize *xcvb-program* 'make-manifest
           :output (pseudo-effective-namestring env manifest)
           :spec (let ((manifest-spec (commands-to-manifest-spec env commands)))
                   (with-safe-io-syntax ()
                     (write-to-string manifest-spec :case :downcase))))))
