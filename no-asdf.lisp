#+xcvb (module ())
(in-package :cl-user)

;; I created this no-asdf file because some implementations (including
;; any implementation distributed on debian that uses debian's antique
;; cl-asdf 1.111-1 as of 2009-09-09, such as clisp 2.48, etc.) come with
;; an ASDF so old that SYSTEM-SOURCE-FILE is a DEFUN rather than a
;; DEFGENERIC, which causes the compilation of a recent asdf.lisp to fail.
;; This no-asdf essentially deletes the ASDF package to avoid this error
;; and any similar error in the future.
;;    Now, on SBCL we also need to un-register a symbol from that package
;; from an internal data-structure by which SBCL allows the functionality
;; of CL:REQUIRE and CL:PROVIDE to be extended. Yet, we need push a magic
;; feature so that same functionality will be provided by any future load
;; of ASDF. Pphew.
;;    All this cannot be done by ASDF itself, because most of the time,
;; loading ASDF on top of a previous ASDF *should* be idempotent and not
;; destroy existing configuration and send your Lisp to limbo.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((package (find-package :asdf))
        #+sbcl (mpf 'sb-ext:*module-provider-functions*))
    (when package
      (format t "~&Removing previous installation of ASDF ~A~%"
              (or (symbol-value (find-symbol "*ASDF-REVISION*" :asdf)) "(revision unspecified)"))
      #+sbcl (set mpf (delete-if (lambda (x)
                                   (when (and (symbolp x) (eq package (symbol-package x)))
                                     (format t "~&Deleting ~S from ~S" x mpf)
                                     t))
                                 (symbol-value mpf)))
      ;; Recursively delete packages that use ASDF (e.g. if your Lisp was compiled with C-L-C)
      (labels ((del (p)
                 (map () #'del (package-used-by-list p))
                 (let ((name (package-name p)))
                   (format t "~&Deleting package ~A~%" name)
                   (delete-package p)
                   (when (find name sb-impl::*modules* :test 'string=)
                     (format t "~&Unregistering required module ~A~%" name)
                     (setf sb-impl::*modules* (remove name sb-impl::*modules*  :test 'string=))))))
        (del package))
      (format t "~&Deleted old instance of ASDF.~%")))
  #+sbcl (pushnew :sbcl-hooks-require *features*)
  (format t "*features* = ~S~%" *features*)
  (finish-output)
  (values))

;; Default optimization setting: let's debug stuff.
(proclaim '(optimize (speed 2) (safety 3) (debug 3) (compilation-speed 0)))
