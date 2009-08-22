#+xcvb (module ())
(in-package :cl-user)

;; I created this no-asdf file because some implementations (like the clisp
;; distributed in debian) come with an ASDF so old that SYSTEM-SOURCE-FILE
;; is a DEFUN rather than a DEFGENERIC, which causes the compilation of a
;; recent asdf.lisp to fail. This no-asdf essentially deletes the ASDF
;; package to avoid this error and any similar error in the future.
;;    Now, on SBCL we also need to un-register a symbol from that package
;; from an internal data-structure by which SBCL allows the functionality
;; of CL:REQUIRE and CL:PROVIDE to be extended. Yet, we need push a magic
;; feature so that same functionality will be provided by any future load
;; of ASDF. Pphew.
;;    All this cannot be done by ASDF, because most of the time, loading
;; ASDF on top of a previous ASDF *should* be idempotent and not destroy
;; existing configuration and send your Lisp to limbo.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((package (find-package :asdf)))
    (when package
       #+sbcl (setf sb-ext:*module-provider-functions*
                    (delete-if (lambda (x)
                                 (when (and (symbolp x) (eq package (symbol-package x)))
                                   (format t "~&Deleted ~S from sb-ext:*module-provider-functions*" x)
                                   t))
                               sb-ext:*module-provider-functions*))
       (delete-package :asdf)
       (format t "~&Deleted old instance of ASDF.~%")))
  #+sbcl (pushnew :sbcl-hooks-require *features*)
  (format t "*features* = ~S~%" *features*)
  (finish-output)
  (values))

(proclaim '(optimize (speed 1) (safety 3) (debug 3)))
