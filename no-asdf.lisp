#+xcvb (module (:build-depends-on nil))
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
;;    Then comes the question of how to fix CL so that it becomes possible
;; to safely redefine a function, generic function, structure, variable,
;; constant, symbol-macro, etc.
;;
;; Note for SBCL: <nyef> _deepfire: I have a possibly-more-complete "solution" for package destruction. For each symbol to be destroyed, and each non-nil element in the array sb-c::*info-types*, (sb-c::clear-info-value <symbol> <element>).

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
                 (when p
                   (map () #'del (package-used-by-list p))
                   (let ((name (package-name p)))
                     (format t "~&Deleting package ~A~%" name)
                     (delete-package p)
                     #+sbcl
                     (when (find name sb-impl::*modules* :test 'string=)
                       (format t "~&Unregistering required module ~A~%" name)
                       (setf sb-impl::*modules* (remove name sb-impl::*modules*  :test 'string=)))))))
        (del (find-package :common-lisp-controller))
        (del package))
      (format t "~&Deleted old instance of ASDF.~%")))
  (setf *features* (delete-if
                    (lambda (x) (member x '(:asdf :clc-os-debian :common-lisp-controller)))
                    *features*))
  ;;#+sbcl (pushnew :sbcl-hooks-require *features*) ;; not useful for ASDF 2.
  (format t "*features* = ~S~%" *features*)
  (finish-output)
  (values))
