#+xcvb
(module
  (:depends-on ("makefile-backend" "simplifying-traversal")))

(in-package :xcvb)

(defclass nem-traversal (simplifying-traversal)
  ())

;;;; TODO: fix the code below HERE... XXX...

(defmethod graph-for-build-grain ((env nem-traversal) grain)
  (load-command-for* env (compile-dependencies grain))
  (load-command-for* env (load-dependencies grain)))

(define-graph-for :asdf ((env nem-traversal) system-name)
  (pushnew system-name *asdf-system-dependencies* :test 'equal))

(defun write-non-enforcing-makefile (build-names &key output-path)
  "Write a Makefile to output-path with information about how to compile the specified BUILD
in a fast way that doesn't enforce dependencies."
  (error "Not implemented Yet")
  (let* ((*print-pretty* nil); otherwise SBCL will slow us down a lot.
         (builds (mapcar #'registered-build build-names))
         (last-build (first (last builds)))
         (default-output-path (merge-pathnames "xcvb-ne.mk" (grain-pathname last-build)))
         (output-path (merge-pathnames output-path default-output-path))
         (makefile-path (ensure-absolute-pathname output-path))
         (makefile-dir (pathname-directory-pathname makefile-path))
         (*default-pathname-defaults* makefile-dir)
         (*makefile-target-directories* nil)
         (*makefile-phonies* nil))
    (log-format 6 "T=~A building dependency graph~%" (get-universal-time))
    ;;XXXX (graph-for-build-grain (make-instance 'static-traversal) build)
    ;;XXXX also create .asd files for each stage... poiu-
    (log-format 6 "T=~A building makefile~%" (get-universal-time))
    (let ((body
           (with-output-to-string (s)
             (dolist (computation *computations*)
               (write-computation-to-makefile s computation)))))
      (with-open-file (out makefile-path
                           :direction :output
                           :if-exists :supersede)
        (log-format 6 "T=~A printing makefile~%" (get-universal-time))
        (write-makefile-prelude out)
        (princ body out)
        (write-makefile-conclusion out))))
        (log-format 6 "T=~A done~%" (get-universal-time)))
