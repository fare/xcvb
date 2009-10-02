#+xcvb (module (:depends-on ("dependencies-interpreter" "traversal")))
(in-package :xcvb)

#|
This provides a simplifying traversal that conflates all kind of dependencies
into just one kind. Used by non-enforcing backends such as the ASDF backend
and the non-enforcing Makefile backend.
|#

(defclass simplifying-traversal (xcvb-traversal)
  ())

(defmethod issue-load-command ((env simplifying-traversal) command)
  (declare (ignorable env command))
  (values))

(defmethod issue-dependency ((env simplifying-traversal) (grain fasl-grain))
  (issue-dependency env (graph-for env `(:lisp ,(second (fullname grain))))))

(define-load-command-for :lisp ((env simplifying-traversal) name)
  (load-command-for-fasl env name))

(defvar *require-dependencies* nil
  "A list of require features we depend upon")

(define-load-command-for :require ((env simplifying-traversal) feature)
  (pushnew feature *require-dependencies* :test 'string-equal)
  nil)

(defmethod graph-for-build-grain ((env simplifying-traversal) grain)
  (load-command-for* env (build-dependencies grain))
  (load-command-for* env (compile-dependencies grain))
  (load-command-for* env (cload-dependencies grain))
  (load-command-for* env (load-dependencies grain))
  nil)

(define-graph-for :fasl ((env simplifying-traversal) name)
  (check-type name string)
  (let* ((grain (resolve-absolute-module-name name))
	 (fullname (if grain (fullname grain) (error "Couldn't resolve ~S to a lisp module" name)))
	 (generator (gethash fullname *generators*)))
    (check-type grain lisp-grain)
    (handle-lisp-dependencies grain)
    (issue-dependency env grain)
    (let* ((dependencies
            (remove-duplicates
             (append (build-dependencies grain)
                     (when generator (generator-dependencies generator))
                     (compile-dependencies grain)
                     (cload-dependencies grain)
                     (load-dependencies grain))
             :test 'equal :from-end t))
           (fasl
            (make-grain 'fasl-grain :fullname `(:fasl ,name)
                        :load-dependencies ())))
      (load-command-for* env dependencies)
      (make-computation
       ()
       :outputs (list fasl)
       :inputs (traversed-dependencies env)
       :command nil)
      fasl)))

(define-graph-for :lisp ((env simplifying-traversal) name)
  (resolve-absolute-module-name name))

(defvar *asdf-system-dependencies* nil
  "A list of asdf system we depend upon")

(define-graph-for :asdf ((env simplifying-traversal) system-name)
  (pushnew system-name *asdf-system-dependencies* :test 'equal)
  nil)