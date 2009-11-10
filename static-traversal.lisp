#+xcvb (module (:depends-on
		("specials" "grain-interface" "dependencies-interpreter")))
;;TODO: rename to static-traversal

(in-package :xcvb)

(defclass enforcing-traversal (xcvb-traversal)
  ())

(defclass static-traversal (enforcing-traversal)
  ((included-dependencies
    :initform (make-hashset :test 'equal)
    :accessor included-dependencies
    :documentation "dependencies included in the current world, as a set")
   (issued-build-commands
    :initform (make-hashset :test 'equal)
    :accessor issued-build-commands
    :documentation "load commands issued so far to build the current world, as a set")
   (build-commands-r
    :initform nil
    :accessor traversed-build-commands-r
    :documentation "load commands issued so far to build the current world, in reverse order")))

(defmethod dependency-already-included-p ((env static-traversal) grain)
  (or (gethash grain (included-dependencies env))
      (call-next-method)))

(defmethod issue-build-command ((env static-traversal) command)
  (unless (gethash command (issued-build-commands env))
    (setf (gethash command (issued-build-commands env)) t)
    (push command (traversed-build-commands-r env))))

(defun graph-for-compiled (env spec)
  (graph-for env (compiled-dependency spec)))

(defmethod graph-for-atom (env name)
  (graph-for env `(:lisp ,name)))

(defun include-image-dependencies (env image)
  (when image
    (check-type image image-grain)
    (setf (included-dependencies env)
          (make-hashset :test 'equal :set (included-dependencies image)))))

(define-graph-for :lisp ((env static-traversal) name)
  (let* ((grain (resolve-absolute-module-name name))
	 (fullname (if grain (fullname grain) (error "Couldn't resolve ~S to a lisp module" name)))
	 (generator (gethash fullname *generators*)))
    (check-type grain lisp-grain)
    (when (and generator (not (generator-computation generator)))
      (let ((dependencies (append (build-dependencies grain) (generator-dependencies generator)))
	    (targets (generator-targets generator)))
	(unless dependencies
	  (error "graph-for-lisp: Need dependencies to generate file ~S.~%" fullname))
	(dolist (target targets)
          (slot-makunbound target 'computation))
        (pre-image-for env grain)
        (build-command-for* env dependencies)
        (setf (generator-computation generator)
              (make-computation
               ()
               :outputs targets
               :inputs (traversed-dependencies env)
               :command
               `(:xcvb-driver-command
                 ,(image-setup env)
                 ,@(traversed-build-commands env))))))
    grain))

(define-graph-for :build (env name)
  (graph-for-build-named env name))

(define-graph-for :compile-build (env name)
  (graph-for-build-named env name))

(defmethod graph-for-build-named (env name)
  (graph-for-build-grain env (registered-build name)))

(defmethod graph-for-build-grain :before (env (grain build-grain))
  (declare (ignore env))
  (handle-lisp-dependencies grain))

(defmethod graph-for-build-grain ((env enforcing-traversal) (grain build-grain))
  (let ((post-image-name (build-image-name grain)))
    (if post-image-name
        (graph-for env `(:image ,post-image-name))
        (make-phony-grain
         :name `(:build ,(fullname grain))
	 :dependencies
	 (progn
           ;;(build-command-for* env (compile-dependencies grain))
           ;;(build-command-for* env (cload-dependencies grain))
           (build-command-for* env (load-dependencies grain))
           (traversed-dependencies env))))))

(define-graph-for :image ((env static-traversal) name)
  (cond
    ((null name) ; special: no image
     nil)
    ((equal name "/_") ;; special: initial image
     (graph-for-image-grain env name nil nil))
    ((string-prefix-p "/_pre/" name)
     (let* ((build-name (subseq name 5))
            (build (registered-build build-name)))
       (check-type build build-grain)
       (handle-lisp-dependencies build)
       (let* ((dependencies (build-dependencies build))
              (starting-build-name (build-starting-dependencies-p dependencies))
              (starting-build-image-name
               (when starting-build-name
                 (build-post-image-name (registered-build starting-build-name)))))
         (graph-for-image-grain env name (or starting-build-image-name "/_") dependencies))))
    (t
     (let* ((build (registered-build name)))
       (graph-for-image-grain
        env name (build-pre-image-name build) (load-dependencies build))))))

(defun pre-image-for (env grain)
  (issue-image-named env (build-pre-image-name grain)))

(defun issue-image-named (env name)
  (if name
    (let ((image (graph-for env `(:image ,name))))
      (issue-dependency env image)
      (include-image-dependencies env image)
      (setf (image-setup env) `(:image ,(fullname image)))
            image)
    (progn
      (setf (image-setup env)
            `(:load ,(loop
                       :for dep :in *lisp-setup-dependencies*
                       :for grain = (graph-for env dep)
                       :do (issue-dependency env grain)
                       :collect (fullname grain))))
      nil)))

(defun make-load-file-command (fullname)
  `(:load-file ,fullname))
(defun unwrap-load-file-command (x)
  (when (and (list-of-length-p 2 x) (eq (first x) :load-file))
    (second x)))
(defun remove-load-file (x)
  (or (unwrap-load-file-command x)
      (error "cannot remove :load-file from ~S" x)))

(defun require-command-p (x)
  (and (list-of-length-p 2 x) (eq (first x) :require)))


(defun manifest-and-build-commands (name image-setup build-commands)
  (if (not *use-master*)
    (values nil build-commands)
    (let* ((initial-loads (getf image-setup :load))
           (initial-name (strcat name "--initial")))
      (values
       (append
        (when initial-loads
          `((:make-manifest
             (:manifest ,initial-name)
             ,@(mapcar #'make-load-file-command initial-loads))))
        (when build-commands
          `((:make-manifest
             (:manifest ,name)
             ,@build-commands))))
       (append
        (when initial-loads
          `((:initialize-manifest (:manifest ,initial-name))))
        (when build-commands
          `((:load-manifest (:manifest ,name)))))))))

(defmethod graph-for-image-grain ((env static-traversal) name pre-image-name dependencies)
  (let ((pre-image (issue-image-named env pre-image-name)))
    (build-command-for* env dependencies)
    (let* ((traversed (traversed-dependencies env))
           (build-commands-r (traversed-build-commands-r env))
           (build-commands (reverse build-commands-r))
           (image-setup (image-setup env))
           (manifest-and-build-commands
            (multiple-value-list
             (manifest-and-build-commands name image-setup build-commands)))
           (manifest-maker (first manifest-and-build-commands))
           (build-commands-spec (second manifest-and-build-commands))
           (world (make-instance
                   'world-grain
                   :fullname `(:world :setup ,(canonicalize-image-setup image-setup)
                                      :commands-r ,build-commands-r)
                   :issued-build-commands
                   (make-hashset :test 'equal :list build-commands-r)
                   :included-dependencies
                   (make-hashset :test 'equal
                                 :set (when pre-image (included-dependencies pre-image))
                                 :list traversed)))
           (grain
            (make-grain 'image-grain
                        :fullname `(:image ,name)
                        :world world)))
      (make-computation ()
       :outputs (list grain)
       :inputs traversed
       :command
       `(:progn
          ,@manifest-maker
          (:xcvb-driver-command
           ,image-setup
           (:create-image
            (:image ,name) ,@build-commands-spec))))
      grain)))

(define-graph-for :source (env name &key in)
  (declare (ignore env))
  (make-source-grain :name name :in in))

(defun make-source-grain (&key name in)
  (make-instance
   'source-grain
   :computation nil
   :name name
   :in in
   :fullname `(:source ,name :in ,in)))

(define-graph-for :fasl ((env enforcing-traversal) lisp-name)
  (first (graph-for-fasls env lisp-name)))

(define-graph-for :cfasl ((env enforcing-traversal) lisp-name)
  (second (graph-for-fasls env lisp-name)))

(defun setup-dependencies-before-fasl (fullname)
  (assert (equal '(:fasl "/xcvb/driver") (car *lisp-setup-dependencies*)))
  (reverse ; put back in order
   (cdr ; skip the current dependency itself
    (member `(:fasl ,fullname) ; what is up to the current dependency
            (reverse *lisp-setup-dependencies*)
            :test #'equal))))

(defmethod make-computation ((env static-traversal) &rest keys &key &allow-other-keys)
  (apply #'make-computation () keys))

(defmethod graph-for-fasls ((env static-traversal) fullname)
  (check-type fullname string)
  (let* ((lisp (graph-for env `(:lisp ,fullname)))
         (fullname (fullname lisp)) ;; canonicalize the fullname
         (driverp (equal fullname "/xcvb/driver"))
         (specialp (member `(:fasl ,fullname) *lisp-setup-dependencies* :test #'equal)))
    (check-type lisp lisp-grain)
    (handle-lisp-dependencies lisp)
    (let ((build-dependencies (if specialp
                                  (setup-dependencies-before-fasl fullname)
                                  (build-dependencies lisp)))
          (compile-dependencies (compile-dependencies lisp))
          (cload-dependencies (cload-dependencies lisp))
          (load-dependencies (load-dependencies lisp)))
      (issue-dependency env lisp)
      (unless specialp
        (pre-image-for env lisp))
      (build-command-for* env build-dependencies)
      (build-command-for* env compile-dependencies)
      (let* ((outputs (fasl-grains-for-name
                       env fullname
                       load-dependencies cload-dependencies
                       build-dependencies)))
        (make-computation
         env
         :outputs outputs
         :inputs (traversed-dependencies env)
         :command
         (if driverp
           `(:xcvb-driver-command
             ()
             (:compile-file-directly ,fullname ,(second outputs)))
           `(:xcvb-driver-command
             ,(if specialp '(:load ((:fasl "/xcvb/driver"))) (image-setup env))
             (:compile-lisp (,fullname) ,@(traversed-build-commands env)))))
        outputs))))
