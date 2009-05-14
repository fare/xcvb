(in-package :xcvb-static-backend)

(define-combinator-interpreter graph-for #'graph-for-atom)

(defun graph-for (env spec)
  ;; TODO: caching? yes, but based on full-names, not local names.
  ;; can we resolve local names early, as part of a first syntax-checking pass?
  (graph-for-interpreter env spec))

(defun graph-for* (spec)
  (graph-for () spec))

(defun load-command-for* (spec)
  (load-command-for #'graph-for* spec))

(defun graph-for-build-grain (grain)
  (check-type grain build-grain)
  (let* (;;(*operating-mode* :loading)
         ;;; Build pre-image if needed
         (pre-image
          (when (build-requires grain)
            (graph-for-image-grain
             (build-pre-image-name grain)
             (build-requires grain))))
         (*lisp-image-name*
          (if pre-image
            (fullname pre-image)
            *lisp-image-name*)))
    ;;; build post-image if needed
    (let* ((post-image-name (build-image-name grain))
           (dependencies (load-depends-on grain)))
      (if post-image-name
        (graph-for-image-grain
         post-image-name
         dependencies)
        (make-phony-grain :name `(:build ,(fullname grain))
                          :dependencies dependencies)))))

(defun graph-for-image-grain (name dependencies)
  (let* (;;(*operating-mode* :loading)
         (grain
          (make-grain 'image-grain
            :fullname `(:image ,name)))
         (dependency-grains
          (mapcar #'graph-for* dependencies)))
    (make-computation 'concrete-computation
      :outputs (list grain)
      :inputs dependency-grains
      :command
      `(:lisp
        (:image ,*lisp-image-name*)
        ,@(mapcar #'load-command-for* dependency-grains)
        (:save-image ,name)))
    grain))

(defun graph-for-atom (atom)
  (check-type atom string)
  (graph-for-lisp-module atom))

(defun graph-for-lisp-module (name)
  (resolve-module-name name *build*))

(define-graph-for :lisp (lisp)
  "Lisp file to load without compiling it"
  (graph-for-lisp-module lisp))

(defun commands-for-loading-lisp (lisp)
  (check-type lisp lisp-grain)
  (graph-for-lisp-module lisp)
  `(:load (:lisp ,(fullname lisp))))

(defun commands-for-loading-fasl-of (lisp)
  (check-type lisp lisp-grain)
  (graph-for-lisp-module lisp)
  `(:load (:lisp ,(fullname lisp))))

(defun commands-for-loading-cfasl-of (lisp)
  (check-type lisp lisp-grain)
  (graph-for-lisp-module lisp)
  `(:load (:lisp ,(fullname lisp))))

(defun graph-for-loading-lisp (lisp)
  (check-type lisp lisp-grain)
  (let* ((fullname (fullname lisp))
         ;;(*operating-mode* :compiling)
         (fasl (make-grain 'fasl-grain :fullname `(:fasl ,fullname)))
         (dependencies (mapcar #'graph-for* (compile-depends-on lisp)))
         (dependency-grains (mapcar #'graph-for* dependencies))
         (cfasl (if *use-cfasls*
                  (make-grain 'cfasl-grain :fullname `(:cfasl ,fullname))
                  fasl)))
    (make-computation
     'concrete-computation
     :outputs (if *use-cfasls* (list fasl cfasl) (list fasl))
     :inputs dependency-grains
     :command
     `(:lisp
       (:image ,*lisp-image-name*)
       ,@(mapcar #'(lambda (x) `(:load ,(fullname x))) dependency-grains)
       (:compile-lisp ,fullname)))
    (values fasl cfasl)))

(defun graph-for-compiling-lisp (lisp)
  (check-type lisp lisp-grain)
  (let* ((fullname (fullname lisp))
         (registered-fasl (registered-grain `(:fasl ,fullname))))
    (or registered-fasl
        (let* (;;(*operating-mode* :compiling)
               (fasl (make-grain 'fasl-grain :fullname `(:fasl ,fullname)))
               (dependencies (mapcar #'graph-for* (compile-depends-on lisp)))
               (dependency-grains (mapcar #'graph-for* dependencies))
               (cfasl (if *use-cfasls*
                        (make-grain 'cfasl-grain :fullname `(:cfasl ,fullname))
                        fasl)))
          (make-computation
           'concrete-computation
           :outputs (if *use-cfasls* (list fasl cfasl) (list fasl))
           :inputs dependency-grains
           :command
           `(:lisp
             (:image ,*lisp-image-name*)
             ,@(mapcar #'(lambda (x) `(:load ,(fullname x))) dependency-grains)
             (:compile-lisp ,fullname)))
          (values fasl cfasl)))))

(defun graph-for-lisp-dependencies (grain)
  (check-type grain lisp-grain)
  (dolist (dep (append (compile-depends-on grain)
                       (load-depends-on grain)))
    (graph-for* dep)))
