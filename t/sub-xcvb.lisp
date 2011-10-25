#+xcvb (module (:depends-on ("helpers" "specials")))

(in-package #:xcvb-test)

(declaim (optimize (speed 1) (debug 3) (safety 3)))

(defsuite* (sub-xcvb
            :in xcvb-test
            :documentation "Test XCVB as a subprocess"))

(defun run-make (dir target &rest keys)
  (apply 'run-cmd "make" "-C" dir target (make-environment keys)))

(defun normalize-environment-var (key)
  (substitute #\_ #\- (string-upcase key)))

(defun make-environment (keys)
  (loop :for xspec :in
    '(:install-bin :install-image :install-lisp :install-source :install-systems
      :install-xcvb :prefix
      (:object-dir "XCVB_OBJECT_DIRECTORY") (:source-registry "CL_SOURCE_REGISTRY")
      (:implementation-type "LISP" string-downcase)
      :path :xdg-cache-home :cl-launch-flags :asdf-output-translations
      :parallelize)
    :for spec = (ensure-list xspec)
    :for key = (first spec)
    :for var = (or (second spec) (normalize-environment-var key))
    :for fun = (or (third spec) 'identity)
    :for val = (getf keys key)
    :when val :collect (format nil "~A=~A" var (funcall fun val))))

(defun upkeywordp (x)
  (find-symbol (string-upcase x) :keyword))

(defun compute-makefile-configuration-variables (&rest keys)
  (loop :with config = (apply 'extract-makefile-configuration keys)
    :for xspec :in
    '(;; :install-bin :install-image :install-lisp
      ;; :install-xcvb :install-source :install-systems
      ;; (:object-dir "XCVB_OBJECT_DIRECTORY")
      ;; (:source-registry "CL_SOURCE_REGISTRY")
      ;; (:implementation-type "LISP" upkeywordp)
      :cl-launch :cl-launch-flags :cl-launch-mode)
    :for spec = (alexandria:ensure-list xspec)
    :for key = (first spec)
    :for var = (or (second spec) (normalize-environment-var key))
    :for fun = (or (third spec) 'identity)
    :for val = (association var config :test 'equal)
    :when val :do (setf (getf keys key) (funcall fun val)))
  keys)

(defun extract-makefile-configuration (&rest keys &key xcvb-dir &allow-other-keys)
  (loop
    :for line :in (apply 'run-cmd/lines "make" "-C" xcvb-dir "show-config"
                         (make-environment keys))
    :for pos = (position #\= line)
    :for name = (subseq line 0 pos)
    :for value = (cond
                   ((null pos)
                    ;;(format *error-output* "Ignoring line ~S~%" line)
                    nil)
                   ((= pos (1- (length line)))
                    "")
                   (t
                    (subseq line (1+ pos) nil)))
    :when value :collect (cons name value)))

(defmacro compute-xcvb-dir-variables! (keys &rest args)
  `(setf ,keys (apply 'compute-xcvb-dir-variables ,@args ,keys)))

(defmacro compute-release-dir-variables! (keys &rest args)
  `(setf ,keys (apply 'compute-release-dir-variables ,@args ,keys)))

(defmacro compute-makefile-configuration-variables! (keys)
  `(setf ,keys (apply 'compute-makefile-configuration-variables ,keys)))


(defun validate-xcvb-checkout (&key xcvb-dir &allow-other-keys)
  (is (probe-file* (in-dir xcvb-dir "xcvb.asd"))
      "Missing xcvb.asd in XCVB directory ~S" xcvb-dir)
  (is (probe-file* (in-dir xcvb-dir "driver.lisp"))
      "Missing driver.lisp in XCVB directory ~S" xcvb-dir)
  (is (probe-file* (in-dir xcvb-dir "configure.mk"))
      "Please configure your configure.mk in ~S~%~
       (and don't forget to properly setup ASDF)" xcvb-dir))

(defun validate-release-checkout (&key release-dir &allow-other-keys)
  (is (probe-file* release-dir)
      "Release directory ~S doesn't exist" release-dir)
  (is (in-dir release-dir "INSTALL")
      "Release directory ~S fails to contain INSTALL" release-dir)
  (is (in-dir release-dir "xcvb/")
      "Release directory ~S fails to contain xcvb/" release-dir)
  (validate-xcvb-dir :xcvb-dir (in-dir release-dir "xcvb/")))

(defun get-makefile-configuration (&rest keys
                                   &key xcvb-dir makefile-configuration &allow-other-keys)
  (or makefile-configuration
      (apply 'extract-makefile-configuration :xcvb-dir xcvb-dir keys)))

(defun ensure-makefile-configuration (keys)
  (if (getf keys :makefile-configuration) keys
      (list* :makefile-configuration (apply 'get-makefile-configuration keys) keys)))

(defun get-system-dependencies ()
  (loop :for dep :in +xcvb-dependencies+
    :append (destructuring-bind (build &key systems upstream repo) dep
              (declare (ignore build upstream repo))
              systems)))

(defun validate-asdf-setup-command (&key cl-launch-flags &allow-other-keys)
  (format nil "cl-launch ~A -s asdf -i ~S"
          cl-launch-flags
          (with-safe-io-syntax (:package :cl-user)
            (format nil "(cl-launch::quit (if ~S 0 1))"
                    `(loop :with cl-user::good = t
                       :for cl-user::x :in ',(get-system-dependencies)
                       :for cl-user::s = (asdf:find-system cl-user::x nil) :do
                       (cond
                         (cl-user::s (format t "Found system ~(~A~) in ~A~%" cl-user::x
                                             (asdf:system-source-directory cl-user::s)))
                         (t (setf cl-user::good nil)
                            (format t "Missing system ~(~A~)~%" cl-user::x)))
                       :finally (return cl-user::good))))))

(defun validate-asdf-setup (&rest keys)
  (apply 'run-cmd (apply 'validate-asdf-setup-command keys)))

(defun lisp-long-name (impl)
  (ecase impl
    ((nil) "")
    ((:sbcl) "SBCL")
    ((:ccl) "Clozure Common Lisp")
    ((:clisp) "CLISP")))

(defun validate-xcvb-version (&key xcvb implementation-type &allow-other-keys)
  (let ((lines (run-cmd/lines xcvb 'version)))
    (is (string-prefix-p "XCVB version " (first lines)))
    (is (string-prefix-p (strcat "built on " (lisp-long-name implementation-type))
                         (second lines)))))

(defun validate-xcvb-ssr (&rest keys &key xcvb xcvb-dir source-registry &allow-other-keys)
  ;; preconditions: XCVB built from DIR into BUILD-DIR using ENV
  ;; postconditions: xcvb ssr working
  (let ((ssr (run-cmd/string
              xcvb 'ssr :source-registry
              (format nil "~A//:~@[~A~]" xcvb-dir source-registry))))
    (unless (cl-ppcre:scan "\\(:build \"/xcvb\" :in-file \".*/build.xcvb\"\\)" ssr)
      (DBG :vxs xcvb keys ssr)
      (sleep 1000000))
    (is (cl-ppcre:scan "\\(:build \"/xcvb\" :in-file \".*/build.xcvb\"\\)" ssr)
        "Can't find build for xcvb in~%~A~%" ssr)
    (is (search "(:asdf \"xcvb\" :superseded-by (:build \"/xcvb\"))" ssr)
        "can't find superseded asdf for xcvb in~%~A~%" ssr)
    (is (cl-ppcre:scan "\\(:invalid-build :registry-conflict \"/xcvb/test/conflict/b\" :among \\(\".*/examples/conflict/b2?/build.xcvb\" \".*/examples/conflict/b2?/build.xcvb\"\\)\\)" ssr)
        "can't find conflict for /xcvb/test/conflict/b in~%~A~%" ssr)))

(defun validate-xcvb (&rest keys &key implementation-type &allow-other-keys)
  ;; preconditions: env, xcvb built, PWD=.../xcvb/
  (apply 'validate-xcvb-version keys) ;; is the built xcvb what we think it is?
  (apply 'validate-xcvb-ssr keys) ;; can it search its search path?
  (apply 'validate-unit-tests keys) ; does it pass unit tests?
  (case implementation-type
    (:sbcl (apply 'validate-a2x keys))) ; can it migrate a2x-test from xcvb?
  (apply 'validate-rmx keys) ; can it remove the xcvb annotations from a2x-test?
  (apply 'validate-x2a keys) ; can it convert hello back to asdf?
  ;; (apply 'validate-simple-build-backend) ; can it build hello with the standalone backend?
  ;; (validate-farmer-backend) ; can it build hello with the standalone backend?
  (apply 'validate-mk-backend keys) ; can it build hello with the Makefile backend?
  (case implementation-type
    ((:sbcl :ccl :clisp)
     (apply 'validate-nemk-backend keys))) ;; can it build hello with the non-enforcing Makefile backend?
  (apply 'validate-master keys) ; does the xcvb master work?
  (apply 'validate-slave keys) ; does the xcvb slave work?
  (apply 'validate-bridge keys)) ; does the xcvb bridge work?

(defun validate-hello (&key install-bin &allow-other-keys)
  (is (equal '("hello, world")
             (run-cmd/lines (in-dir install-bin "hello") "-t"))
      "hello not working"))

(defun ensure-file-deleted (file)
  (ignore-errors (delete-file file)))

(defun validate-hello-build (build-target &rest keys
                             &key install-bin install-image xcvb-dir
                             &allow-other-keys)
  (ensure-directories-exist install-bin)
  (ensure-directories-exist install-image)
  (flet ((clean ()
           (ensure-file-deleted (in-dir xcvb-dir "examples/hello/setup.lisp"))
           (ensure-file-deleted (in-dir install-bin "hello"))))
    (clean)
    (apply 'run-make (in-dir xcvb-dir "examples/hello/") build-target keys)
    (apply 'validate-hello keys)
    (clean)))

(defun validate-mk-backend (&rest keys)
  (apply 'validate-hello-build "hello" keys))

(defun validate-nemk-backend (&rest keys &key parallelize implementation-type &allow-other-keys)
  (apply 'validate-hello-build "hello-using-nemk"
         :parallelize (and parallelize (not (eq implementation-type :clisp)))
         keys))

(defun validate-x2a (&rest keys)
  (apply 'validate-hello-build "hello-using-asdf" keys))

(defun validate-rmx (&rest keys &key xcvb build-dir xcvb-dir source-registry &allow-other-keys)
  (let* ((test-dir (in-dir build-dir "a2x_rmx/"))
         (source-registry (format nil "~A//:~A" test-dir source-registry)))
    (ensure-directories-exist test-dir)
    (rsync "-a" (in-dir xcvb-dir "examples/a2x/") test-dir)
    (apply 'validate-xcvb-ssr :source-registry source-registry keys)
    (run-cmd xcvb 'rmx :build "/xcvb/test/a2x"
             :verbosity 9 :source-registry source-registry)
    (is (not (probe-file* (in-dir test-dir "build.xcvb")))
        "xcvb rmx failed to remove build.xcvb")
    (dolist (l (is (directory (merge-pathnames* #p"*.lisp" test-dir))))
      (is (not (module-form-p (read-module-declaration l)))
          "xcvb rmx failed to delete module form from ~A" l))))

(defun validate-a2x (&key xcvb build-dir xcvb-dir source-registry &allow-other-keys)
  (let* ((test-dir (in-dir build-dir "a2x_a2x/"))
         (source-registry (format nil "~A//:~A" test-dir source-registry)))
    (ensure-directories-exist test-dir)
    (rsync "-a" (in-dir xcvb-dir "examples/a2x/") test-dir)
    (run-cmd xcvb 'a2x :system "a2x-test" :name "/xcvb/test/a2x"
             :source-registry source-registry)
    (is (probe-file* (in-dir test-dir "build.xcvb"))
      "xcvb a2x failed to create build.xcvb")
    (dolist (l (is (directory (merge-pathnames* #p"*.lisp" test-dir))))
      (is (module-form-p (read-module-declaration l))
          "xcvb a2x failed to create module form for ~A" l))
    (rm-rfv test-dir)))

(defun validate-master (&key xcvb build-dir object-dir source-registry implementation-type
                        &allow-other-keys)
  (let* ((driver
          (is (first
               (run-cmd/lines
                xcvb 'find-module :name "xcvb/driver" :short))))
         (out
          (apply 'run-cmd/string
           (xcvb::lisp-invocation-arglist
            :implementation-type implementation-type :lisp-path nil :load driver
            :eval (format nil "'(#.(xcvb-driver:bnl \"xcvb/hello\" ~
              :output-path ~S :object-directory ~S :source-registry ~S :verbosity 9) ~
              #.(let ((*print-base* 30)) (xcvb-hello::hello :name 716822547 :traditional t)))"
                          build-dir object-dir source-registry)))))
    (is (search "hello, tester" out)
        "Failed to use hello through the XCVB master")))

(defun validate-slave (&key xcvb build-dir object-dir implementation-type &allow-other-keys)
  (let ((out
         (run-cmd/string
          xcvb 'slave-builder :build "/xcvb/hello"
          :lisp-implementation (string-downcase implementation-type)
          :object-directory object-dir :output-path build-dir)))
    (is (search "Your desires are my orders" out)
        "Failed to drive a slave ~(~A~) to build hello" implementation-type)))

(defun validate-bridge (&key object-dir implementation-type &allow-other-keys)
  (let ((out
         (run-program/read-output-string
          (xcvb::lisp-invocation-arglist
           :implementation-type implementation-type
           :eval (format nil "'(#.(require ~S)~
                   #.(asdf:load-system :xcvb-driver)~
                   #.(setf xcvb-driver:*object-directory* ~S)~
                   #.(asdf:load-system :xcvb-hello-via-bridge)~
                   #.(xcvb-hello:hello :name ~S :traditional t)#.~A)"
                         "asdf" object-dir "Sub-XCVB TeStEr"
                         ;; what about :output-path??? Something based on build-dir?
                         (xcvb::quit-form :implementation-type implementation-type))))))
    (is (search "hello, sub-xcvb tester" out)
        "Failed to build hello via the ASDF-XCVB bridge using ~(~A~)" implementation-type)))

(defun validate-unit-tests (&rest keys &key xcvb-dir &allow-other-keys)
  (apply 'run-make xcvb-dir "unit-tests" keys))

(defun do-xxx-build (target &rest keys &key xcvb-dir &allow-other-keys)
  (apply 'run-make xcvb-dir target keys))

(defun do-asdf-build (&rest keys)
  (apply 'do-xxx-build "xcvb-using-asdf" keys))

(defun do-self-mk-build (&rest keys)
  (apply 'do-xxx-build "xcvb-using-xcvb" keys))

(defun do-self-nemk-build (&rest keys)
  (apply 'do-xxx-build "xcvb-using-nemk" keys))

(defun do-bootstrapped-build (&rest keys &key release-dir &allow-other-keys)
  (apply 'run-make release-dir "install" keys))

(defun validate-xxx-build (fun &rest keys)
  (apply fun keys)
  (apply 'validate-xcvb keys))

(defun validate-asdf-build (&rest keys)
  (apply 'validate-xxx-build 'do-asdf-build keys))

(defun validate-mk-build (&rest keys)
  (apply 'validate-xxx-build 'do-self-mk-build keys))

(defun validate-nemk-build (&rest keys)
  (apply 'validate-xxx-build 'do-self-nemk-build keys))

(defun validate-bootstrapped-build (&rest keys)
  (apply 'validate-xxx-build 'do-bootstrapped-build keys))

(defun do-release-build (&rest keys &key release-dir build-dir &allow-other-keys)
  (rm-rfv (in-dir build-dir "obj/"))
  (apply 'run-make release-dir "install" keys))

(defun ensure-build-directories (&key object-dir install-bin install-image
                                 &allow-other-keys)
  (map () 'ensure-directories-exist
       (list object-dir install-bin install-image)))

(defun call-with-xcvb-build-dir (thunk &rest keys)
  (apply 'validate-xcvb-checkout keys)
  (apply 'clean-xcvb-dir keys)
  (unwind-protect
       (progn
         (apply 'ensure-build-directories keys)
         (apply thunk keys))
    (apply 'clean-xcvb-dir keys)))

(defun validate-regular-xcvb-builds (wrapper &rest keys)
  (apply wrapper 'validate-asdf-build keys)
  (apply wrapper 'validate-mk-build keys)
  ;; The following doesn't work due to missing translation of
  ;; :around-compile to asdf... will have to think about it...
  #|(apply wrapper 'validate-nemk-build keys)|#
  (values))

(defun validate-xcvb-dir (&rest keys)
  (compute-xcvb-dir-variables! keys)
  (apply 'validate-regular-xcvb-builds 'call-with-xcvb-build-dir keys))

(defun validate-xcvb-dir-all-lisps (&rest keys)
  (compute-xcvb-dir-variables! keys)
  (dolist (implementation-type +xcvb-lisps+)
    (apply 'validate-xcvb-dir :implementation-type implementation-type keys)))

(defun clean-xcvb-dir (&rest keys &key xcvb-dir &allow-other-keys)
  (apply 'run-make xcvb-dir "clean" keys)
  (rm-rfv (in-dir xcvb-dir "build/")))

(defun clean-release-dir (&rest keys &key xcvb-dir release-dir &allow-other-keys)
  (apply 'run-make xcvb-dir "clean" keys)
  (rm-rfv (in-dir release-dir "build/")))

(defun call-with-release-build-dir (thunk &rest keys)
  (compute-release-dir-variables! keys)
  (apply 'validate-release-checkout keys)
  (apply 'clean-release-dir keys)
  (unwind-protect
       (progn
         (apply 'ensure-build-directories keys)
         (apply thunk keys))
    (apply 'clean-release-dir keys)))

(defun validate-release-dir (&rest keys)
  (compute-release-dir-variables! keys)
  (apply 'call-with-release-build-dir 'validate-bootstrapped-build keys)
  (apply 'validate-regular-xcvb-builds 'call-with-release-build-dir keys))

(defun validate-release-dir-all-lisps (&rest keys)
  (compute-xcvb-dir-variables! keys)
  (dolist (implementation-type +xcvb-lisps+)
    (apply 'validate-release-dir :implementation-type implementation-type keys)))

(defmacro letk1 (keys var val &body body)
  `(let ((,var ,val))
     (setf (getf ,keys ,(conc-keyword var)) ,var)
     ,@body))
(defmacro letk* (keys bindings &body body)
  (if bindings
      `(letk1 ,keys ,@(first bindings) (letk* keys ,(rest bindings) ,@body))
      `(progn ,@body)))

(defun finalize-variables (&rest keys &key
                           verbosity
                           implementation-type object-dir build-dir
                           install-bin install-lisp install-image install-source install-systems
                           source-registry xcvb
                           (parallelize () parallelizep)
                           &allow-other-keys)
  (setf xcvb::*xcvb-verbosity* verbosity)
  (macrolet ((dir (x &optional (default `(error "~A not provided" ',x)))
               `(or (when ,x (ensure-directory-pathname ,x)) ,default)))
    (letk* keys
        ((implementation-type (or implementation-type *lisp-implementation-type*))
         (build-dir (dir build-dir))
         (object-dir (dir object-dir (in-dir build-dir "obj/")))
         (install-bin (dir install-bin (in-dir build-dir "bin/")))
         (install-lisp (dir install-lisp (in-dir build-dir "common-lisp/")))
         (install-image (dir install-image (in-dir build-dir "images/")))
         (install-source (dir install-source (in-dir build-dir "source/")))
         (install-systems (dir install-systems (in-dir build-dir "systems/")))
         (parallelize (if parallelizep parallelize
                          (let ((ncpus (parse-integer
                                        (run-program/read-output-string
                                         "cat /proc/cpuinfo|grep '^processor' | wc -l")
                                        :junk-allowed t)))
                            (format nil "-l~A" (1+ ncpus)))))
         (xcvb (or xcvb "xcvb"))
         (path (format nil "~A:~@[~A~]" install-bin (getenv "PATH")))
         (cl-launch-flags (format nil "--lisp ~(~A~) --source-registry ~S"
                                  implementation-type source-registry))
         (asdf-output-translations (format nil "/:~A" (in-dir build-dir "cache/"))))
      keys)))

(defun compute-release-dir-variables (&rest keys &key release-dir &allow-other-keys)
  (letk* keys
      ((release-dir (ensure-directory-pathname (or release-dir (getenv "RELEASE_DIR"))))
       (xcvb-dir (in-dir release-dir "xcvb/"))
       (build-dir (in-dir release-dir "build/"))
       (install-bin (in-dir build-dir "bin/"))
       (xcvb (in-dir install-bin "xcvb"))
       (object-dir (in-dir build-dir "obj/"))
       (install-xcvb (in-dir build-dir "install/source/xcvb/"))
       (install-lisp (in-dir build-dir "install/"))
       (install-source (in-dir build-dir "install/source/"))
       (install-systems (in-dir build-dir "install/systems/"))
       (xdg-cache-home (in-dir build-dir "cache/"))
       (prefix (in-dir build-dir "local/"))
       (source-registry
        (format nil "~A//:~A//:~A//"
                build-dir xcvb-dir (in-dir release-dir "dependencies/"))))
    (compute-makefile-configuration-variables! keys)
    (apply 'finalize-variables keys)))

(defun compute-xcvb-dir-variables (&rest keys &key xcvb-dir &allow-other-keys)
  (letk* keys
      ((xcvb-dir (ensure-directory-pathname
                  (or xcvb-dir
                      (and (asdf:find-system :xcvb)
                           (asdf:system-source-directory :xcvb)))))
       (build-dir (in-dir xcvb-dir "build/"))
       (source-registry
        (format nil "~A//:~A//:~@[~A~]"
                build-dir xcvb-dir (getenv "CL_SOURCE_REGISTRY"))))
    (apply 'finalize-variables keys)))
