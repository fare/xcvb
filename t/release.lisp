;; Help with Releasing XCVB. Goal: no more shell scripting!

#+xcvb (module (:depends-on ("sub-xcvb")))

(in-package #:xcvb-test)

(declaim (optimize (speed 2) (safety 3) (compilation-speed 0) (debug 3)))

(defparameter *spaces*
  (coerce '(#\space #\return #\linefeed #\tab) 'base-string))

(defparameter *release-exclude*
  '(:exclude "build" :exclude "obj" :exclude "obj-ne"
    :exclude "*.bak" :exclude "*~" :exclude ".\#*"
    :exclude "xcvb*.mk" :exclude "configure*.mk"))

(defun space-separated-components (string)
  (remove-if 'emptyp
             (split-string string :separator *spaces*)))

(defun easy-string-p (x)
  (and (loop :for c :across x :always (or (easy-sh-character-p c) (eql c #\~)))
       (not (eql #\~ (first-char x)))))

(defun parse-easy-sh-commands (commands)
  "string is a ;-separated list of easy commands.
commands are lists of space-separated strings
of easy shell characters (that do not require quoting)."
  (loop :for command :in (split-string commands :separator ";")
    :for strings = (space-separated-components command)
    :when (every 'easy-string-p strings)
      :collect strings
    :else
      :do (errexit 80 "Bad characters in easy sh command: ~S" strings)))

(defun get-xcvb-dependencies (xcvb-dir)
  (remove-if
   (lambda (x) (equal ":" (first x)))
   (parse-easy-sh-commands
    (run/s `(make #\s #\C ,xcvb-dir #\f "doc/Makefile.release" "show-dependencies")))))

(defun basename (x &optional ext)
  (let* ((x1 (string-right-trim "/" x))
         (x2 (car (last (split-string x1 :max 2 :separator "/")))))
    (if (and ext (string-suffix-p x2 ext))
        (subseq x2 0 (- (length x2) (length ext)))
        x2)))

(defun make-fake-release-directory (&rest keys)
  (compute-xcvb-dir-variables! keys)
  (apply '%make-fake-release-directory keys))

(defun %make-fake-release-directory (&key release-dir
                                     &allow-other-keys)
  (when (emptyp release-dir)
    (errexit 42 "RELEASE_DIR not defined"))
  (let ((release-xcvb (subpathname release-dir "xcvb/"))
        (release-deps (subpathname release-dir "dependencies/")))
    (ensure-directories-exist release-xcvb)
    (ensure-directories-exist release-deps)
    (rm-rfv (subpathname release-dir "build/"))
    (flet ((r (x y)
             (apply 'rsync `(:archive ,@*release-exclude* ,x ,y))))
      (r (asdf:system-relative-pathname :xcvb "") release-xcvb)
      (let ((dependencies (get-xcvb-dependencies (asdf:system-source-directory :xcvb))))
        (loop :for (nil d) :in dependencies
          :for dep = (basename d ".git")
          :for dir = (let ((*read-eval* t)) ;; allow #. in iolib.asd
		       (cond
			 ((equal dep "libfixposix")
			  (asdf:system-relative-pathname "iolib" "../../libfixposix/"))
			 ((equal dep "iolib")
			  (asdf:system-relative-pathname "iolib" "../"))
			 (t
			  (asdf:system-source-directory dep)))) :do
          (r dir (subpathname release-deps dep :type :directory)))))
    (run `(,(or (getenv "MAKE") "make") "-C" ,release-dir
	   "-f" "xcvb/doc/Makefile.release" "prepare-release"))))

(defun make-release-tarballs (&rest keys)
  (compute-release-dir-variables! keys)
  (apply '%make-release-tarballs keys))

(defun make-small-release-tarballs (&rest keys)
  (compute-release-dir-variables! keys)
  (apply '%make-small-release-tarball keys))

(defun make-big-release-tarballs (&rest keys)
  (compute-release-dir-variables! keys)
  (apply '%make-big-release-tarball keys))

(defun %make-release-tarballs (&rest keys)
  (apply '%make-small-release-tarball keys)
  (apply '%make-big-release-tarball keys))

(defun %make-big-release-tarball (&key release-dir xcvb-dir &allow-other-keys)
  (nest
    (with-standard-io-syntax)
    (with-current-directory (xcvb-dir))
    (let (asdf::*source-registry* asdf::*source-registry-parameter*)
      (asdf:initialize-source-registry
       `(:source-registry (:tree ,release-dir) :ignore-inherited-configuration))
      (asdf:clear-system :xcvb)
      (xcvb::log-format-pp 5 "Making release tarballs for XCVB at: ~A"
			   (asdf:system-relative-pathname :xcvb "../")))
    (let* ((version (xcvb-driver::get-xcvb-version-from-git))
	   (version-dir (strcat "xcvb-" version))
	   (bigsuffix "-and-dependencies.tar.bz2")
	   (bigtarball (strcat version-dir bigsuffix))))
    (with-current-directory ("../../")
      (run `(rm -f ,version-dir))
      (run `(ln -sf ,release-dir ,version-dir))
      (run `(tar ,@*release-exclude* -hjcf ,bigtarball ,version-dir))
      (run `(ln -sf ,bigtarball ("xcvb" ,bigsuffix))))))

(defun %make-small-release-tarball (&key xcvb-dir &allow-other-keys)
  (nest
    (with-standard-io-syntax)
    (with-current-directory (xcvb-dir))
    (let (asdf::*source-registry* asdf::*source-registry-parameter*)
      (asdf:initialize-source-registry
       `(:source-registry (:tree ,xcvb-dir) :ignore-inherited-configuration))
      (asdf:clear-system :xcvb)
      (xcvb::log-format-pp 5 "Making small release tarball for XCVB at: ~A"
			   (asdf:system-source-directory :xcvb)))
    (let* ((version (xcvb-driver::get-xcvb-version-from-git))
	   (version-dir (strcat "xcvb-" version))
	   (suffix ".tar.bz2")
	   (tarball (strcat version-dir suffix))))
    (with-current-directory ("../../")
      (run `(rm -f ,version-dir))
      (run `(ln -sf ,xcvb-dir ,version-dir))
      (xcvb-driver::make-xcvb-version-file)
      (run `(tar ,@*release-exclude* --exclude ".git" -hjcf ,tarball ,version-dir))
      (run `(rm -f ,version-dir ,(xcvb-driver::xcvb-version-file)))
      (run `(ln -sf ,tarball ("xcvb" ,suffix))))))
