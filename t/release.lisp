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
             (asdf:split-string string :separator *spaces*)))

(defun easy-string-p (x)
  (and (loop :for c :across x :always (or (easy-sh-character-p c) (eql c #\~)))
       (not (eql #\~ (first-char x)))))

(defun parse-easy-sh-commands (commands)
  "string is a ;-separated list of easy commands.
commands are lists of space-separated strings
of easy shell characters (that do not require quoting)."
  (loop :for command :in (asdf:split-string commands :separator ";")
    :for strings = (space-separated-components command)
    :when (every 'easy-string-p strings)
      :collect strings
    :else
      :do (errexit 80 "Bad characters in easy sh command: ~S" strings)))

(defun get-xcvb-dependencies (xcvb-dir)
  (remove-if
   (lambda (x) (equal ":" (first x)))
   (parse-easy-sh-commands
    (run-cmd/string 'make #\s #\C xcvb-dir
                    #\f "doc/Makefile.release" "show-dependencies"))))

(defun basename (x &optional ext)
  (let* ((x1 (string-right-trim "/" x))
         (x2 (car (last (asdf:split-string x1 :max 2 :separator "/")))))
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
          :for dir = (if (equal dep "libfixposix")
                         (asdf:system-relative-pathname "iolib" "../../libfixposix/")
                         (asdf:system-source-directory dep)) :do
          (r dir (subpathname release-deps dep :type :directory)))))
    (run-cmd (or (getenv "MAKE") "make") "-C" release-dir
         "-f" "xcvb/doc/Makefile.release" "prepare-release")))

(defun make-release-tarball (&rest keys)
  (compute-release-dir-variables! keys)
  (apply '%make-release-tarball keys))

(defun %make-release-tarball (&key release-dir xcvb-dir &allow-other-keys)
  (chdir xcvb-dir)
  (let* ((version (string-trim *spaces* (run-cmd/string 'git 'describe :tags)))
         (version-dir (strcat "xcvb-" version))
         (tarball (strcat version-dir ".tar.bz2")))
    (chdir "../..")
    (run-cmd 'rm '-f (strcat "xcvb-" version))
    (run-cmd 'ln '-sf release-dir version-dir)
    (apply 'run-cmd `(tar ,@*release-exclude* -hjcf ,tarball ,version-dir))
    (run-cmd 'ln '-sf tarball "xcvb.tar.bz2")))
