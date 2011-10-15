;; Help with Releasing XCVB. Goal: no more shell scripting!

#+xcvb (module (:depends-on ("sub-xcvb")))

(in-package #:xcvb-test)

(declaim (optimize (speed 2) (safety 3) (compilation-speed 0) (debug 3)))

(defparameter *spaces*
  (coerce '(#\space #\return #\linefeed #\tab) 'base-string))

(defparameter *release-exclude*
  '(:exclude "build" :exclude "obj" :exclude "obj-ne"
    :exclude "*~" :exclude ".\#*" :exclude "xcvb*.mk"))

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
  (let ((release-xcvb (in-dir release-dir "xcvb/"))
        (release-deps (in-dir release-dir "dependencies/")))
    (ensure-directories-exist release-xcvb)
    (ensure-directories-exist release-deps)
    (rm-rfv (in-dir release-dir "build/"))
    (flet ((r (x y)
             (apply 'rsync `(:archive ,@*release-exclude* ,x ,y))))
      (r (asdf:system-relative-pathname :xcvb "") release-xcvb)
      (let ((dependencies (get-xcvb-dependencies (asdf:system-relative-pathname :xcvb ""))))
        (loop :for (nil d) :in dependencies
          :for dep = (basename d ".git")
          :for dir = (if (equal dep "libfixposix")
                         (asdf:system-relative-pathname "iolib" "../../libfixposix/")
                         (asdf:system-relative-pathname dep "")) :do
          (r dir (in-dir release-deps (strcat dep "/"))))))
    (run-cmd (or (getenv "MAKE") "make") "-C" release-dir
         "-f" "xcvb/doc/Makefile.release" "prepare-release")))
