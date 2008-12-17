;;; Shell command-line interface for XCVB

(in-package :xcvb)


(defun asdf-to-xcvb ()
#|
  (setf *systems-to-preload* ...) ;; get it from some argument
  (setf *systems-to-convert* ...) ;; get it from some argument
  ... have some mechanism to specify overrides...
  ... use ADG to compute dependencies into migrated-system-components.lisp-expr...
  ... Splice migrated-system-components.lisp-expr into a new migrated-system.asd...
  ... (xcvb::convert-asdf-system-to-xcvb :quux) ...
  ... make clean ...
  ...
|#
  nil)


(defun main ()
#|
  ... Create an image with the setup loaded ... (see load-setup-and-die)
  ... (setf xcvb:*lisp-executable-pathname* ...) ... the path to the image you dumped in
  ... (xcvb::write-makefile "/ita/devel/qres/lisp/quux/BUILD.lisp" "/ita/devel/qres/lisp/quux/") ...
  ... make -f /ita/devel/qres/lisp/quux/Makefile.xcvb lisp-image.core ...
|#
  nil)
