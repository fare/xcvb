;;; Extracting properties from the target lisp implementation
#+xcvb (module (:depends-on ("macros")))

(in-package :xcvb)

#|
If/when ironclad provides tth, we should use that.
Until then, let's rely on the external utility tthsum.
|#
(defparameter +base32-characters+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

(defun tthsum-for-files (files)
  (when files
    (let* ((namestrings (mapcar #'namestring files))
           (lines (apply #'run-program/read-output-lines "tthsum" namestrings)))
      (unless lines
        (error "Couldn't extract TTH digest for given files. Is the tthsum utility installed?"))
      (unless (list-of-length-p (length files) lines)
        (error "tthsum output has wrong number of lines"))
      (loop :for file :in files
        :for namestring :in namestrings
        :for line :in lines
        :for len = (length line)
        :collect
        (progn
          (unless (and (= len (+ 41 (length namestring)))
                       (string= line "  "  :start1 39 :end1 41)
                       (string= line namestring :start1 41)
                       (loop :repeat 39 :for c :across line
                         :always (find c +base32-characters+)))
            (error "unexpected tthsum output line ~S for file ~S" line file))
          (subseq line 0 39))))))

(defun tthsum-for-files-or-nil (specs)
  (let* ((files (remove nil specs))
         (tthsums (tthsum-for-files files)))
    (loop :for spec :in specs
      :collect (when spec (pop tthsums)))))

(defun tthsum-for-file (file)
  (car (tthsum-for-files (list file))))

(defun manifest-form (grains)
  (flet ((extract-tthsum (property)
           (tthsum-for-files-or-nil
            (mapcar #'(lambda (x) (getf x property)) grains))))
    (loop
      :with tthsums = (extract-tthsum :pathname)
      :with source-tthsums = (extract-tthsum :source-pathname)
      :for grain :in grains
      :for tthsum :in tthsums
      :for source-tthsum :in source-tthsums
      :collect
      (destructuring-bind
            (&key fullname pathname source-pathname command) grain
        `(:fullname ,fullname
          ,@(when command
              `(:command ,command))
          ,@(when pathname
              `(:tthsum ,tthsum :pathname ,pathname))
          ,@(when source-pathname
              `(:source-tthsum ,source-tthsum :source-pathname ,source-pathname)))))))

(defun create-manifest (output-path grains)
  (with-user-output-file (o output-path)
    (with-safe-io-syntax ()
      (let ((*print-pretty* nil)
            (*print-case* :downcase))
        (format o "(~{~S~^~% ~})~%" (manifest-form grains)))))
  (values))

(defun has-tthsum-p ()
  (let ((s (ignore-errors
             (run-program/read-output-string
              "tthsum" #-windows "/dev/null" #+windows "NUL"))))
    (and (>= (length s) 41)
         (string= s "LWPNACQDBZRYXW3VHJVCJ64QBZNGHOHHHZWCLNQ" :end1 39))))

(defun ensure-tthsum-present ()
  (unless (has-tthsum-p)
    (errexit 2 "~&XCVB's master mode (enabled by default) requires the tthsum utility.
If you are using Debian or Ubuntu, you can install it with:
	sudo apt-get install tthsum
If you are unable to install this utility, you may disable XCVB's master mode
by passing option --no-master to xcvb make-makefile.
The XCVB master mode is what allows you to load into a running image
new or updated FASLs that you build with XCVB.~%")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make a load manifest ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +make-manifest-option-spec+
  '((("output" #\o) :type string :optional t :initial-value "-"
     :documentation "Path to manifest file or - for stdout")
    (("grains" #\g) :type string :optional nil
     :documentation "alist of grains, mapping fullname to pathname")))

(defun make-manifest (arguments &key output grains)
  (when arguments
    (error "Invalid arguments to make-manifest: ~S~%" arguments))
  (create-manifest output (with-safe-io-syntax () (read-from-string grains))))
