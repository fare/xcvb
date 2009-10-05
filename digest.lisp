;;; Extracting properties from the target lisp implementation
#+xcvb (module (:depends-on ("macros")))

(in-package :xcvb)

#|
If/when ironclad provides tth, we should use that.
Until then, let's rely on tthsum.
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

(defun tthsum-for-file (file)
  (car (tthsum-for-files (list file))))

(defun manifest-form (grains)
  (let* ((fullnames (mapcar #'car grains))
         (pathnames (mapcar #'cdr grains))
         (tthsums (tthsum-for-files pathnames)))
    (loop
      :for fullname :in fullnames
      :for tthsum :in tthsums
      :for pathname :in pathnames
      :collect (list fullname tthsum pathname))))

(defun create-manifest (output-path grains)
  (with-user-output-file (o output-path)
    (with-safe-io-syntax ()
      (let ((*print-pretty* nil)
            (*print-case* :downcase))
        (format o "(~{~S~^~% ~})~%" (manifest-form grains)))))
  (values))
