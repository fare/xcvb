#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :xcvb)

#|
If/when ironclad provides tth, we should use that.
Until then, let's rely on the external utility tthsum.
|#
(defparameter +base32-characters+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

(defun tthsum-for-files (files)
  (loop :for file :in files
    :for p = (probe-file file)
    :collect (when p
               (output-tthsum (tthsum-file p)))))

(defun ensure-tthsum-present ()
  (values))


#| ;; Implementation from before we were using ironclad
(defun tthsum-for-files (files)
  (when files
    (let* ((truefiles
            (loop :for file :in files :collect
              (or (probe-file file) (error "File ~A does not exist" file))))
           (namestrings (mapcar #'namestring truefiles))
           (lines (run-program/ (cons "tthsum" namestrings) :output :lines)))
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

(defun has-tthsum-p ()
  (let ((s (ignore-errors
             (run-program/
              '("tthsum" #-windows "/dev/null" #+windows "NUL")
              :output :string))))
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
|#


(defun tthsum-for-files-or-nil (specs)
  (let* ((files (remove nil specs))
         (tthsums (tthsum-for-files files)))
    (loop :for spec :in specs
      :collect (when spec (pop tthsums)))))

(defun tthsum-for-file (file)
  (car (tthsum-for-files (list file))))

(defun output-tthsum (sum &optional s)
  (with-output (s)
    (princ (tthsum-to-string sum) s)))

(defun tthsum-to-string (sum)
  ;; beware: a base-string, not a simple-string, so not simply readable
  (subseq (binascii:encode sum :base32) 0 39))

(defun tthsum-from-string (string)
  (binascii:decode string :base32))

(defun tthsum-string (string)
  (tthsum-sequence (babel:string-to-octets string :encoding :utf-8)))

(defun tthsum-sequence (sequence)
  (ironclad:digest-sequence :tree-hash sequence))

(defun tthsum-file (file)
  (ironclad:digest-file :tree-hash file))

(defun tthsum-stream (stream)
  (ironclad:digest-stream :tree-hash stream))

(defun tthsum-object (object)
  (tthsum-string
   (with-safe-io-syntax ()
     (write-to-string object
                      :readably nil :pretty nil))))

(defgeneric digest (x))

(defmethod digest ((x string))
  (tthsum-string x))

(defmethod digest ((x vector))
  (unless (typep x '(simple-array (unsigned-byte 8) (*)))
    (error "Can only digest ub8 vectors and strings"))
  (tthsum-sequence x))

(defmethod digest ((x pathname))
  (tthsum-file x))

(defmethod digest ((x cons))
  (tthsum-object x))
