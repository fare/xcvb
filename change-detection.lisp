#+xcvb (module (:depends-on ("traversal")))

(in-package :xcvb)

(eval-when (:compile-toplevel :execute)
  (named-readtables:in-readtable :fare-quasiquote))

(defgeneric already-computed-p (env computation)
  (:documentation "was the computation already done?"))
(defgeneric grain-change-information (env grain &key error)
  (:documentation "change information for the grain"))
(defgeneric update-change-information (env grain &key)
  (:documentation "update the change information for the grain"))

(defparameter *newest-time* most-positive-single-float) ; behold the Y1e31 bug!
(defparameter *oldest-time* most-negative-single-float)
(defun time-or-oldest (time)
  (or time *oldest-time*))
(defun time-or-newest (time)
  (or time *newest-time*))
(defun oldest-time* (times)
  (time-or-newest (loop :for time :in times :minimize (time-or-oldest time))))
(defun newest-time* (times)
  (time-or-oldest (loop :for time :in times :maximize (time-or-newest time))))
(defun oldest-time (&rest times)
  (oldest-time* times))
(defun newest-time (&rest times)
  (newest-time* times))

(defun safe-file-write-date (p &optional error)
  (or (and p (asdf::probe-file* p) (ignore-errors (file-write-date p)))
      (error-behaviour error)))

;; We rely on the same approximation as make and asdf.
;; If the modified file is a generated file a previous version of which
;; was last generated and compiled in the same second, you lose. Unlikely, though.
;; More likely, if you have object files from the recent past and
;; unpack a source code update from a further past (as archived), you lose.
;; Or, if your (file)system clock is skewed and produces object files in the past
;; of the source code, you may lose in strange ways by rebuilding too much.

(defun newest-timestamp (env grains &key (error t))
  (time-or-oldest
   (loop :for g :in grains :maximize
     (or (grain-change-information env g)
         (if error
             (error "~@<Grain not yet built: ~S~:>" g)
             (return *newest-time*))))))

(defun oldest-timestamp (env grains)
  (time-or-newest
   (loop :for g :in grains :minimize
     (or (grain-change-information env g)
         (return *oldest-time*)))))

(defclass timestamp-based-change-detection (traversal) ())

(defmethod already-computed-p ((env timestamp-based-change-detection) computation)
  "Use timestamps to identify whether the grain has changed since last built"
  (let ((inputs (computation-inputs computation))
        (outputs (computation-outputs computation)))
    (<= (newest-timestamp env inputs)
        (oldest-timestamp env outputs))))

(defmethod grain-change-information ((env timestamp-based-change-detection) grain &key error)
  (declare (ignorable env))
  (or (grain-build-timestamp grain)
      (error-behaviour error)))

(defmethod grain-change-information ((env timestamp-based-change-detection)
                                     (grain require-grain) &key error)
  (declare (ignorable env grain error))
  *oldest-time*)

(defmethod grain-change-information ((env timestamp-based-change-detection)
                                     (grain asdf-grain) &key error)
  (declare (ignorable env grain error))
  *oldest-time*)

(defmethod grain-change-information ((env timestamp-based-change-detection) (grain file-grain)
                                     &key error)
  (or (grain-build-timestamp grain)
      (update-change-information env grain)
      (error-behaviour error)))

(defmethod update-change-information ((env timestamp-based-change-detection) grain &key)
  (declare (ignorable env))
  (setf (grain-build-timestamp grain)
        (newest-time* (mapcar/ #'grain-change-information env
                               (when-bind (computation) (grain-computation grain)
                                 (computation-inputs computation))))))

(defmethod update-change-information ((env timestamp-based-change-detection) (grain file-grain)
                                      &key)
  (let ((write-date (safe-file-write-date (grain-namestring env grain))))
    (setf (grain-build-timestamp grain) write-date)
    write-date))


(defclass digest-based-change-detection (traversal) ())

#|
(defmethod already-computed-p ((env digest-based-change-detection) computation)
  "Use cache of previous checksums to determine whether the grain has changed since last built"
  (let* ((inputs (computation-inputs computation))
         (outputs (computation-outputs computation))
         (command (computation-command computation))
         (digest-name `(:computation
                        :command ,command :inputs ,inputs
                        :digests (mapcar 'grain-digest inputs)))
         (cached-results (lookup-metadata-cache (digest digest-name))))
    (ifmatch
     cached-results `(:results :outputs ,(values outputs) :digests ,result-digests)
     (when (and (length=p outputs result-digests)
                (every 'content-cache-present-p result-digests))
       ;; Side-effect: extract the data from the file cache into its destination
       (loop :for o :in outputs :for h :in result-digests :do
         (extract-from-content-cache (grain-namestring env o) h))
       t))))

(defmethod grain-change-information ((env digest-based-change-detection) grain &key error)
  (or (grain-digest grain)
      (error-behaviour error)))

(defmethod grain-change-information ((env digest-based-change-detection) (grain file-grain)
                                     &key error)
  (or (grain-digest grain)
      (update-change-information env grain)
      (error-behaviour error)))

(defmethod update-change-information ((env digest-based-change-detection) grain &key)
  (declare (ignorable env))
  (setf (grain-digest grain) (digest (digest-name grain))))

(defmethod update-change-information ((env digest-based-change-detection) (grain file-grain) &key)
  (setf (grain-build-timestamp grain) (file-digest (grain-namestring env grain))))
|#

(eval-when (:compile-toplevel :execute)
  (named-readtables:in-readtable :standard))
