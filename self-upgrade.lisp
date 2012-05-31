;;; Shell command-line interface for XCVB

#+xcvb
(module (:depends-on ("commands")))

(in-package :xcvb)

(declaim (optimize (speed 1) (safety 3) (compilation-speed 0) (debug 3)))

(defun reduce-xcvb-version (version)
  (subseq version 0
	  (position-if-not (lambda (x) (or (digit-char-p x) (eql x #\.))) version)))

(defun ensure-required-xcvb-version (required-xcvb-version)
  (when required-xcvb-version
    (let ((reduced-required-version (reduce-xcvb-version required-xcvb-version))
          (reduced-current-version (reduce-xcvb-version *xcvb-version*)))
      (unless (asdf:version-satisfies reduced-current-version reduced-required-version)
        (log-format 1 "This is XCVB ~A but version ~A was required"
                    *xcvb-version* required-xcvb-version)
        (flet ((abend (fmt &rest args)
                 (errexit 18 "Can't recompile XCVB ~A or newer: ~?."
                          required-xcvb-version fmt args)))
          (unless (get-xcvb-directory)
            (abend "cannot find source directory for XCVB"))
          (log-format 5 "Found XCVB source in ~A" (get-xcvb-directory))
          (let* ((source-version (get-xcvb-version))
                 (reduced-source-version (reduce-xcvb-version source-version)))
            (unless source-version
              (abend "no version found in XCVB source code."))
            (unless (asdf:version-satisfies reduced-source-version reduced-required-version)
              (abend "found insufficient source XCVB version ~A" source-version))
            (unless (build-xcvb *xcvb-program*)
              (abend "failed to build XCVB ~A" source-version))
            (exit
             (run-program/ (cons *xcvb-program* *arguments*)
                           :output nil :ignore-error-status t))))))))
