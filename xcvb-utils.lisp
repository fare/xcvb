;;;;; XCVB-Utils. Mix and match utilities from xcvb, asdf, alexandria, fare-utils

#+xcvb
(module
 (:build-depends-on ((:asdf "alexandria")
                     (:asdf "fare-utils")
                     (:asdf "asdf-utils")
                     "/xcvb/driver")))

(in-package :fare-utils)

(defmacro define-package-mix (package mixed-packages &rest clauses)
  (let ((h (make-hash-table :test 'equal)))
    (labels ((ensure-imported (n)
               (let* ((s (string n))
                      (x (gethash s h)))
                 (unless x (setf (gethash s h) t))
                 x))
             (import-from (package)
               (loop :for s :being :each :external-symbol :in package
                 :for n = (symbol-name s)
                 :unless (ensure-imported n)
                 :collect n)))
      ;; First, mark the symbols explicitly imported by the user
      (loop :for (kw . ()) :in clauses
        :when (member kw '(:import-from :shadowing-import-from)) :do
        (map () #'ensure-imported (cddr clauses)))
      `(defpackage ,package
         ,@(loop :for p :in mixed-packages
             :collect `(:import-from ,p ,@(import-from p)))
         ,@clauses
         (:export ,@(loop :for s :being :the :hash-keys :of h :collect s))))))

(define-package-mix :xcvb-utils
    (:alexandria :xcvb-driver :asdf-utils :fare-utils))
