#+xcvb (module (:depends-on ("macros" "virtual-pathnames" "commands")))

(in-package :xcvb)

(defun manifest-form (specs)
  (flet ((extract-tthsum (property)
           (tthsum-for-files-or-nil
            (mapcar #'(lambda (x) (getf x property)) specs))))
    (loop
      :with tthsums = (extract-tthsum :pathname)
      :with source-tthsums = (extract-tthsum :source-pathname)
      :for spec :in specs
      :for tthsum :in tthsums
      :for source-tthsum :in source-tthsums
      :collect
      (destructuring-bind (&key command pathname source-pathname) spec
        `(:command ,command ;; TODO :build-command ,spec :driver-command ,command
          ,@(when pathname `(:pathname ,(namestring (truename pathname)) :tthsum ,tthsum))
          ,@(when source-pathname
              `(:source-pathname ,(namestring (truename source-pathname)) :source-tthsum ,source-tthsum)))))))

(defun create-manifest (output-path grains)
  (with-user-output-file (o output-path)
    (with-safe-io-syntax ()
      (let ((*print-pretty* nil)
            (*print-case* :downcase))
        (format o "(~{~S~^~% ~})~%" (manifest-form grains)))))
  (values))

(defun command-to-manifest-spec (env command)
  (let* ((fullname (unwrap-load-file-command command))
         (source-fullname (fullname-source fullname)))
    `(:command ,command ;; TODO: :build-command ,command ....
      ,@(when fullname `(:pathname ,(fullname-namestring env fullname)))
      ,@(when source-fullname `(:source-pathname ,(fullname-namestring env source-fullname))))))

(defun commands-to-manifest-spec (env commands)
  (mapcar/ #'command-to-manifest-spec env commands))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make a load manifest ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command make-manifest
    (("make-manifest")
     ()
     '((("output" #\o) :type string :optional t :initial-value "-"
        :documentation "Path to manifest file or - for stdout")
       (("spec" #\s) :type string :optional nil
        :documentation "list of plists specifying command and optional pathname, source-pathname"))
     "Create a manifest of files to load (for internal use)"
     "given fullnames and paths, output fullnames, tthsum and paths")
  (create-manifest output (with-safe-io-syntax () (read-from-string spec))))
