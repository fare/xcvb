#+xcvb (module (:depends-on ("utilities" "conditions")))

(in-package :xcvb)


;;; A few generic functions
(defgeneric fullname (grain))
(defgeneric specified-fullname (grain))


;;; Define grains.

;; Unit of build: a file, a process, etc.
(defclass grain (simple-print-object-mixin)
  ((fullname
    :accessor fullname)
   (parent
    :accessor grain-parent
    :documentation "parent build.xcvb file for this grain")
   (relative-name
    :accessor grain-relative-name
    :documentation "name relative to the parent"))
  (:documentation "Unit of abstract state intent"))

(defclass buildable-grain (grain)
  ((name-digest
    :accessor grain-name-digest
    :documentation "digest of the full name, including all static dependencies")
   (build-digest
    :accessor grain-build-digest
    :documentation "digest of the build command with actual dependencies")
   (content-digest
    :accessor grain-content-digest
    :documentation "digest of the contents of the grain.")
   (computation
    :initarg :computation
    :accessor grain-computation)
   (computation-index ;; index of the grain amongst the outputs of the computation
    :initarg :computation-index
    :accessor grain-computation-index))
  (:documentation "Mixin for a grain you can build (for V2)"))

(defclass persistent-grain (grain)
  ()
  (:documentation "Files and other persistent data grain"))

(defclass transient-grain (grain)
  ()
  (:documentation "Active computational grain"))

(defclass named-grain (grain)
  ((fullname
    :initarg :fullname
    :reader fullname)))

(defclass phony-grain (buildable-grain named-grain)
  ((fullname
    :initarg :fullname))
  (:documentation "virtual grain used for side-effects"))

(defclass file-grain (persistent-grain buildable-grain)
  ((pathname
    :initarg :pathname
    :accessor grain-pathname
    :documentation "The truepath to the file that the module was declared in"))
  (:documentation "File grain"))

(defclass documented-grain (grain)
  ((author
    :initarg :author
    ;;:reader author
    :documentation "The author of the grain")
   (maintainer
    :initarg :maintainer
    ;;:reader maintainer
    :documentation "The maintainer(s) of the file")
   (version
    :initarg :version
    ;;:reader version
    :documentation "The version number of the file")
   (licence
    :initarg :licence
    ;;:reader licence
    :documentation "The licence being used for the file")
   (description
    :initarg :description
    ;;:reader description
    :documentation "A short description of the file")
   (long-description
    :initarg :long-description
    ;;:reader long-description
    :documentation "A detailed description of the file"))
  (:documentation "Documented grain"))
  
(defclass source-grain (file-grain)
  ((name
    :initarg :name
    :accessor source-grain-name)
   (in
    :initarg :in
    :accessor source-grain-in)
   (fullname :initarg :fullname)
   )
  (:documentation "Data file (source) grain."))

(defclass documented-file-grain (file-grain documented-grain)
  ()
  (:documentation "documented file grain"))

(defclass lisp-grain (documented-file-grain)
  ((compile-depends-on
    :initform nil
    :initarg :compile-depends-on)
   (depends-on
    :initform nil
    :initarg :depends-on)
   (cload-depends-on
    :initarg :cload-depends-on)
   (load-depends-on
    :initform nil
    :initarg :load-depends-on)
   (compile-dependencies
    :reader compile-dependencies)
   (cload-dependencies
    :reader cload-dependencies)
   (load-dependencies
    :reader load-dependencies)
   (extension-forms
    :initarg :extension-forms
    :accessor grain-extension-forms
    :documentation "extension forms to the build specification"))
  (:documentation "Lisp file grain"))

(defclass build-grain (lisp-grain build-registry-entry)
  ((specified-fullname
    :initarg :fullname
    :initform nil
    :reader specified-fullname)
   (nicknames
    :initarg :nicknames
    :reader nicknames
    :initform nil
    :documentation "A short name to be used to refer to the module")
   (supersedes-asdf
    :initarg :supersedes-asdf
    :reader supersedes-asdf
    :initform nil
    :documentation "A list of ASDF systems superseded by this module")
   (build-depends-on
    :initarg :build-depends-on
    :initform nil
    :documentation "A list of dependencies that apply to all files in the
system specified by this build.xcvb file.
These dependencies will be loaded first thing
into an image that will be used for all future compile/load operations")
   (build-dependencies
    :reader build-dependencies
    :documentation "A normalized version of the above")
   (build-pre-image
    :initarg :pre-image
    :accessor build-pre-image
    :initform nil
    :type boolean
    :documentation "Should we build a specific pre-image for this build?")
   (build-image
    :initarg :build-image
    :accessor build-image
    :initform nil
    :type boolean
    :documentation "Should we build a Lisp image with that build loaded?"))
  (:documentation "build.xcvb file grain"))

(defclass fasl-grain (file-grain named-grain)
  ((load-dependencies
    :initarg :load-dependencies
    :reader load-dependencies))
  (:documentation "Lisp FASL file grain"))

(defclass cfasl-grain (file-grain named-grain)
  ((load-dependencies
    :initarg :load-dependencies
    :reader load-dependencies))
  (:documentation "Lisp CFASL file grain"))

(defclass image-grain (file-grain named-grain)
  ((included
    :initarg :included
    :reader image-included))
  (:documentation "Dumped Image"))

(defclass asdf-grain (named-grain)
  ((name
    :initarg :name
    :reader asdf-grain-system-name)
   (implementation
    :initarg :implementation
    :reader asdf-grain-implementation))
  (:documentation "Dumped Image"))

(defun coerce-asdf-system-name (name)
  "This function take the name of an asdf-system, and
converts it to a string representation that can universally be used to refer to that system.
Modeled after the asdf function coerce-name"
  (string-downcase
   (typecase name
     #+asdf (asdf:component (asdf:component-name name))
     (symbol (symbol-name name))
     (string name)
     (asdf-grain (asdf-grain-system-name name))
     (t (simply-error 'syntax-error "~@<invalid asdf system designator ~A~@:>" name)))))

(defun lisp-grain-p (x)
  (typep x 'lisp-grain))

(defun build-grain-p (x)
  (typep x 'build-grain))

(defun asdf-grain-p (x)
  (typep x 'asdf-grain))

(defun image-grain-p (x)
  (typep x 'image-grain))

