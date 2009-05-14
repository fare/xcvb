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
    :documentation "parent BUILD.lisp file for this grain")
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

(defclass phony-grain (buildable-grain)
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
  
(defclass documented-file-grain (file-grain documented-grain)
  ()
  (:documentation "documented file grain"))

(defclass lisp-grain (documented-file-grain)
  ((compile-depends-on
    :initarg :compile-depends-on
    :accessor compile-depends-on)
   (depends-on
    :initarg :depends-on
    :accessor depends-on)
   (load-depends-on
    :initarg :load-depends-on
    :accessor load-depends-on)
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
   (build-requires
    :initarg :build-requires
    :accessor build-requires
    :initform nil
    :documentation "A list of dependencies that apply to all files in the
system specified by this BUILD.lisp file.
These dependencies will be loaded first thing
into an image that will be used for all future compile/load operations")
   (build-pre-image
    :initarg :build-pre-image
    :accessor build-pre-image
    :initform t
    :documentation "Relative portablish pathname of an image to build for requirements")
   (build-image
    :initarg :build-image
    :accessor build-image
    :initform t
    :documentation "Relative portablish pathname of the executable to produce, if any"))
  (:documentation "BUILD.lisp file grain"))

(defclass fasl-grain (file-grain)
  ()
  (:documentation "Lisp FASL file grain"))

(defclass cfasl-grain (file-grain)
  ()
  (:documentation "Lisp CFASL file grain"))

(defclass image-grain (file-grain)
  ()
  (:documentation "Dumped Image"))


;------>8------>8------>8------>8------>8------>8------>8------>8------>8------

(defgeneric direct-dependencies (grain)
  (:documentation "List of all the direct grain dependencies"))

(defgeneric all-dependencies (grain)
  (:documentation "List of all the transitive grain dependencies"))

(defmethod all-dependencies (grain)
  (all-descendents-f grain #'direct-dependencies))


#|
(def grain lisp-grain (file-grain)
  :filename "%.lisp"
  :documentation "LISP file grain")

(def grain fasl-grain (file-grain)
  :filename "%.fasl"
  :documentation "Lisp FASL file grain")

(def grain cfasl-grain (file-grain)
  :filename "%.cfasl"
  :documentation "Lisp CFASL file grain")


(def rule lisp-compile/cleanly
  :input ((lisp lisp-grain) &key
          (implementation lisp-implementation))
  :output ((fasl fasl-grain) &key
           (cfasl cfasl-grain))
  :binding ((dependencies (compile-dependencies lisp)))
  :dependencies (compile-dependencies lisp)
  :execute (in-process
            (lisp-process
             :implementation implementation
             :loaded dependencies)
            (compile-module (filename lisp) :fasl (filename fasl) :cfasl (filename cfasl))))

|#
