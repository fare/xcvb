#+xcvb (module (:depends-on ("utilities" "conditions")))

(in-package :xcvb)

;;; A few generic functions
(defgeneric fullname (grain))
(defgeneric specified-fullname (grain))

(defgeneric image-setup (env))
(defgeneric build-commands-r (env))
(defgeneric included-dependencies (env))
(defgeneric (setf included-dependencies) (value env))
(defgeneric finalize-grain (grain))
(defgeneric object-namestring (env fullname &optional merge))

(defgeneric run-generator (env generator))

(defgeneric brc-pathnames (brc))

(defgeneric build-module-grain-for (grain)
  (:documentation "in which build module is this grain being defined"))

(defgeneric effective-around-compile (grain)
  (:documentation "what hook if any to call around compilation in the target"))

(defgeneric effective-encoding (grain)
  (:documentation "what encoding to use for given grain"))

;;; Define grains.

;; Unit of build: a file, a process, etc.
(defclass grain (simple-print-object-mixin)
  ((finalizedp :accessor grain-finalized-p :initform nil :initarg :finalizedp)
   (ordinal :type fixnum :accessor grain-ordinal))
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
   (build-timestamp
    :accessor grain-build-timestamp
    :initform nil
    :documentation "timestamp at which the grain was built.")
   (generator
    :initarg :generator
    :initform nil
    :accessor grain-generator)
   (computation
    :initarg :computation
    :accessor grain-computation)
   (computation-index ;; index of the grain amongst the outputs of the computation
    :initarg :computation-index
    :accessor grain-computation-index)
   (users
    :initform nil
    :accessor grain-users))
  (:documentation "Mixin for a grain you can build (for V2)"))

(defclass persistent-grain (buildable-grain)
  ()
  (:documentation "Files and other persistent data grain"))

(defclass transient-grain (buildable-grain)
  ()
  (:documentation "Non persistent data grain"))

(defclass named-grain (grain)
  ((fullname
    ;; no :initarg, so that :fullname can mean specified-fullname in builds...
    :accessor fullname)))

(defclass explicitly-named-grain (named-grain)
  ((fullname :initarg :fullname)))

(defclass loaded-grain ()
  ((issued-build-commands
    :documentation "hashset of load commands issued in this world"
    :reader issued-build-commands :initarg :issued-build-commands)
   (included-dependencies
    :documentation "hashset of grains included in this world"
    :reader included-dependencies :initarg :included-dependencies))
  (:documentation "grain in which other grains are loaded"))

(defclass world-grain (transient-grain explicitly-named-grain loaded-grain)
  ;; The fullname is a plist of the following form,
  ;; with keywords always present and in that order:
  ;; `(:world :setup ,setup :commands-r ,commands-r)
  ((hash
    :documentation "precomputed hash of the fullname"
    :reader world-hash :initarg :hash))
  ;; used by the farmer
  (:documentation "state of a running Lisp process in which grains are loaded"))

(defclass phony-grain (transient-grain explicitly-named-grain)
  ()
  (:documentation "virtual grain used for side-effects"))

(defclass file-grain (persistent-grain named-grain)
  ((vp
    :initarg :vp
    :accessor grain-vp
    :documentation "The truepath to the file that the module was declared in")
   (pathname
    :initarg :pathname
    :accessor grain-pathname
    :documentation "The truepath to the file that the module was declared in"))
  (:documentation "File grain"))

(defclass explicitly-named-file-grain (file-grain explicitly-named-grain)
  ()
  (:documentation "File grains with an explicit name"))

(defclass loadable-grain ()
  ((load-dependencies :reader load-dependencies)))

(defclass loadable-file-grain (explicitly-named-grain loadable-grain file-grain)
  ((load-dependencies :initarg :load-dependencies)))

(defclass image-grain (explicitly-named-file-grain)
  ((world :accessor image-world :initarg :world))
  (:documentation "Dumped Image"))

(defclass library-grain (loadable-file-grain loaded-grain)
  ()
  (:documentation "any kind of library for a build under ECL"))

(defclass dynamic-library-grain (library-grain)
  ()
  (:documentation "dynamic library for a build under ECL"))

(defclass static-library-grain (library-grain)
  ()
  (:documentation "static library for a build under ECL"))

(defclass executable-grain (image-grain)
  ((parent
    :initarg :parent
    :accessor grain-parent
    :type build-module-grain))
  (:documentation "Executable Image, or script around image???"))

(defclass executable-generator ()
  ((build :initarg :build :reader generator-build)
   (target :initarg :target :reader generator-target)
   (pre-image-dump
    :initarg :pre-image-dump
    :reader pre-image-dump
    :documentation "string to read and evaluate before dumping an image")
   (post-image-restart
    :initarg :post-image-restart
    :reader post-image-restart
    :documentation "string to read and evaluate after image is started, before the main function")
   (entry-point
    :initarg :entry-point
    :reader entry-point
    :documentation "string specifying the nullary main function for the application")
   (depends-on :initarg :depends-on :reader generator-depends-on)))

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

(defclass source-grain (explicitly-named-file-grain)
  ((name
    :initarg :name
    :accessor source-grain-name)
   (in
    :initarg :in
    :accessor source-grain-in))
  (:documentation "Data file (source) grain."))

(defclass documented-file-grain (file-grain documented-grain)
  ()
  (:documentation "documented file grain"))

(defclass lisp-module-grain (documented-file-grain loadable-grain)
  ((parent
    :accessor grain-parent
    :type (or null build-registry-entry)
    :documentation "parent build.xcvb file for this grain")
   (compile-depends-on
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
   (build-depends-on
    :initarg :build-depends-on
    :documentation "A list of dependencies that apply to all files in the
system specified by this build.xcvb file.
These dependencies will be loaded first thing
into an image that will be used for all future compile/load operations")
   (compile-dependencies
    :reader compile-dependencies)
   (cload-dependencies
    :reader cload-dependencies)
   (build-dependencies
    :reader build-dependencies
    :documentation "A normalized version of the above")
   (around-compile
    :initarg :around-compile
    :reader around-compile
    :documentation "a string that, if read after reading the compile dependencies, evaluates in function context to either NIL or a function taking a thunk and returning the values of calling the thunk in a proper some context")
   (encoding
    :initarg :encoding
    :initform nil
    :reader specified-encoding
    :documentation "a keyword specifying the encoding to use for the grain, as per asdf-encodings")
   (extension-forms
    :initarg :extension-forms
    :accessor grain-extension-forms
    :documentation "extension forms to the build specification"))
  (:documentation "Lisp source or build file"))

(defclass lisp-file-grain (lisp-module-grain source-grain explicitly-named-grain)
  ((parent
    :initarg :parent
    :type build-module-grain))
  (:documentation "Lisp file grain"))

(defclass build-module-grain (lisp-module-grain build-registry-entry)
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
    :documentation "A list of ASDF systems superseded by this module, and optional xcvb name")
   (asdf-supersessions
    :reader asdf-supersessions
    :documentation "Parsed version of supersedes-asdf, A-list for asdf name to xcvb name")
   (build-depends-on
    :initform nil)
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

(defclass fasl-grain (loadable-file-grain)
  ()
  (:documentation "Lisp FASL file grain"))

(defclass cfasl-grain (loadable-file-grain)
  ()
  (:documentation "Lisp CFASL file grain"))

(defclass lisp-object-grain (loadable-file-grain)
  ()
  (:documentation "Linkable compiled Lisp object file grain for ECL"))

(defclass asdf-grain (transient-grain explicitly-named-grain build-registry-entry)
  ((name
    :initarg :name
    :reader asdf-grain-system-name)
   (implementation
    :initarg :implementation
    :reader asdf-grain-implementation))
  (:documentation "Loaded ASDF system"))

(defclass require-grain (transient-grain explicitly-named-grain build-registry-entry)
  ((name
    :initarg :name
    :reader require-grain-name))
  (:documentation "Required feature"))

(defclass lisp-generator ()
  ((build :initarg :build :reader generator-build)
   (targets :initarg :targets :reader generator-targets)
   (dependencies :initarg :dependencies :reader generator-dependencies)))

;;; For build registry

(defclass build-registry-entry ()
  ((root
    :initarg :root :accessor bre-root
    :documentation "root path under which the entry was found")))

(defclass invalid-build-registry-entry (build-registry-entry simple-print-object-mixin)
  ((fullname
    :initarg :fullname :reader fullname)))

(defclass build-registry-conflict (invalid-build-registry-entry)
  ((pathnames
    :initarg :pathnames :reader brc-pathnames
    :documentation "pathnames of conflicting build files with the same name")))

(defclass invalid-build-file (invalid-build-registry-entry)
  ((pathname
    :initarg :pathname :reader grain-pathname
    :documentation "pathname of build file with conflicting ancestor")
   (reason
    :initarg :reason :reader invalid-build-reason)))

;;; Module forms

(defmacro module (&rest options)
  ;; Make sure that module declarations don't have an effect when compiled,
  ;; only when read by XCVB.
  (declare (ignore options))
  nil)
