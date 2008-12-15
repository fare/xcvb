(in-package :xcvb)

(defparameter *extension-functions-map* (make-hash-table)
  "Map of keywords that may appear in the extension-forms part
of a module declaration to functions")


(defmacro defextension (name keyword args &body body)
  `(setf (gethash ,keyword *extension-functions-map*)
         (defun ,name (,@args) ,@body)))

(defun handle-extension-forms (module)
  "This handles the extension forms from the module declaration.
These forms can do things such as (but not limited to)
change slots in the module, specify system-wide dependencies,
or extend xcvb itself.
If an extension function needs to have access to
the module object that it is declared in,
then when writing the extension form in the module declaration of a file,
use the keyword :this-module to represent the current module.
The extension function will be passed the current module
instead of any arguments that are :this-module."
  (dolist (form (extension-forms module))
    (destructuring-bind (operation &rest args) form
      (apply
       (gethash operation *extension-functions-map*)
       (mapcar (lambda (arg) (if (eql arg :this-module) module arg)) args)))))


(defextension add-dependencies-to-module :add (module dep-type deps)
  "Extension form that can be put in a module declaration
to add additional dependencies to that module"
  (with-slots (compile-depends-on load-depends-on) module
    (ecase dep-type
      (:compile-depends-on
         (setf compile-depends-on
               (append compile-depends-on deps)))               
      (:load-depends-on
         (setf load-depends-on
               (append load-depends-on deps)))               
      (:compile-and-load-depends-on
         (setf compile-depends-on
               (append compile-depends-on deps))
         (setf load-depends-on
               (append load-depends-on deps))))))


(defextension remove-dependency-from-module :remove (module dep-type value)
  "Extension form that can be put in a module declaration
to remove dependencies from that module"
  (with-slots (compile-depends-on load-depends-on) module
    (ecase dep-type
      (:compile-depends-on
         (setf compile-depends-on
               (remove value compile-depends-on :test #'equal)))
      (:load-depends-on
         (setf load-depends-on
               (remove value load-depends-on :test #'equal)))
      (:compile-and-load-depends-on
         (setf compile-depends-on
               (remove value compile-depends-on :test #'equal))
         (setf load-depends-on
               (remove value load-depends-on :test #'equal))))))


(defextension set-module-slot :set (module slot-name value)
  "Extension form that can be put in a module declaration
to overwrite the dependencies of that module"
  (let ((slot-symbol (find-symbol (string slot-name) :xcvb)))
    (if (slot-exists-p module slot-symbol)
      (setf (slot-value module slot-symbol) value)
      (error 'simple-error
             :format-control "the slot ~a is not a valid slot for the module ~a"
             :format-arguments (list slot-name (fullname module))))))

(defextension load-into-xcvb :xcvb-requires (deps)
  "Extension form that can be put in a module declaration
so that xcvb will load the given file into itself.
This provides a good facility for users to load their own extensions into xcvb.
It doesn't work yet since it depends on adding to xcvb the functionality
to be able to load a system into the current lisp image."
  (load-systems deps))

(defun load-systems (systems)
  (declare (ignore systems))
  (error "not yet implemented"))
