(in-package :xcvb)

(defextension add-dependencies-to-module :add (module dep-type deps)
  "Extension form that can be put in a module declaration to add additional dependencies to that module"
  (with-slots (compile-depends-on load-depends-on) module
    (case dep-type
      (:compile-depends-on 
         (setf compile-depends-on (remove-duplicates (append compile-depends-on deps) :test #'equal :from-end T)))
      (:load-depends-on 
         (setf load-depends-on (remove-duplicates (append load-depends-on deps) :test #'equal :from-end T)))
      (:compile-and-load-depends-on 
         (setf compile-depends-on (remove-duplicates (append compile-depends-on deps) :test #'equal :from-end T))
         (setf load-depends-on (remove-duplicates (append load-depends-on deps) :test #'equal :from-end T)))
      (otherwise (error "Invalid property for :add operation, must be one of (:compile-depends-on :load-depends-on :compile-and-load-depends-on)")))))


(defextension remove-dependency-from-module :remove (module dep-type value)
  "Extension form that can be put in a module declaration to remove dependencies from that module"
  (case dep-type
    (:compile-depends-on 
       (setf (slot-value module 'compile-depends-on) 
             (remove value (slot-value module 'compile-depends-on) :test #'equal)))
    (:load-depends-on 
       (setf (slot-value module 'load-depends-on) 
             (remove value (slot-value module 'load-depends-on) :test #'equal)))
    (:compile-and-load-depends-on 
       (setf (slot-value module 'compile-depends-on) 
             (remove value (slot-value module 'compile-depends-on) :test #'equal))
       (setf (slot-value module 'load-depends-on) 
             (remove value (slot-value module 'load-depends-on) :test #'equal)))
    (otherwise (error "Invalid property for :remove operation, must be one of (:compile-depends-on :load-depends-on :compile-and-load-depends-on"))))

(defextension set-module-slot :set (module slot-name value)
  "Extension form that can be put in a module declaration to overwrite the dependencies of that module"
  (let ((slot-symbol (find-symbol (string slot-name) :xcvb)))
    (if slot-symbol
      (setf (slot-value module slot-symbol) value)
      (error 'simple-error 
             :format-control "the slot ~a is not a valid slot for the module" 
             :format-arguments (list slot-name)))))

(defextension load-into-xcvb :xcvb-requires (deps)
  "Extension form that can be put in a module declaration so that xcvb will load the given file into itself.  This provides a good facility for users to load their own extensions into xcvb"
  (load-systems deps))

(defun load-systems (systems)
  (declare (ignore systems))
  (error "not yet implemented"))
