;; Simple test for a2x

(asdf:defsystem :a2x-test
    :author ("Francois-Rene Rideau")
    :depends-on (:cl-launch)
    :components
    ((:file "pkgdcl")
     (:file "specials" :depends-on ("pkgdcl"))
     (:file "help" :depends-on ("specials"))
     (:file "main" :depends-on ("help"))))
