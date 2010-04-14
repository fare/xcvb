#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :xcvb-hello)

(defvar *version* nil)

(defparameter +hello-option-spec+
  '((("traditional" #\t) :type boolean :optional t
     :documentation "traditional version, no punctuation")
    (("name" #\n) :type string :initial-value "World"
     :documentation "name of person(s) to salute")
    (("help" #\h #\?) :type boolean :optional t
     :documentation "display help")
    (("version" #\v) :type integer :optional t
     :documentation "display version")))
