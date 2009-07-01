#+xcvb
(module
 (:description "package for command-line-arguments"))

(cl:defpackage :command-line-arguments
  (:use :cl)
  (:export
   #:*command-line-arguments*
   #:*command-line-options*
   #:*command-line-option-specification*
   #:process-command-line-options
   #:compute-and-process-command-line-options
   #:get-command-line-arguments
   #:show-option-help
   ))
