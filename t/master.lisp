#+xcvb (module (:depends-on ("package")))

(in-package #:xcvb-unit-tests)

(defsuite* (run-program/*
            :in root-suite
            :documentation "Test run-program/* and associated functions"))

#|
#:run-program/process-output-stream
#:run-program/read-output-lines
#:run-program/read-output-string
#:run-program/read-output-form
#:run-program/read-output-forms
#:run-program/echo-output
|#

;; Convert the input format to a string stream, read it into a string,
;; and see if they match.
(defun slurp-stream-string/check (input-string &key (test #'string=))
  (let ((input-string (format nil input-string)))
    (with-open-stream (s (make-string-input-stream input-string))
      (is (funcall test input-string (slurp-stream-string s))))))

;; Call with a bunch of strings to call the above function upon.
(defun slurp-stream-string/checks (&rest input-string-list)
  (dolist (input-string input-string-list)
    (funcall #'slurp-stream-string/check input-string)))

;; Check to see if the input-string ins converted correctly to the
;; output-form
(defun slurp-stream-lines/check (input-string output-form &key (test #'equal))
  (let ((input-string (format nil input-string)))
    (with-open-stream (s (make-string-input-stream input-string))
      (is (funcall test output-form (slurp-stream-lines s))))))

;; Check to see if the individual input/output lists passed into this
;; function are correct.
(defun slurp-stream-lines/checks (&rest control-forms)
  (dolist (form control-forms)
    (destructuring-bind (input-string output-form) form
      (funcall #'slurp-stream-lines/check input-string output-form))))

(deftest test/slurp-stream-string ()
  ;; Check to make sure the string is exactly what it is when read
  ;; back through a stream. This is a format specifier so we can
  ;; portably test newline processing.
  (slurp-stream-string/checks
   ""
   " "
   "~%"
   "~%~%"
   "~%~%~%"
   "one~%two~%three~%~%four"
   "one two three four"
   "one two~%three four")

  ;; Check some boundary cases on the types passed.
  (signals error (slurp-stream-string nil))
  (signals error (slurp-stream-string 42))
  (signals error (slurp-stream-string "not valid")))

(deftest test/slurp-stream-lines ()
  (slurp-stream-lines/checks
   ;; input-string first, then expected output-form after its parsing
   '("" nil)
   '(" " (" "))
   '("~%" (""))
   '("~%~%" ("" ""))
   '("~%~%~%" ("" "" ""))
   '("foo" ("foo"))
   '("~%foo" ("" "foo"))
   '("~%foo~%" ("" "foo")) ; consumes last newline!
   '("one~%two~%~%three" ("one" "two" "" "three"))
   '("one~%two~%~%three~%" ("one" "two" "" "three"))
   '("one two three four" ("one two three four"))
   '("one two~%three four~%" ("one two" "three four")))

  ;; Check some boundary cases on the types passed.
  ;; NOTE: NIL is ok since it means read from stdin!
  (signals error (slurp-stream-lines 42))
  (signals error (slurp-stream-lines "not valid")))

(defun common-test/run-program/process-output-stream ()
  ;; Test that the 'echo' program can echo a single string.
  ;; Use the output-processor of slurp-stream-lines.
  (let ((ret (run-program/process-output-stream
              '("echo" "string") 'slurp-stream-lines)))
    (is (equal ret '("string"))))

  ;; Test that the 'echo' program can echo a single string.
  ;; Use the output-processor of slurp-stream-string.
  (let ((ret (run-program/process-output-stream
              '("echo" "string") 'slurp-stream-string)))
    (is (string= ret (nl "string"))))

  ;; Test that the 'echo' program can echo an argument with a space.
  ;; Use the output-procesor of slurp-stream-lines.
  (let ((ret (run-program/process-output-stream
              '("echo" "Hello World") 'slurp-stream-lines)))
    (is (equal ret '("Hello World"))))

  ;; Test that the 'echo' program can echo an argument with a space.
  ;; Use the output-processor of slurp-stream-string.
  (let ((ret (run-program/process-output-stream
              '("echo" "Hello World") 'slurp-stream-string)))
    (is (string= ret (nl "Hello World"))))

  ;; Test that the 'echo' program can echo an argument with a space.
  ;; Use the output-processor of slurp-stream-string. Also use the
  ;; command string form.
  (let ((ret (run-program/process-output-stream
              "echo Hello World" 'slurp-stream-string)))
    (is (string= ret (nl "Hello World"))))

  ;; Test that run-program/process-output-stream fails properly with an
  ;; empty program string
  (signals error (run-program/process-output-stream '("")
                                                    'slurp-stream-lines))
  
  ;; An empty string itself is ok since it is passed to the shell.
  (is (string= "" (run-program/process-output-stream "" 'slurp-stream-string)))

  ;; Test that run-program/process-output-stream fails properly with an
  ;; program list containing a nil executable
  (signals error (run-program/process-output-stream '(nil)
                                                    'slurp-stream-lines))

  ;; Test that run-program/process-output-stream fails properly with a
  ;; nil program list
  (signals error (run-program/process-output-stream nil 'slurp-stream-lines))

  ;; Test that run-program/process-output-stream fails properly when the
  ;; executable doesn't exist.
  (signals error (run-program/process-output-stream '("does-not-exist")
                                                    'slurp-stream-lines))
  (signals error (run-program/process-output-stream "does-not-exist"
                                                    'slurp-stream-lines)))

(defun unix-only-test/run-program/process-output-stream ()

  (let ((tf (namestring (asdf:system-relative-pathname :xcvb "t/test-file"))))

    ;; a basic smoke test
    (is (equal '("Single")
               (run-program/read-output-lines
                `("/bin/grep" "Single" ,tf))))

    ;; Make sure space is handled correctly
    (is (equal '("double entry")
               (run-program/read-output-lines
                `("/bin/grep" "double entry" ,tf))))

    ;; Make sure space is handled correctly
    (is (equal '("triple word entry")
               (run-program/read-output-lines
                `("/bin/grep" "triple word entry" ,tf))))

    ;; Testing special characters
    (loop :for char :across "+-_.,%@:/\\!&*(){}"
      :for str = (string char) :do
      (is (equal (list (strcat "escape " str))
                 (run-program/read-output-lines
                  `("/bin/grep" ,(strcat "[" str "]") ,tf)))))

    ;; Test that run-program/process-output-stream signals an error
    ;; with an executable that doesn't return 0
    (signals error (run-program/read-output-lines '("/bin/false")))

    ;; Test that we can suppress the error on run-program/process-output-stream
    (is (null (run-program/read-output-lines '("/bin/false")
                                             :ignore-error-status t)))))

(defun windows-only-test/run-program/process-output-stream ()

  ;; a basic smoke test
  (is (equal (run-program/read-output-lines '("cmd" "/c" "echo" "ok"))
             '(("ok"))))

  nil)

(deftest test/run-program/process-output-stream ()
  (common-test/run-program/process-output-stream)
  (using-unix
    (unix-only-test/run-program/process-output-stream))
  (using-windows
    (windows-only-test/run/program/process/output-stream)))
