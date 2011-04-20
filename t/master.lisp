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

(deftest test/slurp-stream-* ()
  ;; Test that slurp-stream-stream function works in the common case
  (let ((input-string (format nil "one~%two~%three~%")))
    (with-open-stream (s (make-string-input-stream input-string))
      (is (string= input-string (slurp-stream-string s)))))

  ;; Test that slurp-stream-lines function works in the common case
  (let ((output-list '("one" "two" "" "three"))
        (input-string (format nil "one~%two~%~%three")))
    (with-open-stream (s (make-string-input-stream input-string))
      (is (equal output-list (slurp-stream-lines s)))))

  ;; Test that slurp-stream-lines function works with no input
  (let ((output-list nil)
        (input-string (format nil "")))
    (with-open-stream (s (make-string-input-stream input-string))
      (is (equal output-list (slurp-stream-lines s))))))


(deftest test/run-program/process-output-stream ()
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

  ;; Test that run-program/process-output-stream fails properly with an
  ;; empty program string
  (signals error (run-program/process-output-stream '("")
                                                    'slurp-stream-lines))

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

  ;; XXX This is UNIX specific
  ;; Test that run-program/process-output-stream signals an error with an
  ;; executable that doesn't return 0
  (signals error (run-program/process-output-stream '("/bin/false")
                                                     'slurp-stream-lines))

  ;; XXX This is UNIX specific
  ;; Test that we can surpress the error on run-program/process-output-stream
  (is (null (run-program/process-output-stream '("/bin/false")
                                               'slurp-stream-lines
                                               :ignore-error-status t))))
