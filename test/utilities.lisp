(in-package :xcvb-test)

(assert
 (equal
  (strcat "foo" "bar" "baz")
  "foobarbaz"))

(assert
 (equal
  (join-strings ":" '("/bin" "/usr/bin" "/usr/local/bin"))
  "/bin:/usr/bin:/usr/local/bin"))

(assert
 (equal
  (flatten-f '((1 (2)) 3 4 5) (lambda (x) (cond ((null x) nil) ((consp x) x) (t (list x)))))
  '(((1 (2)) 3 4 5) (1 (2)) 1 (2) 2 3 4 5)))
