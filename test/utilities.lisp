(in-package :xcvb)

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
  (all-descendents-f '((1 (2)) 3 4 5)
                     (lambda (x) (if (listp x) x (list x))))
  '(((1 (2)) 3 4 5) (1 (2)) 1 (2) 2 3 4 5)))
