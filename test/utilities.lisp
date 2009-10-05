(in-package :xcvb)

(assert
 (equal
  (strcat "foo" "bar" "baz")
  "foobarbaz"))

(assert
 (equal
  (join-strings ":" '("/bin" "/usr/bin" "/usr/local/bin"))
  "/bin:/usr/bin:/usr/local/bin"))

