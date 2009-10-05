;; This should be generalized and moved to some generic file
(defun group-grains-by-implementation (asdf-grains)
    (loop :while asdf-grains :collect
      (loop
        :with implementation = (asdf-grain-implementation (first asdf-grains))
        :for grain :in asdf-grains
        :when (equal implementation (asdf-grain-implementation grain))
        :collect grain :into in
        :else :collect grain :into out
        :finally (progn
                   (setf asdf-grains out)
                   (return in)))))

(defun all-descendents-f (x children-f &key (test #'eql))
  (let ((r nil))
    (labels ((add1 (x)
               (unless (member x r :test test)
                 (push x r)
                 (map () #'add1 (funcall children-f x)))))
      (add1 x))
    (nreverse r)))

(assert
 (equal
  (all-descendents-f '((1 (2)) 3 4 5)
                     (lambda (x) (if (listp x) x (list x))))
  '(((1 (2)) 3 4 5) (1 (2)) 1 (2) 2 3 4 5)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "~&~S~%"
          (macroexpand-1 '

(FOO)

)))
