(in-package :xcvb)

;;TODO: this code is really ugly.

;;TODO: double check this explanation.

;;TODO: use :use instead of :load where appropriate

(defun traverse (node operation)
  "Takes a node and returns a topologically sorted list of all the sub-nodes
that must be processed when creating the file the node represents.
The operation argument specifies whether to return a list of what is needed
to :create the file (e.g. compile the corresponding source file) or
to :use the existing file (e.g. load it into the current image).
For other kinds of nodes, the operation argument merely specifies whether
the node the function is being called on is included in the resulting list."
  (reverse (traverse-internal node operation (make-hash-table :test 'equal))))

(defgeneric traverse-internal (node operation visited-nodes-map)
  (:documentation "helper for the traverse function"))

(defmethod traverse-internal :around (node operation visited-nodes-map)
  ;; Only visit a node once during a traversal.
  (unless (gethash (fullname node) visited-nodes-map)
    ;; Add the node to the map of nodes that have already been traversed
    (setf (gethash (fullname node) visited-nodes-map) T)
    (call-next-method)))

(defmethod traverse-internal ((node asdf-system-node) operation visited-nodes-map)
  (list node))

(defmethod traverse-internal ((node source-file-node) operation visited-nodes-map)
  (list node))

(defmethod traverse-internal ((node lisp-node) (operation (eql :create)) visited-nodes-map)
  (mapcan (lambda (dep) (traverse-internal dep :load visited-nodes-map))
          (compile-dependencies node)))

(defmethod traverse-internal ((node image-dump-node) (operation (eql :create)) visited-nodes-map)
  (cons node
        (traverse-internal (lisp-image node) :create visited-nodes-map)))

(defmethod traverse-internal ((node object-file-node) (operation (eql :create)) visited-nodes-map)
  (mapcan (lambda (dep)
            (traverse-internal dep :load visited-nodes-map))
          (compile-dependencies node)))

#|(defmethod traverse-internal ((node cfasl-node) (operation (eql :create)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest)
              (nconc (traverse-internal dep :load visited-nodes-map) rest))
            (compile-dependencies node)
            :from-end T
            :initial-value nil)))|#

(defmethod traverse-internal ((node lisp-node) (operation (eql :load)) visited-nodes-map)
  (mapcan (lambda (dep) (traverse-internal dep :load visited-nodes-map))
          (compile-dependencies node)))

(defmethod traverse-internal ((node image-dump-node) (operation (eql :load)) visited-nodes-map)
  (traverse-internal (lisp-image node) :load visited-nodes-map))

(defmethod traverse-internal ((node object-file-node) (operation (eql :load)) visited-nodes-map)
  (cons node
        (mapcan (lambda (dep)
                  (traverse-internal dep :load visited-nodes-map))
                (load-dependencies node))))

#|(defmethod traverse-internal ((node cfasl-node) (operation (eql :load)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (list node)))|#



(defmethod traverse-internal ((node dependency-graph-node-with-dependencies) (operation (eql :all)) visited-nodes-map)
  (cons node
        (mapcan (lambda (dep)
                  (traverse-internal dep :all visited-nodes-map))
                (append (load-dependencies node) (compile-dependencies node)))))

(defmethod traverse-internal ((node image-dump-node) (operation (eql :all)) visited-nodes-map)
  (cons node (traverse-internal (lisp-image node) :all visited-nodes-map)))

(defmethod traverse-internal ((node source-file-node) (operation (eql :all)) visited-nodes-map)
  nil)


#|(defmethod traverse-internal ((node lisp-node) (operation (eql :create-with-cfasls)) visited-nodes-map)
  (reduce (lambda (dep rest) (nconc (traverse-internal dep :load-with-cfasls visited-nodes-map) rest)) (compile-dependencies node) :from-end T :initial-value nil))


(defmethod traverse-internal ((node image-dump-node) (operation (eql :create-with-cfasls)) visited-nodes-map)
  (cons node (traverse-internal (lisp-image node) :load-with-cfasls visited-nodes-map)))



(defmethod traverse-internal ((node fasl-node) (operation (eql :create-with-cfasls)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest)
                         (nconc (traverse-internal dep :load-with-cfasls visited-nodes-map) rest))
                       (compile-dependencies node)
                       :from-end T
                       :initial-value nil)))


(defmethod traverse-internal ((node cfasl-node) (operation (eql :create-with-cfasls)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest)
              (nconc (traverse-internal dep :load-with-cfasls visited-nodes-map) rest))
            (compile-dependencies node)
            :from-end T
            :initial-value nil)))


(defmethod traverse-internal ((node lisp-node) (operation (eql :load-with-cfasls)) visited-nodes-map)
  (cons node (reduce (lambda (dep rest) (nconc (traverse-internal dep :load-with-cfasls visited-nodes-map) rest)) (compile-dependencies node) :from-end T :initial-value nil)))

(defmethod traverse-internal ((node image-dump-node) (operation (eql :load-with-cfasls)) visited-nodes-map)
  (cons node (traverse-internal (lisp-image node) :load-with-cfasls visited-nodes-map)))


(defmethod traverse-internal ((node fasl-node) (operation (eql :load-with-cfasls)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (cons node (reduce (lambda (dep rest)
                         (nconc (traverse-internal dep :load-with-cfasls visited-nodes-map) rest))
                       (load-dependencies node)
                       :from-end T
                       :initial-value nil))))

(defmethod traverse-internal ((node cfasl-node) (operation (eql :load-with-cfasls)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest)
              (nconc (traverse-internal dep :load-with-cfasls visited-nodes-map) rest))
            (compile-dependencies node)
            :from-end T
            :initial-value nil)))
|#
