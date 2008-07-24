(in-package :xcvb)

;;TODO: this code is really ugly.

(defgeneric traverse-internal (node operation visited-nodes-map)
  (:documentation "Takes a node and returns a list of all nodes in order that must be processed in order to create the target of the node. For fasl-nodes, the operation argument specifies whether to return a list of what is needed to create the fasl (by compiling the source file) or what is needed to load the fasl if it already exists.  For other kinds of nodes, the operation argument mearly changes whether or not the node the method is being called on is included in the resulting list."))

(defmethod traverse-internal ((node asdf-system-node) operation visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map));If this node has already been put in the traversal, don't include it again.
    (setf (gethash (fullname node) visited-nodes-map) T);Add the node to the map of nodes that have already been traversed
    (list node)))

(defmethod traverse-internal ((node source-file-node) operation visited-nodes-map)
  (list node))


(defmethod traverse-internal ((node lisp-node) (operation (eql :create)) visited-nodes-map)
  (reduce (lambda (dep rest) (nconc (traverse-internal dep :load visited-nodes-map) rest)) (compile-dependencies node) :from-end T :initial-value nil))


(defmethod traverse-internal ((node image-dump-node) (operation (eql :create)) visited-nodes-map)
  (cons node (traverse-internal (lisp-image node) :create visited-nodes-map)))



(defmethod traverse-internal ((node fasl-or-cfasl-node) (operation (eql :create)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest) 
              (nconc (traverse-internal dep :load visited-nodes-map) rest)) 
            (compile-dependencies node) 
            :from-end T 
            :initial-value nil)))

#|(defmethod traverse-internal ((node cfasl-node) (operation (eql :create)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest) 
              (nconc (traverse-internal dep :load visited-nodes-map) rest)) 
            (compile-dependencies node) 
            :from-end T 
            :initial-value nil)))|#


(defmethod traverse-internal ((node lisp-node) (operation (eql :load)) visited-nodes-map)
  (reduce (lambda (dep rest) (nconc (traverse-internal dep :load visited-nodes-map) rest)) (compile-dependencies node) :from-end T :initial-value nil))

(defmethod traverse-internal ((node image-dump-node) (operation (eql :load)) visited-nodes-map)
  (traverse-internal (lisp-image node) :load visited-nodes-map))

(defmethod traverse-internal ((node fasl-or-cfasl-node) (operation (eql :load)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (cons node (reduce (lambda (dep rest) 
                         (nconc (traverse-internal dep :load visited-nodes-map) rest)) 
                       (load-dependencies node) 
                       :from-end T 
                       :initial-value nil))))

#|(defmethod traverse-internal ((node cfasl-node) (operation (eql :load)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (list node)))|#



(defmethod traverse-internal ((node dependency-graph-node-with-dependencies) (operation (eql :all)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (cons node (reduce (lambda (dep rest) 
                         (nconc (traverse-internal dep :all visited-nodes-map) rest)) 
                       (append (load-dependencies node) (compile-dependencies node))
                       :from-end T 
                       :initial-value nil))))

(defmethod traverse-internal ((node image-dump-node) (operation (eql :all)) visited-nodes-map)
  (cons node (traverse-internal (lisp-image node) :all visited-nodes-map)))

(defmethod traverse-internal ((node source-file-node) (operation (eql :all)) visited-nodes-map)
  nil)

(defun traverse (node operation)
  "Wrapper around traverse-internal generic function"
  (reverse (traverse-internal node operation (make-hash-table :test 'equal))))
  




(defmethod traverse-internal ((node lisp-node) (operation (eql :create-with-cfasls)) visited-nodes-map)
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