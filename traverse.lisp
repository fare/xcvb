(in-package :xcvb)

(defun traverse (node operation)
  "Takes a node and returns a topologically sorted list of all the sub-nodes
that must be processed when creating the file the node represents.
For object-file nodes the operation argument specifies whether to return a list 
of what is needed to :create the file (e.g. compile the corresponding source 
file) or to :load the existing file into the current lisp image.
For other kinds of nodes, the operation argument merely specifies whether
the node the function is being called on is included in the resulting list."
  (reverse (traverse-helper node operation (make-hash-table :test 'equal))))

(defgeneric traverse-helper (node operation visited-nodes-map)
  (:documentation "helper for the traverse function"))

(defmethod traverse-helper :around (node operation visited-nodes-map)
  ;; Only visit a node once during a traversal.
  (unless (gethash (fullname node) visited-nodes-map)
    ;; Add the node to the map of nodes that have already been traversed
    (setf (gethash (fullname node) visited-nodes-map) T)
    (call-next-method)))

(defmethod traverse-helper ((node asdf-system-node) operation visited-nodes-map)
  (list node))

(defmethod traverse-helper ((node source-file-node) operation visited-nodes-map)
  (list node))

(defmethod traverse-helper ((node lisp-image-node) operation visited-nodes-map)
  (reduce (lambda (dep rest)
            (append (traverse-helper dep :load visited-nodes-map) rest))
          (compile-dependencies node) :from-end T :initial-value nil)) 

(defmethod traverse-helper ((node image-dump-node) 
                            (operation (eql :create)) 
                            visited-nodes-map)
  (traverse-helper (lisp-image node) :load visited-nodes-map))

(defmethod traverse-helper ((node object-file-node) 
                            (operation (eql :create)) 
                            visited-nodes-map)
  (reduce (lambda (dep rest) 
            (append (traverse-helper dep :load visited-nodes-map) rest)) 
          (compile-dependencies node) :from-end T :initial-value nil))

#|(defmethod traverse-helper ((node cfasl-node) (operation (eql :create)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest)
              (nconc (traverse-helper dep :load visited-nodes-map) rest))
            (compile-dependencies node)
            :from-end T
            :initial-value nil)))|#         

(defmethod traverse-helper ((node image-dump-node) 
                            (operation (eql :load))
                            visited-nodes-map)
  (cons node (traverse-helper (lisp-image node) :load visited-nodes-map)))

(defmethod traverse-helper ((node object-file-node) 
                            (operation (eql :load))
                            visited-nodes-map)
  (cons node
        (reduce (lambda (dep rest)
                  (append (traverse-helper dep :load visited-nodes-map) rest))
                (load-dependencies node) :from-end T :initial-value nil)))

#|(defmethod traverse-helper ((node cfasl-node) (operation (eql :load)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (list node)))|#



(defmethod traverse-helper ((node dependency-graph-node-with-dependencies) 
                            (operation (eql :all))
                            visited-nodes-map)
  (cons node
        (reduce (lambda (dep rest)
                  (append (traverse-helper dep :all visited-nodes-map) rest))
                (append (load-dependencies node) (compile-dependencies node))
                :from-end T
                :initial-value nil)))

(defmethod traverse-helper ((node lisp-image-node)
                            (operation (eql :all)) 
                            visited-nodes-map)
  (cons node 
        (reduce (lambda (dep rest)
                  (append (traverse-helper dep :all visited-nodes-map) rest))
          (append (load-dependencies node) (compile-dependencies node)) 
          :from-end T 
          :initial-value nil)))

(defmethod traverse-helper ((node image-dump-node) 
                            (operation (eql :all))
                            visited-nodes-map)
  (cons node (traverse-helper (lisp-image node) :all visited-nodes-map)))

(defmethod traverse-helper ((node source-file-node) 
                            (operation (eql :all))
                            visited-nodes-map)
  nil)


#|(defmethod traverse-helper ((node lisp-image-node) (operation (eql :create-with-cfasls)) visited-nodes-map)
  (reduce (lambda (dep rest) (nconc (traverse-helper dep :load-with-cfasls visited-nodes-map) rest)) (compile-dependencies node) :from-end T :initial-value nil))


(defmethod traverse-helper ((node image-dump-node) (operation (eql :create-with-cfasls)) visited-nodes-map)
  (cons node (traverse-helper (lisp-image node) :load-with-cfasls visited-nodes-map)))



(defmethod traverse-helper ((node fasl-node) (operation (eql :create-with-cfasls)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest)
                         (nconc (traverse-helper dep :load-with-cfasls visited-nodes-map) rest))
                       (compile-dependencies node)
                       :from-end T
                       :initial-value nil)))


(defmethod traverse-helper ((node cfasl-node) (operation (eql :create-with-cfasls)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest)
              (nconc (traverse-helper dep :load-with-cfasls visited-nodes-map) rest))
            (compile-dependencies node)
            :from-end T
            :initial-value nil)))


(defmethod traverse-helper ((node lisp-image-node) (operation (eql :load-with-cfasls)) visited-nodes-map)
  (cons node (reduce (lambda (dep rest) (nconc (traverse-helper dep :load-with-cfasls visited-nodes-map) rest)) (compile-dependencies node) :from-end T :initial-value nil)))

(defmethod traverse-helper ((node image-dump-node) (operation (eql :load-with-cfasls)) visited-nodes-map)
  (cons node (traverse-helper (lisp-image node) :load-with-cfasls visited-nodes-map)))


(defmethod traverse-helper ((node fasl-node) (operation (eql :load-with-cfasls)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (cons node (reduce (lambda (dep rest)
                         (nconc (traverse-helper dep :load-with-cfasls visited-nodes-map) rest))
                       (load-dependencies node)
                       :from-end T
                       :initial-value nil))))

(defmethod traverse-helper ((node cfasl-node) (operation (eql :load-with-cfasls)) visited-nodes-map)
  (unless (nth-value 1 (gethash (fullname node) visited-nodes-map))
    (setf (gethash (fullname node) visited-nodes-map) T)
    (reduce (lambda (dep rest)
              (nconc (traverse-helper dep :load-with-cfasls visited-nodes-map) rest))
            (compile-dependencies node)
            :from-end T
            :initial-value nil)))
|#
