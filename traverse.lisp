(in-package :xcvb)

(defun traverse (node operation)
  "Takes a node and an operation, call traverse-nodes on just that node."
  (traverse-nodes (list (list node operation))))

(defun traverse-nodes (node-operation-list)
  "Takes a list of specifications of a node and an operation (as a list),
and returns a topologically sorted list of all the sub-nodes
that must be processed when creating the files the nodes represents.
For object-file nodes the operation argument specifies whether to return a list
of what is needed to :create the file (e.g. compile the corresponding source
file) or to :load the existing file into the current lisp image.
For other kinds of nodes, the operation argument merely specifies whether
the node the function is being called on is included in the resulting list."
  (let ((visited-nodes-map (make-hash-table :test 'equal))
	(list nil))
    (flet ((pusher (x) (push x list)))
      (loop for (node operation) in node-operation-list do
	    (traverse-helper node operation #'pusher visited-nodes-map)))
    (reverse list)))

(defgeneric traverse-helper (node operation pusher visited-nodes-map)
  (:documentation "helper for the traverse function"))

(defmethod traverse-helper :around (node operation pusher visited-nodes-map)
  ;; Only visit a node once during a traversal.
  (unless (gethash (fullname node) visited-nodes-map)
    ;; Add the node to the map of nodes that have already been traversed
    (setf (gethash (fullname node) visited-nodes-map) T)
    (call-next-method)))

(defmacro define-traverse ((node operation) &body body)
  (with-gensyms (pusher visited-nodes-map x y op)
    `(defmethod traverse-helper (,node ,operation ,pusher ,visited-nodes-map)
      (labels ((c (,x) (funcall ,pusher ,x))
	       (rec (,x ,y) (traverse-helper ,x ,y ,pusher ,visited-nodes-map))
	       (rec* (,y ,op) (dolist (,x ,y) (rec ,x ,op))))
	(declare (ignorable #'c #'rec #'rec*))
	,@body))))

(define-traverse ((node asdf-system-node) operation)
  (c node))

(define-traverse ((node source-file-node) operation)
  (c node))

(define-traverse ((node lisp-image-node) operation)
  (rec* (compile-dependencies node) :load))

(define-traverse ((node image-dump-node)
		  (operation (eql :create)))
  (rec (lisp-image node) :load))

(define-traverse ((node object-file-node)
		  (operation (eql :create)))
  (rec* (compile-dependencies node) :load))

#|(define-traverse ((node cfasl-node) (operation (eql :create)))
  (rec* (compile-dependencies node) :load))|#

(define-traverse ((node image-dump-node) (operation (eql :load)))
  (rec (lisp-image node) :load)
  (c node))

(define-traverse ((node object-file-node) (operation (eql :load)))
  (rec* (load-dependencies node) :load)
  (c node))

#|(define-traverse ((node cfasl-node) (operation (eql :load)))
  (c node))|#

(define-traverse ((node dependency-graph-node-with-dependencies) (operation (eql :all)))
  (rec* (compile-dependencies node) :all)
  (rec* (load-dependencies node) :all)
  (c node))

(define-traverse ((node lisp-image-node) (operation (eql :all)))
  (rec* (compile-dependencies node) :all)
  (rec* (load-dependencies node) :all)
  (c node))

(define-traverse ((node image-dump-node) (operation (eql :all)))
  (rec (lisp-image node) :all)
  (c node))

(define-traverse ((node source-file-node) (operation (eql :all)))
  nil)

#|(define-traverse ((node lisp-image-node) (operation (eql :create-with-cfasls)))
  (rec* (compile-dependencies node) :load-with-cfasls))

(define-traverse ((node image-dump-node) (operation (eql :create-with-cfasls)))
  (rec (lisp-image node) :load-with-cfasls))

(define-traverse ((node fasl-node) (operation (eql :create-with-cfasls)))
  (rec* (compile-dependencies node) :load-with-cfasls))

(define-traverse ((node cfasl-node) (operation (eql :create-with-cfasls))
  (rec* (compile-dependencies node) :load-with-cfasls)))

(define-traverse ((node lisp-image-node) (operation (eql :load-with-cfasls)))
  (rec* (compile-dependencies node) :load-with-cfasls)
  (c node))

(define-traverse ((node image-dump-node) (operation (eql :load-with-cfasls)))
  (rec (lisp-image node) :load-with-cfasls)
  (c node))

(define-traverse ((node fasl-node) (operation (eql :load-with-cfasls)))
  (rec* (load-dependencies node) :load-with-cfasls)
  (c node))

(define-traverse ((node cfasl-node) (operation (eql :load-with-cfasls)))
  (rec* (compile-dependencies node) :load-with-cfasls))
|#
