(in-package :xcvb)

;;(DBG :static-backends ...)
(progn
  (reset-variables)
  (search-search-path)
  (graph-for (make-instance 'static-traversal) '(:build "/c")))

