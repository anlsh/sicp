;; Nil isn't treated as a leaf node in this implementation
(defun tree-map (fn tree)
  (if (atom tree)
      (funcall fn tree)
      (mapcar (lambda (x) (tree-map fn x)) tree)))
