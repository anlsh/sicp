;; Direct definition
(defun square-tree-direct (tree)
  (cond ((null tree) nil)
        ((listp tree) (cons (square-tree-direct (car tree)) (square-tree-direct (cdr tree))))
        (t (* tree tree))))

;; Using higher-order functions
(defun square-tree-higher (tree)
  (if (listp tree)
      (mapcar #'square-tree-higher tree)
      (* tree tree)))
