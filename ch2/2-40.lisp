(defun flatmap (op seq)
  (reduce #'append (mapcar op seq)))

(defun enumerate-interval (i j)
  ;; Generates [i, j]
  (labels ((rec (j acc)
             (if (> i j) acc (rec (1- j) (cons j acc)))))
    (rec j nil)))

(defun all-pairs (n)
  ;; Generate all (i, j) where i, j in [1, n] and i < j
  (flatmap (lambda (i) (mapcar (lambda (j) (list i j)) (enumerate-interval 1 (1- i))))
           (enumerate-interval 1 n)))
