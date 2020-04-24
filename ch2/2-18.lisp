(defun +reverse (ls &optional (acc nil))
  (if (null ls)
      acc
      (+reverse (cdr ls) (cons (car ls) acc))))
