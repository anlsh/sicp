(defun deep-reverse (ls)
  (labels ((rec (ls acc)
             (cond ((null ls) acc)
                   ((atom ls) ls)
                   (t (rec (cdr ls) (cons (rec (car ls) nil) acc))))))
    (rec ls nil)))
C
