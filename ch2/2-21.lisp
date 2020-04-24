(defun +map (fn ls)
  (if (null ls)
      nil
      (cons (funcall fn (car ls)) (+map fn (cdr ls)))))

(defun square-list-1 (items)
  (if (null items)
      nil
      (let* ((n (car items))
             (n2 (* n n))
             (rest (square-list-1 (cdr items))))
        (cons n2 rest))))

(defun square-list-2 (items)
  (+map (lambda (x) (* x x)) items))
