;; It's in reverse because it prepends to the accumulator at every step
(defun square-list (items)
  (labels ((square (x) (* x x))
           (iter (things answer)
             (if (null things)
                 answer
                 (iter (cdr things)
                       (cons (square (car things))
                             answer)))))
    (iter items nil)))
(square-list '(1 2 3 4 5 6 7))

;; You get a really malformed tree: what you want is append-single-element, not *cons*
(defun square-list (items)
  (labels ((square (x) (* x x))
           (iter (things answer)
             (if (null things)
                 answer
                 (iter (cdr things)
                       (cons answer
                             (square (car things)))))))
    (iter items nil)))
(square-list '(1 2 3 4 5 6 7))
