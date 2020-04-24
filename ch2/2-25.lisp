(destructuring-bind
    (l1 l2 l3) '((1 3 (5 7) 9)
                 ((7))
                 (1 (2 (3 (4 (5 (6 7)))))))
  (format t "From ~a~%=> ~a~%" l1 (car (cdr (car (cdr (cdr l1))))))
  (format t "From ~a~%=> ~a~%" l2 (caar l2))
  (format t "From ~a~%=> ~a~%" l3
          (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))))
