(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence)
               (accumulate op initial (cdr sequence)))))

;; Solution
(defun accumulate-n (op initial sequences)
  (if (null (car sequences))
      nil
      (cons (accumulate op initial (mapcar #'car sequences))
            (accumulate-n op initial (mapcar #'cdr sequences)))))

(accumulate-n #'+ 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
;; => (22 26 30)
