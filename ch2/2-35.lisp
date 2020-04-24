(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence)
               (accumulate op initial (cdr sequence)))))

;; Solution
(defun count-leaves (tree)
  (accumulate #'+ 0 (mapcar (lambda (tr) (if (atom tr) 1 (count-leaves tr))) tree)))
