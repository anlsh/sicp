(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence)
               (accumulate op initial (cdr sequence)))))

;; Solution
(defun horner-eval (x coefficient-sequence)
  (accumulate (lambda (this-coeff cdr-acc) (+ this-coeff (* x cdr-acc)))
              0 coefficient-sequence))
