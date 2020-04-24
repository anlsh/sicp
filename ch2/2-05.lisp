(defun +cons (a b)
  (* (expt 2 a) (expt 3 b)))

(labels ((extract-exp (nat base &optional (acc 0))
           (if (zerop (mod nat base)) (extract-exp (/ nat base) base (1+ acc)) acc)))
  (defun +car (p) (extract-exp p 2))
  (defun +cdr (p) (extract-exp p 3)))
