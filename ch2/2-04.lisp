(defun +cons (a b)
  (lambda (m) (funcall m a b)))

(labels ((extract-exp (nat base &optional (acc 0))
           (if (zerop (mod nat base)) (extract-exp (/ nat base) base (1+ acc)) acc)))
  (defun +car (p) (funcall p (lambda (x y) (declare (ignore y)) x)))
  (defun +cdr (p) (funcall p (lambda (x y) (declare (ignore x)) y))))
