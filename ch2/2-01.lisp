(in-package :sicp-cl)

(defun make-rat (n d)
  (let* ((gcd (gcd (abs n) (abs d)))
         (num (abs (/ n gcd)))
         (dnom (abs (/ d gcd))))
    (cond ((> (* n d) 0) (cons num dnom))
          (t (cons (* -1 num) dnom)))))
