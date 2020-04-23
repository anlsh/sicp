;; Exercise 1

(in-package :sicp-cl)

(define-type :int-ends)

(defun make-rat (n d)
  (let* ((gcd (gcd (abs n) (abs d)))
         (num (abs (/ n gcd)))
         (dnom (abs (/ d gcd))))
    (construct :int-ends
               (cond ((> (* n d) 0) (cons num dnom))
                     (t (cons (* -1 num) dnom))))))

;; Interval Arithmetic

(define-generic lower-bound ((:int-ends x))
  (car x))

(let ((a 3))
  (define-generic add-a-lower ((:int-ends x))
    (+ a (car x))))
