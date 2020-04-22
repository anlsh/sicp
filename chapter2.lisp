;; Exercise 1

(in-package :sicp-cl)

(define-type :int-ends)

(defun make-rat (n d)
  (let* ((gcd (gcd (abs n) (abs d)))
         (num (abs (/ n gcd)))
         (dnom (abs (/ d gcd))))
    (construct :rat
               (cond ((> (* n d) 0) (cons num dnom))
                     (t (cons (* -1 num) dnom))))))

;; Interval Arithmetic

(defun make-int-ends (l r)
  (construct :int-ends (cons l r)))

(define-generic lower-bound ((:int-ends x))
  (car x))

(define-generic upper-bound ((:int-ends x))
  (cdr x))

