;; Exercise 1

(in-package :sicp-cl)


;; Exercise 1

(define-type :rat)
(defun make-rat (n d)
  (let* ((gcd (gcd (abs n) (abs d)))
         (num (abs (/ n gcd)))
         (dnom (abs (/ d gcd))))
    (construct :rat
               (cond ((> (* n d) 0) (cons num dnom))
                     (t (cons (* -1 num) dnom))))))

;; Exercise 2

(labels ((make (x y) (cons x y))
         (+point-x (p) (car p))
         (+point-y (p) (cdr p)))
  (define-type :point)
  (defun make-point (x y)
    (construct :point (make x y)))
  (define-generic point-x ((:point p))
    (+point-x p))
  (define-generic point-y ((:point p))
    (+point-y p)))

(labels ((make (x y) (cons x y))
         (+seg-p1 (p) (car p))
         (+seg-p2 (p) (cdr p)))
  (define-type :segment)
  (defun make-segment (p1 p2)
    (construct :segment (make p1 p2)))
  (define-generic segment-midpoint ((:segment s))
    (make-point (* 1/2 (+ (point-x (+seg-p1 s)) (point-x (+seg-p2 s))))
                (* 1/2 (+ (point-y (+seg-p1 s)) (point-y (+seg-p2 s))))))
  (define-generic seg-p1 ((:point p))
    (+seg-p1 p))
  (define-generic seg-p2 ((:point p))
    (+seg-p2 p)))

;; Complex Numbers

;; Rectangular Complex Numbers
(labels ((make (a b) (cons a b))
         (+real-part (c) (car c))
         (+imag-part (c) (cdr c))
         (square (x) (* x x)))
  (define-type :complex-rect)
  (defun make-complex-rect (a b)
    (construct :complex-rect (make a b)))
  (define-generic real-part ((:complex-rect c))
    (+real-part c))
  (define-generic imag-part ((:complex-rect c))
    (+imag-part c))
  (define-generic mag ((:complex-rect c))
    (sqrt (+ (square (+real-part c)) (square (+imag-part c))))))

;; Polar Complex Numbers
(labels ((make (r t) (cons r t))
         (+real-part (c) (car c))
         (+imag-part (c) (cdr c))
         (square (x) (* x x)))
  (define-type :complex-rect)
  (defun make-complex-rect (a b)
    (construct :complex-rect (make a b)))
  (define-generic real-part ((:complex-rect c))
    (+real-part c))
  (define-generic imag-part ((:complex-rect c))
    (+imag-part c))
  (define-generic mag ((:complex-rect c))
    (sqrt (+ (square (+real-part c)) (square (+imag-part c))))))

;; Interval Arithmetic

(define-generic lower-bound ((:int-ends x))
  (car x))

;; (let ((a 3))
;;   (define-generic add-a-lower ((:int-ends x))
;;     (+ a (car x))))
