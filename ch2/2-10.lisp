(defun make-interval (a b) (cons a b))
(defun lower-bound (i) (min (car i) (cdr i)))
(defun upper-bound (i) (max (car i) (cdr i)))

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; Solution
(defun div-interval (x y)
  (when (<= (lower-bound y) 0 (upper-bound y))
    (error (format nil "Interval ~a contains 0, cannot divide by it" y)))
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(defun sub-interval (i1 i2)
  (add-interval i1 (mul-interval i2 (make-interval -1 -1))))
