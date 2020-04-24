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

(defun div-interval (x y)
  (when (<= (lower-bound y) 0 (upper-bound y))
    (error (format nil "Interval ~a contains 0, cannot divide by it" y)))
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(defun sub-interval (i1 i2)
  (add-interval i1 (mul-interval i2 (make-interval -1 -1))))


(defun make-center-width (c w)
    (make-interval (- c w) (+ c w)))

(defun center (i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(defun width (i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(defun make-center-percent (c p)
  (make-interval (- c (* p (abs c)))
                 (+ c (* p (abs c)))))

(defun percent (i) (/ (width i) (abs (center i))))

(defun par1 (r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))
(defun par2 (r1 r2)
    (let ((one (make-interval 1 1)))
      (div-interval
       one (add-interval (div-interval one r1)
                         (div-interval one r2)))))

;; Solution
(let ((i1 (make-center-width 10 0.1))
      (i2 (make-center-width 20 0.1)))

  (format t "Interval 1: ~a, Interval 2: ~a~%" i1 i2)
  (let ((i (par1 i1 i2)))
    (format t "Par1 yields ~a which has center ~a, tolerance ~a~%" i (center i) (percent i)))
  (let ((i (par2 i1 i2)))
    (format t "Par2 yields ~a which has center ~a, tolerance ~a~%" i (center i) (percent i))))
