(defun make-point (x y) (cons x y))
(defun point-x (p) (car p))
(defun point-y (p) (cdr p))

(defun make-segment (p1 p2)
  (cons p1 p2))

(defun start-segment (s)
  (car s))

(defun end-segment (s)
  (cdr s))

(defun midpoint (s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (make-point (* 1/2 (+ (point-x p1) (point-x p2)))
                (* 1/2 (+ (point-y p1) (point-y p2))))))
