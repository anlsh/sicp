;; Utils

(defun make-point (x y) (cons x y))
(defun point-x (p) (car p))
(defun point-y (p) (cdr p))

;; Implementation 1
;; Points of opposite corners
(labels ()
  (defun make-rect (p1 p2)
    (cons p1 p2))
  (defun rect-width (r) (abs (- (point-x (car r)) (point-x (cdr r)))))
  (defun rect-height (r) (abs (- (point-y (car r)) (point-y (cdr r))))))


;; Implementation 2
;; Bottom-left + length and width
(labels ()
  (defun make-rect (p w h)
    (list p w h))
  (defun rect-width (r) (second r))
  (defun rect-height (r) (third r)))


;; Functions the exercise asks for

(defun perimeter (rect)
  (* 2 (+ (rect-width rect) (rect-height rect))))

(defun area (rect)
  (* (rect-width rect) (rect-height rect)))
