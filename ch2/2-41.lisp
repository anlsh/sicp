(defun flatmap (op seq)
  (reduce #'append (mapcar op seq)))

(defun enumerate-interval (i j)
  ;; Generates [i, j]
  (labels ((rec (j acc)
             (if (> i j) acc (rec (1- j) (cons j acc)))))
    (rec j nil)))

(defun n-queens (n)
  ;; Returns arrangements of safe queens (terrible documentation ik)
  (labels
      ((safe (pos curr-queens)
         (every #'identity
                (mapcar (lambda (other-pos)
                          (and (/= (cadr pos) (cadr other-pos))
                               ;; Checking that car unequal unneccessary by construction
                               (/= (- (car pos) (cadr pos)) (- (car other-pos) (cadr other-pos)))
                               (/= (+ (car pos) (cadr pos)) (+ (car other-pos) (cadr other-pos)))))
                        curr-queens)))
       (rec (level curr-queens)
         (if (= level 0)
             (list curr-queens)
             (flatmap (lambda (pos) (rec (1- level) (cons (list level pos) curr-queens)))
                      (remove-if-not (lambda (pos) (safe (list level pos) curr-queens))
                                     (enumerate-interval 1 n))))))
    (rec n '())))

;; (length (n-queens 8))
;; => 92
