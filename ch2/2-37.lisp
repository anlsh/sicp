(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence)
               (accumulate op initial (cdr sequence)))))

(defun accumulate-n (op initial sequences)
  (if (null (car sequences))
      nil
      (cons (accumulate op initial (mapcar #'car sequences))
            (accumulate-n op initial (mapcar #'cdr sequences)))))

(defun dot-product (v1 v2) (accumulate #'+ 0 (mapcar #'* v1 v2)))

;; Solution
(defun mat-*-vector (m v) (mapcar (lambda (r) (dot-product r v)) m))
(defun transpose-matrix (m) (accumulate-n #'cons nil m))
