(defun except-first-denomination (ls) (cdr ls))
(defun first-denomination (ls) (car ls))
(defun no-more? (ls) (null ls))

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (t (+ (cc amount (except-first-denomination coin-values))
              (cc (- amount (first-denomination coin-values)) coin-values)))))

;; Order simply doesn't matter: the originally-presented proof still holds
