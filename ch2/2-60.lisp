(defun set-member (el set)
  (cond ((null set) nil)
        ((equal el (car set)) t)
        (t (set-member el (cdr set)))))

(defun set-add (el set)
  (cons el set))

(defun set-remove (el set)
  (cond ((null set) nil)
        ((equal el (car set)) (set-remove el (cdr set)))
        (t (cons (car set) (set-remove el (cdr set))))))

(defun set-intersection (s1 s2)
  (remove-if-not (lambda (s1-el) (set-member s1-el s2)) s1))

(defun set-union (s1 s2)
  (append s1 s2))

;; Set-member unchanged
;; Set-add: O(n) => O(1)
;; Set-remove: unchanged
;; Set-intersection: Unchanged
;; Set-union O(n^2) => O(n)

;; So in applications where you do a lot of unions and elements are
;; usually unique, you see performance bumps
