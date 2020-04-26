(defun set-member (el set)
  (cond ((null set) nil)
        ((equal el (car set)) t)
        (t (set-member el (cdr set)))))

(defun set-add (el set)
  (if (set-member el set)
      set
      (cons el set)))

(defun set-remove (el set)
  (cond ((null set) nil)
        ((equal el (car set)) (cdr set))
        (t (cons (car set) (set-remove el (cdr set))))))

;; Bit different than the code in SICP, but the same algorithm really
(defun set-intersection (s1 s2)
  (remove-if-not (lambda (s1-el) (set-member s1-el s2)) s1))

;; Solution using fold-right
(defun set-union (s1 s2)
  (reduce #'cons (remove-if (lambda (s1-el) (set-member s1-el s2)) s1)
          :from-end t :initial-value s2))
