;; Same as accumulate
(defun fold-right (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence)
               (fold-right op initial (cdr sequence)))))

(defun fold-left (op initial sequence)
    (labels ((iter (result rest)
               (if (null rest)
                   result
                   (iter (funcall op result (car rest))
                         (cdr rest)))))
      (iter initial sequence)))

(fold-right #'/ 1 (list 1 2 3))
;; => 3/2
(fold-left #'/ 1 (list 1 2 3))
;; => 1/6
(fold-right #'list nil (list 1 2 3))
;; => (1 (2 (3 nil)))
(fold-left #'list nil (list 1 2 3))
;; => (((nil 1) 2) 3)

;; The proper condition to satisfy is associativity
;; ie f(f(a, b), c) = f(a, f(b, c))
