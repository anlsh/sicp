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

;; Solution
(defun reverse-via-right (sequence)
  (fold-right (lambda (x cdr-results) (append cdr-results (list x))) nil sequence))

(defun reverse-via-left (sequence)
  (fold-left (lambda (prev-results x) (cons x prev-results)) nil sequence))
