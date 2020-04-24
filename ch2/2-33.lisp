;; accumulate is actually a special case of reduce, but let's go with this for now
(defun accumulate (op initial sequence)
    (if (null sequence)
        initial
        (funcall op (car sequence)
                 (accumulate op initial (cdr sequence)))))

(defun +map (op seq)
  (accumulate (lambda (car cdr-acc) (cons (funcall op car) cdr-acc)) nil seq))

(defun +append (l1 l2)
  (accumulate #'cons l2 l1))

(defun +length (sequence)
  (accumulate (lambda (car cdr-acc) (+ 1 cdr-acc)) 0 sequence))
