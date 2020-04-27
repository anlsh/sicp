(defun make-tree (entry left right)
  (list entry left right))
(defun entry (tree) (car tree))
(defun left-branch (tree) (cadr tree))
(defun right-branch (tree) (caddr tree))

(defun inter-lset (set1 set2)
  (if (or (null set1) (null set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (inter-lset (cdr set1)
                                    (cdr set2))))
              ((< x1 x2)
               (inter-lset (cdr set1) set2))
              ((< x2 x1)
               (inter-lset set1 (cdr set2)))))))

(defun adjoin-lset (el set)
  (cond ((null set) (list el))
        ((> el (car set)) (cons (car set) (adjoin-lset el (cdr set))))
        ((= el (car set)) set)
        ((< el (car set)) (cons el set))))

;; Solution
(defun union-lset (s1 s2)
  (cond ((null s1) s2)
        ((null s2) s1)
        (t (let ((s1car (car s1)) (s2car (car s2)))
             (cond ((= s1car s2car) (cons s1car (union-lset (cdr s1) (cdr s2))))
                   ((< s1car s2car) (cons s1car (union-lset (cdr s1) s2)))
                   ((> s1car s2car) (cons s2car (union-lset s1 (cdr s2)))))))))

(defun list->tree (elements)
  (labels ((partial-tree (elts n)
             (if (= n 0)
                 (cons '() elts)
                 (let ((left-size (floor (- n 1) 2)))
                   (let ((left-result
                           (partial-tree elts left-size)))
                     (let ((left-tree (car left-result))
                           (non-left-elts (cdr left-result))
                           (right-size (- n (+ left-size 1))))
                       (let ((this-entry (car non-left-elts))
                             (right-result
                               (partial-tree
                                (cdr non-left-elts)
                                right-size)))
                         (let ((right-tree (car right-result))
                               (remaining-elts
                                 (cdr right-result)))
                           (cons (make-tree this-entry
                                            left-tree
                                            right-tree)
                                 remaining-elts)))))))))
    (car (partial-tree elements (length elements)))))

;; Solution in O(n)
(defun in-order (tree)
  (let ((stack nil))
    (labels ((rec (curr)
               (unless (null curr)
                 (let ((entry (entry curr))
                       (lbranch (left-branch curr))
                       (rbranch (right-branch curr)))
                   (rec rbranch)
                   (setf stack (cons entry stack))
                   (rec lbranch)))))
      (rec tree)
      stack)))

(defun union-bset (bset1 bset2)
  (list->tree (union-lset (in-order bset1)
                          (in-order bset2))))

(defun inter-bset (bset1 bset2)
  (list->tree (inter-lset (in-order bset1)
                          (in-order bset2))))