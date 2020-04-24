;; Can use either representation & set of selectors
;; Option 1
(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
    (list length structure))

(defun left-branch (mobile) (car mobile))
(defun right-branch (mobile) (cadr mobile))
(defun branch-length (branch) (car branch))
(defun branch-item (branch) (cadr branch))

;; Option 2 (part d)
(defun make-mobile (left right)
  (cons left right))

(defun make-branch (length structure)
  (cons length structure))

;; Only need to modify the cdr parts
(defun right-branch (mobile) (cdr mobile))
(defun branch-item (branch) (cdr branch))

;; To avoid redundant calls to weight in balanced [which would add up quickly]
;; I define a function which returns related information together
(labels ((wb (mobile)
           (if (numberp mobile)
               (list mobile t)
               (let* ((lbranch (left-branch mobile))
                      (rbranch (right-branch mobile))
                      (left-wb (wb (branch-item lbranch)))
                      (right-wb (wb (branch-item rbranch)))
                      (lweight (first left-wb)) (lbal (second left-wb))
                      (rweight (first right-wb)) (rbal (second right-wb))
                      (llen (branch-length lbranch)) (rlen (branch-length rbranch)))
                 (list (+ lweight rweight) (and lbal rbal (= (* lweight llen)
                                                             (* rweight rlen))))))))
  (defun weight (mobile) (first (wb mobile)))
  (defun balanced (mobile) (second (wb mobile))))
