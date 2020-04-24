;; Compute all subsets
(defun subsets (s)
  (if (null s)
      (list nil)
      (let ((head (car s))
            (cdr-subsets (subsets (cdr s))))
        ;; Quite a nice formulation actually :D
        ;; Appends (Unions) the subsets of the cdr of the lists [subsets not including head item]
        ;; with the list of the save prepended with head [subsets including the head item]
        (append cdr-subsets
                (mapcar (lambda (subsets) (cons head subsets)) cdr-subsets)))))
