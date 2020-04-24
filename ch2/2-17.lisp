(defun last-pair (ls)
  (if (cdr ls) (last-pair (cdr ls)) ls))
