(defun for-each (fn ls)
  (if (null ls)
      nil
      (+map fn (cdr ls))))
