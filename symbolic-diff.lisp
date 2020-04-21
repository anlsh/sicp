;; A small and pretty dumb symbolic arithmetic system.

(let (fns diff-fns)
  (setf fns { ('+ #'+) ('+ #'+) ('- #'-) ('/ #'/) ('pow #'expt) })
  (setf diff-fns {
        ('+ (lambda (var fst snd)
              `(+ ,(derive-exp fst var) ,(derive-exp snd var))))
        ('* (lambda (var fst snd) `(+ (* ,fst ,(derive-exp snd var))
                                      (* ,snd ,(derive-exp fst var)))))
        ('- (lambda (var fst snd) `(- ,(derive-exp fst var) ,(derive-exp snd var)) ))
        })

  (defun eval-exp (exp &optional (vars {}))
    (cond ((numberp exp) exp)
          ((multiple-value-bind (_ is-in) (gethash exp vars)
             (declare (ignore _)) is-in)
           (gethash exp vars))
          (t (destructuring-bind (fn . args) exp
               (apply (gethash fn fns)
                      (map 'list (lambda (exp) (eval-exp exp vars)) args))))))

  (defun derive-exp (exp var)
    (cond ((numberp exp) 0)
          ((symbolp exp) (if (equalp exp var) 1 0))
          (t (destructuring-bind (fn . args) exp
               (apply (gethash fn diff-fns) `(,var . ,args)))))))
