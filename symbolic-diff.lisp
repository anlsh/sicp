;; A small and pretty dumb symbolic arithmetic system.

(defvar *symbolic-fun-table* (make-hash-table))
(setf (gethash '+ *symbolic-fun-table*) #'+)
(setf (gethash '* *symbolic-fun-table*) #'*)
(setf (gethash '- *symbolic-fun-table*) #'-)
(setf (gethash '/ *symbolic-fun-table*) #'/)
(setf (gethash 'pow *symbolic-fun-table*) #'expt)

(defvar *derivations* (make-hash-table))
(setf (gethash '+ *derivations*)
      (lambda (var fst snd)
        `(+ ,(derive-exp fst var) ,(derive-exp snd var))))
(setf (gethash '* *derivations*)
      (lambda (var fst snd) `(+ (* ,fst ,(derive-exp snd var))
                                (* ,snd ,(derive-exp fst var)))))

(defun eval-exp (exp &optional vars)
  (cond ((numberp exp) exp)
        ((multiple-value-bind (_ is-in) (gethash exp vars) is-in) (gethash exp vars))
        (t (destructuring-bind (fn . args) exp
             (apply (gethash fn *symbolic-fun-table*)
                    (map 'list (lambda (exp) (eval-exp exp vars)) args))))))

(defun derive-exp (exp var)
  (cond ((numberp exp) 0)
        ((symbolp exp) (if (equalp exp var) 1 0))
        (t (destructuring-bind (fn . args) exp
             (apply (gethash fn *derivations*) `(,var . ,args))))))
