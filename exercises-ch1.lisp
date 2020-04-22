(in-package :sicp-cl)

(defun exercise-1 ()
  '(10 12 8 3 -16 3 4 19 nil 3 16 6 16))

(defun exercise-2 ()
  '(/
    (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
    (* 3 ( - 6 2) (- 2 7))))

(defun exercise-3 (x y z)
  (labels ((sqsum (n1 n2) (+ (* n1 n1) (* n2 n2))))
    (let ((min (min x y z)))
      (cond
        ((= min x) (sqsum y z))
        ((= min y) (sqsum x z))
        ((= min z) (sqsum x y))))))

(defun exercise-4 ()
  "It adds a to (absolute value of b) god why do I do this to myself")

(defun exercise-5 ()
  ;; https://sookocheff.com/post/fp/evaluating-lambda-expressions/
  "Applicative order would result in an infinite loop, normal order would return 0")


;; Section 2

(defun exercise-6 ()
  "Infinite loop: both branches are always taken, and so it continues trying
to improve guess forever")

(labels ((improve (guess x)
           (/ (+ guess (/ x guess)) 2))
         (square (x)
           (* x x))
         (good-enough? (guess x)
           (< (abs ( - (square guess) x)) 0.001))
         (sqrt-iter (guess x)
           (if (good-enough? guess x)
               guess
               (sqrt-iter (improve guess x) x)))
         (>sqrt (x) (sqrt-iter 1.0 x)))

  (defun exercise-7 ()
    (format t "~a ~a ~a~%"
            "Showing that it fails for small numbers is easy! For instance, sqrt(1e-8) = 1-e4 but"
            (my-sqrt .00000001) "isn't really close")
    ;; The following code illustrates the example: if your guess is (sqrt (- big-number .01))
    ;; and you're trying to find (sqrt big-number), you'll have issues
    (let ((big-number (expt 10 10))) (- big-number (square (sqrt (- big-number .01)))))
    ;; Not gonna do this last part
    ))

(defun exercise-9 ()
  (format t "The first is a recursive process while the second is an iterative one"))

(defun ackermann (x y) (cond ((= y 0) 0)
                             ((= x 0) (* 2 y))
                             ((= y 1) 2)
                             (t (ackermann (1- x) (ackermann x (1- y))))))

(defun exercise-10 ()
  ;; DONE
  (format t "(A 1 10): ~a~%" (ackermann 1 10))
  (format t "(A 2 4): ~a~%" (ackermann 2 4))
  (format t "(A 3 3): ~a~%" (ackermann 3 3))
  (format t "(A 0 n) = 2y~%")
  (format t "(A 1 n) = 2^n")
  (format t "(A 2 n) = (2 ^ 2 ^ ... ^ 2) where the stack in n long"))

(defun exercise-16 (base pow &optional (accumulator 1))
  ;; TODO Haven't done this :9
  )
