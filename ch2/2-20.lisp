(defun same-parity (root &rest others)
  (let ((root-parity (evenp root)))
    (remove-if-not (lambda (n) (equal root-parity (evenp n))) others)))
