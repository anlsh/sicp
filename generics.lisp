;; A dummy system for generic functions!

(in-package :sicp-cl)

(let ((type-table {})
      (generic-defs {}))

  ;; Internal functions related to typing
  (defun construct (type val)
    (unless (gethash type type-table) (error (format nil "Nonexistent type ~a" type)))
    (cons type val))

  (defun define-type (tname)
    (sethash (tname t) type-table))

  (labels ((get-type (object) (car object))
           (get-val (object) (cdr object))
           (apply-generic (name args)
             (let ((types (mapcar #'get-type args))
                   (vals (mapcar #'get-val args)))
               (multiple-value-bind (fn exists) (gethash types (gethash name generic-defs))
                 (unless exists
                   (error (format nil "~a has no definition for ~a~%" name types)))
                 (apply fn vals)))))

    (defmacro define-generic (name argspecs &body body)
      (put-if-absent (name {}) generic-defs)
      (destructuring-bind
          (arg-types arg-names)
          (transpose argspecs)
        ;; Make sure that all the types actually exist!
        (mapcar (lambda (type)
                  (unless (gethash type type-table)
                    (error (format nil "Nonexistent type ~a" type))))
                arg-types)
        (sethash (arg-types (eval `(lambda ,arg-names ,@body))) (gethash name generic-defs))
        ;; Enable calling generics as you would other functions
        (setf (symbol-function name) (lambda (&rest args) (apply-generic name args)))))))
