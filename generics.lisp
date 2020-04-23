;; A dummy system for generic functions!

(in-package :sicp-cl)

(let ((type-table {})
      (generic-defs {}))

  (defun define-type (tname)
    (sethash (tname t) type-table))

  (defun construct (type val)
    (unless (gethash type type-table) (error (format nil "Nonexistent type ~a" type)))
    (cons type val))

  (labels ((get-type (object) (car object))
           (get-val (object) (cdr object))
           (apply-generic (name args)
             (let ((types (mapcar #'get-type args))
                   (vals (mapcar #'get-val args)))
               (multiple-value-bind (fn-table exists) (gethash name generic-defs)
                 (unless exists (error (format nil "~a has no specializations" name)))
                 (multiple-value-bind (fn exists) (gethash types fn-table)
                   (unless exists
                     (error (format nil "~a has no definition for ~a" name types)))
                   (apply fn vals))))))

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

        ;; Allows generic functions to access lexical scope
        `(progn (setf (symbol-function ',name) ,(lambda (&rest args) (apply-generic name args)))
                (setf (gethash ',arg-types (gethash ',name ,generic-defs))
                      (lambda ,arg-names ,@body)))))))
