;; A dummy system for generic functions!

(in-package :sicp-cl)

(let ((type-table {})
      type-hierarchy
      (generic-defs {}))

  (declare (ignore type-hierarchy))

  ;; Internal functions related to typing
  (defun construct (type val)
    (unless (gethash type type-table) (error "Attempting to construct undeclared type"))
    (cons type val))
  (defun get-type (object)
    (car object))
  (defun get-val (object)
    (cdr object))

  (defun define-type (tname)
    (multiple-value-bind (_ exists) (gethash tname type-table)
      (declare (ignore _))
      (if exists (error "Type redefinition") (sethash (tname t) type-table))))

  (defmacro define-generic (name argspecs &body body)
    (destructuring-bind
        (arg-names arg-types)
        (volt:transpose argspecs)
      ;; Make sure that all the types actually exist!
      (mapcar (lambda (type)
                (unless (gethash type type-table)
                  (error "Nonexistent type")))
              arg-types)
      (put-if-absent (name {}) generic-defs)
      (sethash arg-types (eval `(lambda ,arg-names ,@body)) (gethash name generic-defs))))

  (defun apply-generic (name &rest args)
    (let ((types (mapcar #'get-type args))
          (vals (mapcar #'get-val args)))
      (apply (gethash types (gethash name generic-defs)) vals))))
