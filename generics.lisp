;; A dummy system for generic functions!

(in-package :sicp-cl)

(let ((type-table {})
      type-hierarchy
      (generic-defs {}))

  (declare (ignore type-hierarchy))

  (defun get-type-table () type-table)
  (defun get-generic-defs () generic-defs)

  (defun make-type (tname)
    (multiple-value-bind (_ exists) (gethash tname type-table)
      (declare (ignore _))
      (if exists (error "Type redefinition") (sethash (tname t) type-table))))

  (defmacro define-generic (name argspecs &body body)
    (destructuring-bind
        (arg-names arg-types)
        (volt:transpose argspecs)
      ;; Make sure that all the types actually exist!
      (mapcar (lambda (type)
                (unless (second (multiple-value-list (gethash type type-table)))
                  (error "Nonexistent type")))
              arg-types)
      ;; If it's a new generic function, add an entry to the table
      (put-if-absent (name {}) generic-defs)
      ;; Add an entry for the generic function with the given arguments
      (sethash (arg-types `(lambda ,arg-names ,@body)) (gethash name generic-defs)))))
