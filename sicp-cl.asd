;;;; sicp-cl.asd

(asdf:defsystem #:sicp-cl
  :description "Common Lisp solutions to exercises in SICP"
  :serial t
  :components ((:file "package")
               (:file "exercises-ch1" :depends-on ("package"))))
