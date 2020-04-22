;;;; sicp-cl.asd

(asdf:defsystem #:sicp-cl
  :description "Common Lisp solutions to exercises in SICP"
  :serial t
  :components ((:file "package")
               (:file "generics" :depends-on ("package"))
               (:file "chapter2" :depends-on ("generics"))
               (:file "exercises-ch1" :depends-on ("package"))))
