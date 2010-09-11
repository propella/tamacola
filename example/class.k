;; Create object and access test

(class Foo (Object) (foo)
       (lambda () (slot-setq! #self foo "Hello,")))
(class Bar (Foo) (bar)
       (lambda (str)
         (constructsuper)
         (slot-setq! #self bar str)))

(let ((obj (new Bar "World!")))
  (print (slot-getq obj foo) (slot-getq obj bar)))
