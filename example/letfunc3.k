;; Inner a should refer outer a

(define func
  (lambda (a)
    (let ((b))
      ((lambda () (set! b a)))
      b)))

(print (func "Hello, World!")) ;; => should be "Hello, World!"
