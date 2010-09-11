(define nfibs (lambda (n) (if (< n 2) 1 (+ 1 (+ (nfibs (- n 1)) (nfibs (- n 2)))))))
(print (nfibs 32))

