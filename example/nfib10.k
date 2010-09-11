(define nfibs (lambda (n) (if (< n 2) 1 (+ 1 (+ (nfibs (- n 1)) (nfibs (- n 2)))))))

(define main
  (lambda (dummy)
    (let ((n 0))
      (while (< n 10)
	(print (nfibs 32))
	(set! n (+ 1 n))))
    0))

(main 0)
