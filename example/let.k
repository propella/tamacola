(define func
  (lambda (a)
    (let ((dummy) (x "earth") (y "world") (z "westwood"))
      (print a)
      (print y))))

(func "hello")

(let () (print "Let without variable even works."))
