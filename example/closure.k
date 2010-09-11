(define hello
  (lambda (free)
    (set! free "Free variable: Good")
    (lambda ()
      (print free))))

(let ((func (hello "*** Free variable: BAD ***")))
  (func))
