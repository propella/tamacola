;; Runtime Macro test

(let ((pattern (macro-at 'list)))
  (check (pattern-apply pattern '(name arg1 arg2 arg3)) => '(cons arg1 (list arg2 arg3))))

(check (list) => ())
(check (list 1) => (cons 1 ()))
(check (list 1 2) => (cons 1 (cons 2 ())))
(check (list 1 2 3) => (cons 1 (cons 2 (cons 3 ()))))

(check (quote 1) => 1)
(check (quote a) => (intern "a"))
(check (quote (a (1 2 3))) => (list (intern "a") (list 1 2 3)))

(check (let ((a 3)) `(1 2 ,a)) => (list 1 2 3))

(define-form after (a b) b)

(let ((f (macro-at 'after)))
  (check (f '(1 2)) => 2))

