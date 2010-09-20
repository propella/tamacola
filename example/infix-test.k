;;;; Infix grammar test

(check (parse-collection infix/make-left '(1 ())) => 1)
(check (parse-collection infix/make-left '(1 ((+ 2)))) => '(+ 1 2))
(check (parse-collection infix/make-left '(1 ((+ 2) (- 3)))) => '(- (+ 1 2) 3))

(check (parse-collection infix/digit "1") => 49)
(check (parse-collection infix/number "123") => 123)
(check (parse-collection infix/sum "1") => 1)
(check (parse-collection infix/sum "1 + 2") => '(+ 1 2))
(check (parse-collection infix/expr "1 + 2 * 3") => '(+ 1 (* 2 3)))

(trace "(3 + 4) * 6 =?")
(trace (eval (parse-collection infix/expr "(3 + 4) * 6")))
