;;;; Infix grammar test

;; Make a left associative expression from a sequence of command like
;; 1 and ((+ 2) (- 3) ..) to (- (+ 1 2) 3)
(define make-left
  (lambda (first rest)
    (if rest
	(let ((op     (car  (car rest)))
	      (second (cadr (car rest))))
	  (make-left (list op first second) (cdr rest)))
	first)))

(check (parse-collection idigit "1") => 49)
(check (parse-collection inumber "123") => 123)
(check (parse-collection isum "1") => 1)
(check (parse-collection isum "1 + 2") => '(+ 1 2))
(check (parse-collection iexpr "1 + 2 * 3") => '(+ 1 (* 2 3)))

(trace "(3 + 4) * 6 =?")
(trace (eval (parse-collection iexpr "(3 + 4) * 6")))
